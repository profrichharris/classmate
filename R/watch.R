# ---------------------------------------------------------------------------
# watch() / raisehand() / endclass() / reset_key()
#
# A lightweight background watcher that captures R errors and recent command
# history, then explains them in plain English via Claude when the student
# calls raisehand().  No Shiny UI — works entirely in the R console.
#
# Key sharing: uses the same active_config.rds and usage_log.rds as ask(),
# so a key loaded in either place is immediately available in the other.
# ---------------------------------------------------------------------------

.watch_env <- new.env(parent = emptyenv())
.watch_env$active         <- FALSE
.watch_env$api_key        <- NULL
.watch_env$last_error     <- NULL
.watch_env$history_buffer <- list()
.watch_env$callback_id    <- NULL
.watch_env$original_error <- NULL

.WATCH_BUFFER_SIZE <- 30L
.WATCH_MODEL       <- "claude-sonnet-4-6"

# Input token price per million (Sonnet 4.6)
.WATCH_PRICE_IN  <- 3.00 / 1e6
.WATCH_PRICE_OUT <- 15.00 / 1e6

# ---------------------------------------------------------------------------
# Shared path helpers (identical to those in app.R so files are the same)
# ---------------------------------------------------------------------------

.watch_cfg_path <- function() {
  d <- tools::R_user_dir("classmate", "config")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.path(d, "active_config.rds")
}

.watch_usage_path <- function() {
  d <- tools::R_user_dir("classmate", "data")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.path(d, "usage_log.rds")
}

.watch_load_cfg <- function() {
  p <- .watch_cfg_path()
  if (!file.exists(p)) return(NULL)
  tryCatch(readRDS(p), error = function(e) NULL)
}

.watch_read_usage <- function() {
  p <- .watch_usage_path()
  if (!file.exists(p)) return(list())
  tryCatch(readRDS(p), error = function(e) list())
}

.watch_append_usage <- function(cost_usd) {
  log <- .watch_read_usage()
  log <- c(log, list(list(timestamp = Sys.time(), cost_usd = cost_usd)))
  tryCatch(saveRDS(log, .watch_usage_path()), error = function(e) NULL)
  invisible(log)
}

# ---------------------------------------------------------------------------
# Quota helpers
# ---------------------------------------------------------------------------

.watch_period_cutoff <- function(reset_period) {
  now <- Sys.time()
  if (reset_period == "rolling_24h") return(now - 86400)
  if (reset_period == "hourly")      return(trunc(now, "hours"))
  if (reset_period == "daily")       return(as.POSIXct(as.Date(now)))
  if (reset_period == "weekly") {
    d   <- as.Date(now)
    dow <- as.integer(format(d, "%u"))  # 1=Mon … 7=Sun
    return(as.POSIXct(d - (dow - 1L)))
  }
  m <- regmatches(reset_period, regexpr("[0-9]+", reset_period))
  unit <- sub("^rolling_[0-9]+", "", reset_period)
  secs <- if (unit == "h") as.integer(m) * 3600L else as.integer(m) * 60L
  now - secs
}

# Returns a named list: used_usd, limit_usd, reset_label, over_limit, expired
.watch_quota <- function(cfg) {
  if (is.null(cfg)) return(NULL)

  expired <- !is.null(cfg$final_expiry) && Sys.Date() > as.Date(cfg$final_expiry)

  cutoff <- tryCatch(.watch_period_cutoff(cfg$reset_period), error = function(e) NULL)
  log    <- .watch_read_usage()
  used   <- if (!is.null(cutoff)) {
    sum(vapply(log, function(e) {
      if (e$timestamp >= cutoff) e$cost_usd else 0
    }, numeric(1)))
  } else 0

  limit <- cfg$cost_limit
  over  <- !is.null(limit) && used >= limit

  reset_label <- switch(cfg$reset_period,
    weekly     = paste("Monday", format(Sys.Date() + (8L - as.integer(format(Sys.Date(), "%u"))), "%d %b")),
    daily      = "midnight",
    hourly     = "next hour",
    rolling_24h = "24h from first use",
    paste("after", cfg$reset_period)
  )

  list(used_usd    = used,
       limit_usd   = limit,
       reset_label = reset_label,
       over_limit  = over,
       expired     = expired)
}

# ---------------------------------------------------------------------------
# Schema helper (mirrors schema_from_r_object in app.R)
# ---------------------------------------------------------------------------

.watch_schema <- function(obj) {
  if (inherits(obj, "sf")) {
    geom_col  <- attr(obj, "sf_column")
    cols      <- setdiff(names(obj), geom_col)
    geom_type <- tryCatch(
      paste(unique(as.character(sf::st_geometry_type(obj))), collapse = "/"),
      error = function(e) "spatial"
    )
    list(type = paste("sf", geom_type), cols = cols, nrow = nrow(obj))
  } else if (is.data.frame(obj)) {
    list(type = "data frame", cols = names(obj), nrow = nrow(obj))
  } else if (is.matrix(obj)) {
    list(type = "matrix", cols = colnames(obj), nrow = nrow(obj))
  } else if (is.list(obj)) {
    list(type = "list", cols = names(obj))
  } else {
    list(type = paste(class(obj), collapse = "/"), cols = NULL)
  }
}

.watch_format_workspace <- function(ws) {
  if (length(ws) == 0) return("(workspace is empty)")
  lines <- vapply(names(ws), function(nm) {
    s <- ws[[nm]]
    if (is.null(s)) return(paste0("  ", nm, ": (unreadable)"))
    if (!is.null(s$cols) && length(s$cols) > 0) {
      col_str <- if (length(s$cols) > 25)
        paste0(paste(head(s$cols, 25), collapse = ", "),
               " ... [", length(s$cols), " total]")
      else paste(s$cols, collapse = ", ")
      nr <- if (!is.null(s$nrow)) paste0(", ", s$nrow, " rows") else ""
      paste0("  ", nm, " [", s$type, nr, "]: ", col_str)
    } else {
      paste0("  ", nm, " [", s$type, "]")
    }
  }, character(1))
  paste(lines, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Claude API call
# ---------------------------------------------------------------------------

.watch_call_claude <- function(user_prompt, api_key) {
  system_prompt <- paste(
    "You are a friendly teaching assistant helping students who are learning R.",
    "You will receive: (1) a numbered log of the student's recent R commands,",
    "with [ERROR] marking the command that failed; (2) the error message;",
    "(3) a summary of their current R workspace showing object names, types,",
    "dimensions, and column names where available.",
    "",
    "Your job is to explain in plain English what went wrong and how to fix it.",
    "Important: the error may have been caused by an earlier command, not the",
    "one that produced the error message — look at the full history and workspace",
    "to find the real root cause.",
    "",
    "Guidelines:",
    "- Be concise, friendly, and encouraging.",
    "- Avoid jargon; explain any technical terms you use.",
    "- Focus on the most likely cause first.",
    "- If the fix is a simple name or spelling correction, show it.",
    "- Do not write full corrected code blocks unless the fix is trivial.",
    "- If you spot a typo in a column name or object name, call it out clearly."
  )

  result <- tryCatch({
    resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      httr2::req_body_json(list(
        model      = .WATCH_MODEL,
        max_tokens = 1024L,
        system     = system_prompt,
        messages   = list(list(role = "user", content = user_prompt))
      )) |>
      httr2::req_timeout(30L) |>
      httr2::req_perform()

    body    <- httr2::resp_body_json(resp)
    text    <- body$content[[1]]$text
    usage   <- body$usage
    cost    <- (usage$input_tokens  %||% 0L) * .WATCH_PRICE_IN +
               (usage$output_tokens %||% 0L) * .WATCH_PRICE_OUT
    list(text = text, cost = cost)
  }, error = function(e) {
    list(text = paste("Could not reach the AI:", conditionMessage(e)), cost = 0)
  })
  result
}

# ---------------------------------------------------------------------------
# Interactive key prompt (called when no saved key exists)
# ---------------------------------------------------------------------------

.watch_prompt_for_key <- function() {
  message("")
  message("No classmate key found. Do you have:")
  message("  1. A key file (.key) provided by your instructor")
  message("  2. Your own Anthropic API key")
  message("")
  choice <- trimws(readline("Enter 1 or 2 (or press Enter to cancel): "))

  if (choice == "1") {
    message("A file browser will open — navigate to your .key file and select it.")
    path <- tryCatch(file.choose(), error = function(e) NULL)
    if (is.null(path) || !nzchar(path)) {
      message("No file selected.")
      return(NULL)
    }
    payload <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(payload) || !nzchar(payload$api_key %||% "")) {
      message("That file does not appear to be a valid classmate key.")
      return(NULL)
    }
    tryCatch(saveRDS(payload, .watch_cfg_path()), error = function(e) NULL)
    message("Key loaded and saved for future sessions.")
    return(payload$api_key)

  } else if (choice == "2") {
    key <- trimws(readline("Paste your Anthropic API key: "))
    if (!nzchar(key)) {
      message("No key entered.")
      return(NULL)
    }
    if (!grepl("^sk-ant-", key)) {
      message("That does not look like an Anthropic API key (should start with 'sk-ant-').")
      return(NULL)
    }
    Sys.setenv(ANTHROPIC_API_KEY = key)
    return(key)

  } else {
    message("Cancelled.")
    return(NULL)
  }
}

# ---------------------------------------------------------------------------
# watch()
# ---------------------------------------------------------------------------

#' Start watching for R errors
#'
#' Runs silently in the background. After any error, type \code{raisehand()}
#' for a plain-English explanation. Use \code{endclass()} to stop watching.
#'
#' @param key Path to a \code{.key} file supplied by your instructor, OR a
#'   raw Anthropic API key string (\code{"sk-ant-..."}). If a key was already
#'   loaded (in this session, a previous session, or via \code{ask()}), you can
#'   omit this argument.
#' @return Invisible NULL (called for side effects).
#' @export
watch <- function(key = NULL) {

  if (.watch_env$active) {
    message("classmate is already watching. Use endclass() to stop.")
    return(invisible(NULL))
  }

  # --- Resolve API key -------------------------------------------------------
  api_key <- NULL

  # 1. Explicit argument — could be a .key file path or a raw API key string
  if (!is.null(key) && nzchar(key)) {
    if (file.exists(key)) {
      payload <- tryCatch(readRDS(key), error = function(e) NULL)
      if (!is.null(payload) && nzchar(payload$api_key %||% "")) {
        tryCatch(saveRDS(payload, .watch_cfg_path()), error = function(e) NULL)
        api_key <- payload$api_key
        message("Key file loaded and saved for future sessions.")
      } else {
        message("Could not read key from file: ", key)
        return(invisible(NULL))
      }
    } else if (grepl("^sk-ant-", key)) {
      Sys.setenv(ANTHROPIC_API_KEY = key)
      api_key <- key
    } else {
      message("key must be a path to a .key file or an API key starting with 'sk-ant-'.")
      return(invisible(NULL))
    }
  }

  # 2. Saved student key from active_config.rds (written by ask() or prior watch())
  if (is.null(api_key)) {
    cfg <- .watch_load_cfg()
    if (!is.null(cfg) && nzchar(cfg$api_key %||% ""))
      api_key <- cfg$api_key
  }

  # 3. Environment variable (set by ask() for personal/non-student use)
  if (is.null(api_key)) {
    ev <- Sys.getenv("ANTHROPIC_API_KEY")
    if (nzchar(ev)) api_key <- ev
  }

  if (is.null(api_key)) {
    api_key <- .watch_prompt_for_key()
    if (is.null(api_key)) return(invisible(NULL))
  }

  # --- Check expiry / quota --------------------------------------------------
  cfg   <- .watch_load_cfg()
  quota <- .watch_quota(cfg)
  if (!is.null(quota)) {
    if (quota$expired) {
      message("Your classmate key has expired. Please ask your instructor for a new one.")
      return(invisible(NULL))
    }
    if (quota$over_limit) {
      message(sprintf(
        "You have reached your usage limit ($%.2f). It resets %s.",
        quota$limit_usd, quota$reset_label
      ))
      return(invisible(NULL))
    }
  }

  # --- Install state ---------------------------------------------------------
  .watch_env$api_key        <- api_key
  .watch_env$history_buffer <- list()
  .watch_env$last_error     <- NULL
  .watch_env$original_error <- getOption("error")

  # Hook 1: rolling command history
  cb_id <- addTaskCallback(function(expr, value, ok, visible) {
    cmd <- tryCatch(paste(deparse(expr), collapse = "\n"), error = function(e) "?")
    entry <- list(cmd = cmd, ok = ok, time = Sys.time())
    buf <- c(.watch_env$history_buffer, list(entry))
    if (length(buf) > .WATCH_BUFFER_SIZE)
      buf <- buf[(length(buf) - .WATCH_BUFFER_SIZE + 1L):length(buf)]
    .watch_env$history_buffer <- buf
    TRUE
  }, name = "classmate_watcher")

  .watch_env$callback_id <- cb_id

  # Hook 2: error capture (normal error message prints first, then this runs)
  options(error = function() {
    err_msg <- geterrmessage()

    tb <- tryCatch({
      calls <- sys.calls()
      vapply(calls, function(x)
        tryCatch(paste(deparse(x), collapse = " "), error = function(e) "?"),
        character(1))
    }, error = function(e) character(0))

    ws_names <- tryCatch(ls(envir = .GlobalEnv), error = function(e) character(0))
    ws_snap  <- lapply(setNames(ws_names, ws_names), function(nm) {
      tryCatch(.watch_schema(get(nm, envir = .GlobalEnv)), error = function(e) NULL)
    })

    .watch_env$last_error <- list(
      message   = err_msg,
      traceback = tb,
      history   = .watch_env$history_buffer,
      workspace = ws_snap,
      time      = Sys.time()
    )

    message("  [classmate] Type raisehand() for help with this error.")
  })

  .watch_env$active <- TRUE
  message("classmate is watching. Type raisehand() when you get stuck, endclass() to stop.")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# raisehand()
# ---------------------------------------------------------------------------

#' Ask for help with the last R error
#'
#' Sends the error message, recent command history, and workspace summary to
#' Claude and prints a plain-English explanation. Also shows your remaining
#' quota if a student key is in use.
#'
#' Can also be called without a prior error just to check quota remaining.
#'
#' @return Invisible NULL (called for side effects).
#' @export
raisehand <- function() {

  if (!.watch_env$active) {
    message("classmate is not watching. Start with watch().")
    return(invisible(NULL))
  }

  # --- Quota check first -----------------------------------------------------
  cfg   <- .watch_load_cfg()
  quota <- .watch_quota(cfg)
  if (!is.null(quota)) {
    if (quota$expired) {
      message("Your classmate key has expired. Please ask your instructor for a new one.")
      return(invisible(NULL))
    }
    if (quota$over_limit) {
      message(sprintf(
        "You have reached your usage limit ($%.2f). It resets %s.",
        quota$limit_usd, quota$reset_label
      ))
      return(invisible(NULL))
    }
  }

  # --- No error yet ----------------------------------------------------------
  e <- .watch_env$last_error
  if (is.null(e)) {
    msg <- "No error has been captured yet."
    if (!is.null(quota)) {
      msg <- paste0(msg, sprintf(
        "\nUsage so far: $%.4f of $%.2f (resets %s).",
        quota$used_usd, quota$limit_usd %||% 0, quota$reset_label
      ))
    }
    message(msg)
    return(invisible(NULL))
  }

  # --- Build prompt ----------------------------------------------------------
  history_lines <- vapply(seq_along(e$history), function(i) {
    h   <- e$history[[i]]
    flag <- if (!h$ok) " [ERROR]" else ""
    paste0(i, ". ", h$cmd, flag)
  }, character(1))

  tb_lines <- if (length(e$traceback) > 0)
    paste(tail(e$traceback, 6L), collapse = "\n")
  else
    "(no traceback available)"

  prompt <- paste0(
    "Recent commands (oldest to newest):\n",
    paste(history_lines, collapse = "\n"),
    "\n\nError message:\n",
    trimws(e$message),
    "\n\nCall stack at time of error:\n",
    tb_lines,
    "\n\nWorkspace at time of error:\n",
    .watch_format_workspace(e$workspace)
  )

  # --- Call Claude -----------------------------------------------------------
  message("classmate is thinking...")
  result <- .watch_call_claude(prompt, .watch_env$api_key)

  # --- Display response ------------------------------------------------------
  sep <- strrep("─", 60)
  cat("\n", sep, "\n", sep = "")
  cat(result$text, "\n")
  cat(sep, "\n\n", sep = "")

  # --- Log usage and show quota ----------------------------------------------
  if (result$cost > 0) .watch_append_usage(result$cost)

  if (!is.null(cfg)) {
    quota2 <- .watch_quota(cfg)  # re-read after appending
    if (!is.null(quota2)) {
      if (!is.null(quota2$limit_usd)) {
        remaining <- max(0, quota2$limit_usd - quota2$used_usd)
        cat(sprintf(
          "[classmate] $%.4f used this period · $%.4f remaining · resets %s\n\n",
          quota2$used_usd, remaining, quota2$reset_label
        ))
      }
    }
  }

  invisible(NULL)
}

# ---------------------------------------------------------------------------
# endclass()
# ---------------------------------------------------------------------------

#' Stop watching for R errors
#'
#' Removes the error hook and command history callback installed by
#' \code{watch()}.
#'
#' @return Invisible NULL (called for side effects).
#' @export
endclass <- function() {
  if (!.watch_env$active) {
    message("classmate is not currently watching.")
    return(invisible(NULL))
  }
  options(error = .watch_env$original_error)
  tryCatch(removeTaskCallback(.watch_env$callback_id), error = function(e) NULL)
  .watch_env$active         <- FALSE
  .watch_env$api_key        <- NULL
  .watch_env$last_error     <- NULL
  .watch_env$history_buffer <- list()
  .watch_env$callback_id    <- NULL
  message("classmate has stopped watching.")
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# reset_key()
# ---------------------------------------------------------------------------

#' Clear the saved classmate key
#'
#' Removes the key file saved by \code{watch()} or \code{ask()} and unsets
#' the \code{ANTHROPIC_API_KEY} environment variable. The next call to
#' \code{watch()} or \code{ask()} will prompt for a new key.
#'
#' @return Invisible NULL (called for side effects).
#' @export
reset_key <- function() {
  removed <- FALSE

  cfg_path <- .watch_cfg_path()
  if (file.exists(cfg_path)) {
    file.remove(cfg_path)
    message("Saved key file removed.")
    removed <- TRUE
  }

  if (nzchar(Sys.getenv("ANTHROPIC_API_KEY"))) {
    Sys.unsetenv("ANTHROPIC_API_KEY")
    message("ANTHROPIC_API_KEY cleared from environment.")
    removed <- TRUE
  }

  if (!removed) message("No saved key found.")

  if (.watch_env$active)
    message("Note: classmate is still watching with the previous key until endclass() is called.")

  invisible(NULL)
}
