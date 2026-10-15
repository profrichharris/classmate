#' Create a classmate key file for distribution to students
#'
#' Bundles an Anthropic API key together with per-student usage-limit settings
#' into a single \code{.key} file that can be emailed to students. Students
#' load the file via the \strong{Load key file} button in the app and never see
#' the raw API key.
#'
#' Any of \code{api_key}, \code{reset_period} and \code{final_expiry} left as
#' \code{NULL}, and \code{cost_limit}, \code{max_conversations} or
#' \code{max_lines} left out entirely, is requested interactively at the
#' console. For those last three, passing \code{NULL} (or typing \code{none})
#' means no limit and requires a Y/N confirmation.
#'
#' @param api_key Anthropic API key (string starting with \code{"sk-ant-"}).
#'   If omitted, you are prompted to paste it.
#' @param cost_limit Maximum spend in USD per reset period. Default \code{0.30}.
#'   If omitted, you are prompted to confirm or change the default. Passing
#'   \code{NULL} (or typing \code{none} at the prompt) creates a key with no
#'   spending limit; you are asked to confirm this with Y/N.
#' @param reset_period Duration of the usage window. Default \code{"weekly"}.
#'   If omitted, you are prompted to confirm or change the default. Options:
#'   \itemize{
#'     \item \code{"weekly"}     — resets at midnight each Monday (default)
#'     \item \code{"rolling_24h"} — rolling 24-hour window
#'     \item \code{"rolling_Nh"} — rolling N hours, e.g. \code{"rolling_6h"}
#'     \item \code{"rolling_Nm"} — rolling N minutes, e.g. \code{"rolling_90m"}
#'     \item \code{"hourly"}     — resets at the top of each clock hour
#'     \item \code{"daily"}      — resets at midnight each day
#'   }
#' @param max_conversations Maximum number of conversations per app start, from
#'   1 to 25. Default \code{3}. If omitted, you are prompted to confirm or
#'   change the default. Passing \code{NULL} (or typing \code{none} at the
#'   prompt) removes the limit; you are asked to confirm this with Y/N.
#' @param max_lines Maximum length, in lines, of code the app will accept or
#'   generate. Default \code{50}. If omitted, you are prompted to confirm or
#'   change the default. Passing \code{NULL} (or typing \code{none} at the
#'   prompt) removes the limit; you are asked to confirm this with Y/N.
#' @param output_file Path for the key file. Default \code{"classmate.key"} in
#'   the current working directory.
#'
#' @return The normalised path to the key file, invisibly.
#' @export
classmate_make_key <- function(api_key           = NULL,
                                cost_limit,
                                reset_period      = NULL,
                                final_expiry      = NULL,
                                max_conversations,
                                max_lines,
                                output_file       = "classmate.key") {

  confirm_unlimited <- function(what) {
    ans <- trimws(readline(paste0(
      "This key will have NO ", what, ". Are you sure? (Y/N): ")))
    if (!tolower(ans) %in% c("y", "yes"))
      stop("Key not created.", call. = FALSE)
    invisible(TRUE)
  }

  if (is.null(api_key) || !nzchar(trimws(api_key))) {
    api_key <- trimws(readline("Paste Anthropic API key: "))
    if (!nzchar(api_key)) stop("No API key provided.", call. = FALSE)
  }

  if (missing(cost_limit)) {
    ans <- trimws(readline(paste0(
      "Cost limit in USD per reset period ",
      "[default: 0.30, 'none' for no limit, or press Enter to accept]: ")))
    cost_limit <- if (!nzchar(ans)) 0.30
    else if (tolower(ans) %in% c("none", "unlimited")) NULL
    else {
      v <- suppressWarnings(as.numeric(ans))
      if (is.na(v) || v < 0) {
        message("Invalid value — using default of $0.30.")
        0.30
      } else v
    }
  }
  if (is.null(cost_limit)) confirm_unlimited("spending limit")

  if (missing(max_lines)) {
    ans <- trimws(readline(paste0(
      "Maximum code length in lines ",
      "[default: 50, 'none' for no limit, or press Enter to accept]: ")))
    max_lines <- if (!nzchar(ans)) 50L
    else if (tolower(ans) %in% c("none", "unlimited")) NULL
    else {
      v <- suppressWarnings(as.integer(ans))
      if (is.na(v) || v < 1L) {
        message("Invalid value — using default of 50 lines.")
        50L
      } else v
    }
  }
  if (is.null(max_lines) || (is.numeric(max_lines) && is.infinite(max_lines))) {
    confirm_unlimited("code-length limit")
    max_lines <- Inf
  } else {
    max_lines <- as.integer(max_lines)
    if (is.na(max_lines) || max_lines < 1L)
      stop("max_lines must be a positive whole number, or NULL for no limit.",
           call. = FALSE)
  }

  if (is.null(reset_period)) {
    message("Reset period options: weekly, daily, hourly, rolling_24h, rolling_Nh, rolling_Nm")
    ans <- trimws(readline("Reset period [default: weekly, or press Enter to accept]: "))
    reset_period <- if (!nzchar(ans)) "weekly" else ans
  }

  if (is.null(final_expiry)) {
    default_expiry <- Sys.Date() + 7L * 15L
    default_str    <- format(default_expiry, "%d/%m/%y")
    ans <- trimws(readline(paste0(
      "Final expiry date DD/MM/YY [default: ", default_str, ", or press Enter to accept]: "
    )))
    final_expiry <- if (!nzchar(ans)) {
      default_expiry
    } else {
      parsed <- tryCatch(as.Date(ans, format = "%d/%m/%y"), error = function(e) NA)
      if (is.na(parsed)) {
        message("Invalid date — using default of ", default_str, ".")
        default_expiry
      } else parsed
    }
  } else if (is.character(final_expiry)) {
    final_expiry <- as.Date(final_expiry, format = "%d/%m/%y")
  }

  if (missing(max_conversations)) {
    ans <- trimws(readline(paste0(
      "Max conversations per app start ",
      "[default: 3, max 25, 'none' for no limit, or press Enter to accept]: ")))
    max_conversations <- if (!nzchar(ans)) 3L
    else if (tolower(ans) %in% c("none", "unlimited", "0")) NULL
    else {
      v <- suppressWarnings(as.integer(ans))
      if (is.na(v) || v < 1L) {
        message("Invalid value — using default of 3.")
        3L
      } else min(v, 25L)
    }
  }
  if (is.null(max_conversations)) {
    confirm_unlimited("limit on conversations")
  } else {
    max_conversations <- min(max(as.integer(max_conversations), 1L), 25L)
  }

  valid_named <- c("rolling_24h", "hourly", "daily", "weekly")
  is_rolling_n <- grepl("^rolling_[0-9]+(h|m)$", reset_period)
  if (!reset_period %in% valid_named && !is_rolling_n)
    stop("Unrecognised reset_period: '", reset_period, "'\n",
         "Use one of: rolling_24h, rolling_Nh, rolling_Nm, hourly, daily, weekly",
         call. = FALSE)

  message("Validating API key ...")
  ok <- tryCatch({
    resp <- httr2::request("https://api.anthropic.com/v1/messages") |>
      httr2::req_headers(
        "x-api-key"         = api_key,
        "anthropic-version" = "2023-06-01",
        "content-type"      = "application/json"
      ) |>
      httr2::req_body_json(list(
        model      = "claude-haiku-5-5",
        thinking   = list(type = "disabled"),
        max_tokens = 1L,
        messages   = list(list(role = "user", content = "Hi"))
      )) |>
      httr2::req_perform()
    httr2::resp_status(resp) == 200L
  }, error = function(e) FALSE)

  if (!ok)
    stop("API key could not be validated. Please check the key and try again.",
         call. = FALSE)

  payload <- list(
    api_key           = api_key,
    key_id            = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                               sample(10000L:99999L, 1L)),
    cost_limit        = cost_limit,
    reset_period      = reset_period,
    final_expiry      = final_expiry,
    max_conversations = max_conversations,
    max_lines         = max_lines,
    created           = Sys.time()
  )
  saveRDS(payload, output_file)

  out <- normalizePath(output_file)
  message("Key file created: ", out)
  if (!is.null(cost_limit))
    message("  Cost limit:   $", cost_limit, " per ", reset_period)
  else
    message("  Cost limit:   none")
  message("  Final expiry: ", format(final_expiry, "%d/%m/%y"))
  if (!is.null(max_conversations))
    message("  Max conversations per start: ", max_conversations)
  else
    message("  Max conversations per start: unlimited")
  message("  Max code length: ",
          if (is.infinite(max_lines)) "unlimited" else paste(max_lines, "lines"))
  message("Email this file to students — they load it via the 'Load key file'",
          " button in the app.")
  invisible(out)
}


#' Show the current classmate configuration
#'
#' Prints the active cost limit and reset period stored in the most recently
#' loaded key file, along with the paths to the config and usage-log files.
#'
#' @return Invisible \code{NULL}.
#' @export
classmate_config_show <- function() {
  cfg_path   <- file.path(tools::R_user_dir("classmate", "config"), "active_config.rds")
  usage_path <- file.path(tools::R_user_dir("classmate", "data"),   "usage_log.rds")

  cat("classmate configuration\n")
  if (file.exists(cfg_path)) {
    cfg <- tryCatch(readRDS(cfg_path), error = function(e) NULL)
    if (!is.null(cfg)) {
      cat("  Key file loaded\n")
      cat("  Cost limit:  ", if (is.null(cfg$cost_limit)) "none"
                             else paste0("$", cfg$cost_limit, " per ", cfg$reset_period), "\n")
      cat("  Code length: ", if (is.null(cfg$max_lines)) "50 lines (key predates this setting)"
                             else if (is.infinite(cfg$max_lines)) "unlimited"
                             else paste(cfg$max_lines, "lines"), "\n")
      cat("  Key created: ", format(cfg$created, "%Y-%m-%d %H:%M"), "\n")
    }
  } else {
    cat("  No key file loaded\n")
  }
  cat("  Config file: ", cfg_path,   "\n")
  cat("  Usage file:  ", usage_path, "\n")
  invisible(NULL)
}


