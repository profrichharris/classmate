#' Create a classmate key file for distribution to students
#'
#' Bundles an Anthropic API key together with per-student usage-limit settings
#' into a single \code{.key} file that can be emailed to students. Students
#' load the file via the \strong{Load key file} button in the app and never see
#' the raw API key.
#'
#' Any argument left as \code{NULL} is requested interactively at the console.
#'
#' @param api_key Anthropic API key (string starting with \code{"sk-ant-"}).
#'   If omitted, you are prompted to paste it.
#' @param cost_limit Maximum spend in USD per reset period. Default \code{0.30}.
#'   Set to \code{NULL} for no limit (not recommended for student distribution).
#'   If omitted, you are prompted to confirm or change the default.
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
#' @param output_file Path for the key file. Default \code{"classmate.key"} in
#'   the current working directory.
#'
#' @return The normalised path to the key file, invisibly.
#' @export
classmate_make_key <- function(api_key      = NULL,
                                cost_limit   = NULL,
                                reset_period = NULL,
                                output_file  = "classmate.key") {

  if (is.null(api_key) || !nzchar(trimws(api_key))) {
    api_key <- trimws(readline("Paste Anthropic API key: "))
    if (!nzchar(api_key)) stop("No API key provided.", call. = FALSE)
  }

  if (is.null(cost_limit)) {
    ans <- trimws(readline("Cost limit in USD per reset period [default: 0.30, or press Enter to accept]: "))
    cost_limit <- if (!nzchar(ans)) 0.30 else {
      v <- suppressWarnings(as.numeric(ans))
      if (is.na(v) || v < 0) {
        message("Invalid value — using default of $0.30.")
        0.30
      } else v
    }
  }

  if (is.null(reset_period)) {
    message("Reset period options: weekly, daily, hourly, rolling_24h, rolling_Nh, rolling_Nm")
    ans <- trimws(readline("Reset period [default: weekly, or press Enter to accept]: "))
    reset_period <- if (!nzchar(ans)) "weekly" else ans
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
        model      = "claude-sonnet-4-6",
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
    api_key      = api_key,
    key_id       = paste0(format(Sys.time(), "%Y%m%d%H%M%S"), "_",
                          sample(10000L:99999L, 1L)),
    cost_limit   = cost_limit,
    reset_period = reset_period,
    created      = Sys.time()
  )
  saveRDS(payload, output_file)

  out <- normalizePath(output_file)
  message("Key file created: ", out)
  if (!is.null(cost_limit))
    message("  Cost limit:   $", cost_limit, " per ", reset_period)
  else
    message("  Cost limit:   none")
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
      cat("  Mode:         student (key file loaded)\n")
      cat("  Cost limit:  ", if (is.null(cfg$cost_limit)) "none"
                             else paste0("$", cfg$cost_limit, " per ", cfg$reset_period), "\n")
      cat("  Key created: ", format(cfg$created, "%Y-%m-%d %H:%M"), "\n")
    }
  } else {
    cat("  Mode:         personal (own API key)\n")
  }
  cat("  Config file: ", cfg_path,   "\n")
  cat("  Usage file:  ", usage_path, "\n")
  invisible(NULL)
}


