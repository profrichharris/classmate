`%||%` <- function(x, y) if (is.null(x)) y else x

KLAUSMATE_GITHUB_REPO <- "profrichharris/klausmate"

#' Launch the Klausmate AI tutor Shiny app
#'
#' Opens a Shiny front-end that connects students to Claude AI directly from
#' their R session. Any missing dependencies (shiny, shinyjs, shinyFiles,
#' callr, rstudioapi, httr2) are installed automatically on first use.
#'
#' The app creates an \code{outputs/} folder in your current working directory
#' and saves all generated files there. A session can be paused and resumed:
#' the pause file is also written to your current working directory.
#'
#' @return Invisible NULL (called for its side effect of launching the app).
#' @export
#' @examples
#' \dontrun{
#' talk()
#' }
talk <- function() {

  # --- Stop whisper() if running — avoid conflicts with the app -------------
  if (.watch_env$active) {
    message("Stopping klausmate whisper before launching the app...")
    ssshh()
  }

  # --- Dependency checks and installation ------------------------------------
  cran_pkgs <- c("shiny", "shinyjs", "shinyFiles", "callr", "rstudioapi", "httr2")
  for (pkg in cran_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("klausmate: installing missing dependency '", pkg, "' ...")
      utils::install.packages(pkg)
    }
  }

  # --- The package may have been reinstalled while this R session was running ---
  if (klausmate_session_is_stale("talk")) return(invisible(NULL))

  # --- Preflight: check for updates (once per R session, not after a Pause) -
  updated <- klausmate_preflight()
  if (isTRUE(updated)) return(invisible(NULL))   # new talk() launched inside preflight

  # Drop the private copy of the student's API key however the app ends (Pause, Quit, Escape in
  # the console, an error)
  on.exit(cm_clear_key(), add = TRUE)

  # --- Pass the caller's working directory to the app -----------------------
  options(.klausmate_project_root = getwd())

  app_dir <- system.file("app", package = "klausmate")
  if (!nzchar(app_dir)) {
    stop("Could not locate the klausmate app directory. Try reinstalling the package.")
  }

  # A paused session remembers the port it ran on. Starting on the same port lets the old (disconnected) browser
  # tab reconnect by itself and carry on; a new tab is opened only if no tab has connected a few seconds later.
  port <- klausmate_paused_port()
  options(klausmate.session_connected = FALSE)
  launch <- if (is.null(port)) TRUE else function(url) {
    later::later(function() {
      if (!isTRUE(getOption("klausmate.session_connected"))) utils::browseURL(url)
    }, 6)
  }
  run_args <- list(appDir = app_dir, launch.browser = launch)
  if (!is.null(port)) run_args$port <- port
  suppressWarnings(suppressMessages(do.call(shiny::runApp, run_args)))

  # runApp() restores the working directory it found. If the user changed it
  # inside Klausmate (Change Working Directory, or setwd() in the Quick Console),
  # carry the change back to this R session (and so to RStudio).
  final_wd <- getOption(".klausmate_project_root")
  if (!is.null(final_wd) && dir.exists(final_wd)) {
    final_wd <- normalizePath(final_wd, winslash = "/")
    if (!identical(final_wd, normalizePath(getwd(), winslash = "/"))) {
      setwd(final_wd)
      message("Klausmate: your working directory is now ", final_wd)
    }
  }
  invisible(NULL)
}

# The port a paused session was running on (stored in its pause file), if it is still free
klausmate_paused_port <- function() {
  f <- file.path(getwd(), "claude_assistant_pause.rds")
  if (!file.exists(f)) return(NULL)
  port <- tryCatch(as.integer(readRDS(f)$port), error = function(e) NA_integer_)
  if (length(port) != 1L || is.na(port) || port < 1024L) return(NULL)
  free <- tryCatch({ srv <- httpuv::startServer("127.0.0.1", port, list()); httpuv::stopServer(srv); TRUE },
                   error = function(e) FALSE)
  if (free) port else NULL
}

# The newest release: list(version =, url =) or NULL if GitHub cannot be reached.
# The GitHub API is tried first; it only allows 60 anonymous requests an hour per network address, so a class
# sharing one connection can be refused (HTTP 403). The fallback reads the version from DESCRIPTION on GitHub
# (no such limit) and builds the release download address from it.
klausmate_latest_release <- function() {
  repo <- KLAUSMATE_GITHUB_REPO
  from_api <- tryCatch({
    resp <- httr2::request(paste0("https://api.github.com/repos/", repo, "/releases/latest")) |>
      httr2::req_headers("Accept" = "application/vnd.github+json", "X-GitHub-Api-Version" = "2022-11-28") |>
      httr2::req_timeout(6) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200L) NULL else {
      rel <- httr2::resp_body_json(resp)
      tarball <- Filter(\(a) grepl("\\.tar\\.gz$", a$name), rel$assets)
      if (length(tarball) == 0) NULL
      else list(version = gsub("^v", "", rel$tag_name %||% ""), url = tarball[[1]]$browser_download_url)
    }
  }, error = function(e) NULL)
  if (!is.null(from_api)) return(from_api)

  tryCatch({
    resp <- httr2::request(paste0("https://raw.githubusercontent.com/", repo, "/main/DESCRIPTION")) |>
      httr2::req_timeout(6) |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    if (httr2::resp_status(resp) != 200L) return(NULL)
    m <- regmatches(httr2::resp_body_string(resp), regexec("(?m)^Version:\\s*([0-9.]+)", httr2::resp_body_string(resp), perl = TRUE))[[1]]
    if (length(m) < 2) return(NULL)
    ver <- m[2]
    list(version = ver,
         url = sprintf("https://github.com/%s/releases/download/v%s/klausmate_%s.tar.gz", repo, ver, ver))
  }, error = function(e) NULL)
}

# ---------------------------------------------------------------------------
# Preflight update check
# Returns TRUE if a new version was installed and a fresh talk() was launched.
# Returns FALSE (silently) in every other case, including all error paths.
# ---------------------------------------------------------------------------
klausmate_preflight <- function(relaunch = "talk", args = list()) {

  # Already checked this session, or this is a post-update re-entry
  if (isTRUE(getOption("klausmate.update_checked"))) return(FALSE)
  options(klausmate.update_checked = TRUE)

  release <- klausmate_latest_release()
  if (is.null(release)) return(FALSE)   # offline, or GitHub could not be reached: carry on silently

  latest_version  <- release$version
  current_version <- as.character(utils::packageVersion("klausmate"))

  if (!nzchar(latest_version)) return(FALSE)
  if (utils::compareVersion(latest_version, current_version) <= 0) return(FALSE)

  # Don't retry a version whose installation already failed this session
  if (identical(getOption("klausmate.failed_update_version"), latest_version))
    return(FALSE)

  download_url <- release$url
  klausmate_do_update(latest_version, download_url, relaunch, args)
}

# ---------------------------------------------------------------------------
# Downloads and installs the new version silently and carries on with it.
# A naive live detach/reload causes the "lazyload database corrupt" error
# (R still needs the old .rdb when the new version overwrites it), so the
# lazy-load promises are all forced before installing, then the namespace is
# unloaded and loaded afresh (klausmate_reload_and_relaunch()); if that fails
# we fall back to asking the user to restart R.
# Returns TRUE on success, FALSE if anything goes wrong (caller continues
# with the existing version in that case).
# ---------------------------------------------------------------------------
klausmate_do_update <- function(latest_version, download_url, relaunch = "talk", args = list()) {

  # Pull every lazy-loaded object of the running copy into memory first, so that nothing in
  # it needs to be read from the package files once the new version has overwritten them.
  klausmate_force_lazy_objects()

  problems <- character(0)
  success <- tryCatch({
    tmp <- tempfile(fileext = ".tar.gz")
    withCallingHandlers(
      {
        utils::download.file(download_url, tmp, quiet = TRUE, mode = "wb")
        suppressMessages(utils::install.packages(tmp, repos = NULL, type = "source", quiet = TRUE))
      },
      warning = function(w) { problems <<- c(problems, conditionMessage(w)); invokeRestart("muffleWarning") })
    TRUE
  }, error = function(e) { problems <<- c(problems, conditionMessage(e)); FALSE })

  # Say so (once) if the update could not be installed: the student should not be left on an old version without
  # knowing. Running out of rate limit or being offline is not reported (see klausmate_preflight()).
  say_failed <- function() {
    options(klausmate.failed_update_version = latest_version)
    message("Klausmate: version ", latest_version, " is available but could not be installed automatically",
            if (length(problems)) paste0(" (", substr(paste(unique(problems), collapse = "; "), 1, 300), ")"), ".\n",
            "To update, restart R and, before loading klausmate, run:\n",
            "  remotes::install_github(\"", KLAUSMATE_GITHUB_REPO, "\")")
    FALSE
  }
  if (!success) return(say_failed())

  new_version <- tryCatch(
    as.character(utils::packageVersion("klausmate")),
    error = function(e) ""
  )

  if (!identical(new_version, latest_version)) return(say_failed())

  # Swap the new version in and carry on, with no restart of R needed
  if (klausmate_reload_and_relaunch(relaunch, args)) return(TRUE)

  relaunch_call <- if (relaunch == "whisper") "whisper()" else "talk()"
  message(
    "Klausmate has been updated to v", latest_version, ".\n",
    "Please restart R and then run ", relaunch_call, " to continue."
  )
  TRUE
}


# Evaluate every lazy-load promise in the klausmate namespace and attached package environment.
klausmate_force_lazy_objects <- function() {
  envs <- list(asNamespace("klausmate"))
  if ("package:klausmate" %in% search()) envs <- c(envs, list(as.environment("package:klausmate")))
  for (e in envs)
    for (n in ls(e, all.names = TRUE))
      tryCatch(get(n, envir = e), error = function(err) NULL)
  invisible(NULL)
}

# After a successful in-session update: unload the old copy, load the new one (re-attaching it if
# it was attached) and start talk()/whisper() again from it. TRUE if that worked; FALSE means the
# caller should ask for a restart of R instead.
# TRUE if the files of the installed package are newer than the copy loaded in this R session (the
# package was reinstalled or updated while R was running, so the loaded R code may not match the
# installed app). Then the new copy is loaded and `relaunch` is started again from it, or, if that is
# not possible, the user is asked to restart R. Returns TRUE when the caller should stop.
klausmate_session_is_stale <- function(relaunch = "talk", args = list()) {
  loaded    <- tryCatch(as.character(getNamespaceVersion("klausmate")), error = function(e) NA_character_)
  installed <- tryCatch(as.character(utils::packageVersion("klausmate")), error = function(e) loaded)
  if (is.na(loaded) || identical(loaded, installed)) return(FALSE)
  if (klausmate_reload_and_relaunch(relaunch, args)) return(TRUE)
  message("Klausmate was updated while R was running. Please restart R (Session > Restart R in RStudio) ",
          "and then run ", relaunch, "() again.")
  TRUE
}

klausmate_reload_and_relaunch <- function(relaunch = "talk", args = list()) {
  attached <- "package:klausmate" %in% search()
  ok <- tryCatch({
    unloadNamespace("klausmate")
    loadNamespace("klausmate")
    if (attached) suppressMessages(library("klausmate", character.only = TRUE))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(FALSE)
  # Not inside tryCatch(): whisper() registers global calling handlers, which R only allows when
  # no other handlers are on the stack.
  do.call(getExportedValue("klausmate", relaunch), args)
  TRUE
}


# ---------------------------------------------------------------------------
# klausmate_speaks() / klausmate_language()
# ---------------------------------------------------------------------------

.klausmate_known_languages <- c(
  "Afrikaans", "Albanian", "Amharic", "Arabic", "Armenian", "Azerbaijani",
  "Basque", "Belarusian", "Bengali", "Bosnian", "Bulgarian", "Catalan",
  "Chinese", "Croatian", "Czech", "Danish", "Dutch", "English",
  "Estonian", "Finnish", "French", "Galician", "Georgian", "German",
  "Greek", "Gujarati", "Hebrew", "Hindi", "Hungarian", "Icelandic",
  "Indonesian", "Irish", "Italian", "Japanese", "Kannada", "Kazakh",
  "Korean", "Latvian", "Lithuanian", "Macedonian", "Malay", "Malayalam",
  "Maltese", "Marathi", "Mongolian", "Nepali", "Norwegian", "Pashto",
  "Persian", "Polish", "Portuguese", "Punjabi", "Romanian", "Russian",
  "Serbian", "Sinhala", "Slovak", "Slovenian", "Somali", "Spanish",
  "Swahili", "Swedish", "Tamil", "Telugu", "Thai", "Turkish", "Ukrainian",
  "Urdu", "Uzbek", "Vietnamese", "Welsh", "Zulu"
)

.klausmate_lang_path <- function()
  file.path(klausmate_user_dir("config"), "language.rds")

#' Set the language klausmate responds in
#'
#' Saves a language preference that is applied to all AI responses in both
#' \code{talk()} and \code{whisper()}. R code is always written in English;
#' only explanatory text (and optionally code comments) changes language.
#' Call \code{klausmate_speaks("English")} to revert to British English.
#'
#' @param language A language name, e.g. \code{"French"}, \code{"Spanish"},
#'   \code{"Welsh"}. Minor misspellings are accepted.
#' @return Invisible canonical language name, or invisible NULL on failure.
#' @export
klausmate_speaks <- function(language) {
  lang <- trimws(language)
  if (!nzchar(lang)) {
    message("Please supply a language name, e.g. klausmate_speaks(\"French\").")
    return(invisible(NULL))
  }
  matches <- agrep(lang, .klausmate_known_languages,
                   ignore.case = TRUE, value = TRUE, max.distance = 0.25)
  if (length(matches) == 0) {
    message(
      "klausmate does not recognise '", lang, "' as a language.\n",
      "Examples of supported languages: Arabic, Chinese, Dutch, French, German, ",
      "Hindi, Italian, Japanese, Korean, Polish, Portuguese, Russian, Spanish, ",
      "Swahili, Turkish, Ukrainian, Welsh — and many more.\n",
      "Check the spelling and try again."
    )
    return(invisible(NULL))
  }
  canonical <- matches[1]
  config_dir <- klausmate_user_dir("config")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(canonical, .klausmate_lang_path())
  if (tolower(canonical) == "english") {
    message("klausmate will respond in British English.",
            " Restart talk() or whisper() to apply.")
  } else {
    message("klausmate will respond in ", canonical, ".",
            " Restart talk() or whisper() to apply.")
  }
  invisible(canonical)
}

#' @keywords internal
klausmate_language <- function() {
  path <- .klausmate_lang_path()
  if (!file.exists(path)) return("English")
  tryCatch(readRDS(path), error = function(e) "English")
}

#' Chicago census tracts with income data
#'
#' An \code{sf} object of census tracts for Chicago with median household
#' income attributes.
#'
#' @format An \code{sf} object with 856 rows and 4 variables:
#'   \describe{
#'     \item{GEOID}{Census tract identifier}
#'     \item{NAME}{Census tract name}
#'     \item{median_income}{Median household income (USD)}
#'     \item{geom}{Multipolygon geometry (WGS 84)}
#'   }
#' @source Derived from US Census / ACS data.
"chicago"
