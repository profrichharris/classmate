`%||%` <- function(x, y) if (is.null(x)) y else x

CLASSMATE_GITHUB_REPO <- "profrichharris/classmate"

#' Launch the Classmate AI tutor Shiny app
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
    message("Stopping classmate whisper before launching the app...")
    ssshh()
  }

  # --- Dependency checks and installation ------------------------------------
  cran_pkgs <- c("shiny", "shinyjs", "shinyFiles", "callr", "rstudioapi", "httr2")
  for (pkg in cran_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("classmate: installing missing dependency '", pkg, "' ...")
      utils::install.packages(pkg)
    }
  }

  # --- The package may have been reinstalled while this R session was running ---
  if (classmate_session_is_stale("talk")) return(invisible(NULL))

  # --- Preflight: check for updates (once per R session, not after a Pause) -
  updated <- classmate_preflight()
  if (isTRUE(updated)) return(invisible(NULL))   # new talk() launched inside preflight

  # Drop the private copy of the student's API key however the app ends (Pause, Quit, Escape in
  # the console, an error)
  on.exit(cm_clear_key(), add = TRUE)

  # --- Pass the caller's working directory to the app -----------------------
  options(.classmate_project_root = getwd())

  app_dir <- system.file("app", package = "classmate")
  if (!nzchar(app_dir)) {
    stop("Could not locate the classmate app directory. Try reinstalling the package.")
  }

  # A paused session remembers the port it ran on. Starting on the same port lets the old (disconnected) browser
  # tab reconnect by itself and carry on; a new tab is opened only if no tab has connected a few seconds later.
  port <- classmate_paused_port()
  options(classmate.session_connected = FALSE)
  launch <- if (is.null(port)) TRUE else function(url) {
    later::later(function() {
      if (!isTRUE(getOption("classmate.session_connected"))) utils::browseURL(url)
    }, 6)
  }
  run_args <- list(appDir = app_dir, launch.browser = launch)
  if (!is.null(port)) run_args$port <- port
  suppressWarnings(suppressMessages(do.call(shiny::runApp, run_args)))

  # runApp() restores the working directory it found. If the user changed it
  # inside Classmate (Change Working Directory, or setwd() in the Quick Console),
  # carry the change back to this R session (and so to RStudio).
  final_wd <- getOption(".classmate_project_root")
  if (!is.null(final_wd) && dir.exists(final_wd)) {
    final_wd <- normalizePath(final_wd, winslash = "/")
    if (!identical(final_wd, normalizePath(getwd(), winslash = "/"))) {
      setwd(final_wd)
      message("Classmate: your working directory is now ", final_wd)
    }
  }
  invisible(NULL)
}

# The port a paused session was running on (stored in its pause file), if it is still free
classmate_paused_port <- function() {
  f <- file.path(getwd(), "claude_assistant_pause.rds")
  if (!file.exists(f)) return(NULL)
  port <- tryCatch(as.integer(readRDS(f)$port), error = function(e) NA_integer_)
  if (length(port) != 1L || is.na(port) || port < 1024L) return(NULL)
  free <- tryCatch({ srv <- httpuv::startServer("127.0.0.1", port, list()); httpuv::stopServer(srv); TRUE },
                   error = function(e) FALSE)
  if (free) port else NULL
}

# ---------------------------------------------------------------------------
# Preflight update check
# Returns TRUE if a new version was installed and a fresh talk() was launched.
# Returns FALSE (silently) in every other case, including all error paths.
# ---------------------------------------------------------------------------
classmate_preflight <- function(relaunch = "talk", args = list()) {

  # Already checked this session, or this is a post-update re-entry
  if (isTRUE(getOption("classmate.update_checked"))) return(FALSE)
  options(classmate.update_checked = TRUE)

  repo    <- CLASSMATE_GITHUB_REPO
  api_url <- paste0("https://api.github.com/repos/", repo, "/releases/latest")

  release <- tryCatch({
    req  <- httr2::request(api_url) |>
      httr2::req_headers(
        "Accept"               = "application/vnd.github+json",
        "X-GitHub-Api-Version" = "2022-11-28"
      ) |>
      httr2::req_timeout(6) |>
      httr2::req_error(is_error = \(r) FALSE)
    resp <- httr2::req_perform(req)
    if (httr2::resp_status(resp) != 200L) return(NULL)
    httr2::resp_body_json(resp)
  }, error = function(e) NULL)

  if (is.null(release)) return(FALSE)   # network error — continue as-is

  latest_version  <- gsub("^v", "", release$tag_name %||% "")
  current_version <- as.character(utils::packageVersion("classmate"))

  if (!nzchar(latest_version)) return(FALSE)
  if (utils::compareVersion(latest_version, current_version) <= 0) return(FALSE)

  # Don't retry a version whose installation already failed this session
  if (identical(getOption("classmate.failed_update_version"), latest_version))
    return(FALSE)

  # Find the source tarball asset
  tarball <- Filter(\(a) grepl("\\.tar\\.gz$", a$name), release$assets)
  if (length(tarball) == 0) return(FALSE)
  download_url <- tarball[[1]]$browser_download_url

  classmate_do_update(latest_version, download_url, relaunch, args)
}

# ---------------------------------------------------------------------------
# Downloads and installs the new version silently and carries on with it.
# A naive live detach/reload causes the "lazyload database corrupt" error
# (R still needs the old .rdb when the new version overwrites it), so the
# lazy-load promises are all forced before installing, then the namespace is
# unloaded and loaded afresh (classmate_reload_and_relaunch()); if that fails
# we fall back to asking the user to restart R.
# Returns TRUE on success, FALSE if anything goes wrong (caller continues
# with the existing version in that case).
# ---------------------------------------------------------------------------
classmate_do_update <- function(latest_version, download_url, relaunch = "talk", args = list()) {

  # Pull every lazy-loaded object of the running copy into memory first, so that nothing in
  # it needs to be read from the package files once the new version has overwritten them.
  classmate_force_lazy_objects()

  success <- tryCatch({
    tmp <- tempfile(fileext = ".tar.gz")
    suppressWarnings(utils::download.file(download_url, tmp, quiet = TRUE, mode = "wb"))
    suppressMessages(suppressWarnings(
      utils::install.packages(tmp, repos = NULL, type = "source", quiet = TRUE)
    ))
    TRUE
  }, error = function(e) FALSE)

  if (!success) {
    options(classmate.failed_update_version = latest_version)
    return(FALSE)
  }

  new_version <- tryCatch(
    as.character(utils::packageVersion("classmate")),
    error = function(e) ""
  )

  if (!identical(new_version, latest_version)) {
    options(classmate.failed_update_version = latest_version)
    return(FALSE)
  }

  # Swap the new version in and carry on, with no restart of R needed
  if (classmate_reload_and_relaunch(relaunch, args)) return(TRUE)

  relaunch_call <- if (relaunch == "whisper") "whisper()" else "talk()"
  message(
    "Classmate has been updated to v", latest_version, ".\n",
    "Please restart R and then run ", relaunch_call, " to continue."
  )
  TRUE
}


# Evaluate every lazy-load promise in the classmate namespace and attached package environment.
classmate_force_lazy_objects <- function() {
  envs <- list(asNamespace("classmate"))
  if ("package:classmate" %in% search()) envs <- c(envs, list(as.environment("package:classmate")))
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
classmate_session_is_stale <- function(relaunch = "talk", args = list()) {
  loaded    <- tryCatch(as.character(getNamespaceVersion("classmate")), error = function(e) NA_character_)
  installed <- tryCatch(as.character(utils::packageVersion("classmate")), error = function(e) loaded)
  if (is.na(loaded) || identical(loaded, installed)) return(FALSE)
  if (classmate_reload_and_relaunch(relaunch, args)) return(TRUE)
  message("Classmate was updated while R was running. Please restart R (Session > Restart R in RStudio) ",
          "and then run ", relaunch, "() again.")
  TRUE
}

classmate_reload_and_relaunch <- function(relaunch = "talk", args = list()) {
  attached <- "package:classmate" %in% search()
  ok <- tryCatch({
    unloadNamespace("classmate")
    loadNamespace("classmate")
    if (attached) suppressMessages(library("classmate", character.only = TRUE))
    TRUE
  }, error = function(e) FALSE)
  if (!ok) return(FALSE)
  # Not inside tryCatch(): whisper() registers global calling handlers, which R only allows when
  # no other handlers are on the stack.
  do.call(getExportedValue("classmate", relaunch), args)
  TRUE
}


# ---------------------------------------------------------------------------
# classmate_speaks() / classmate_language()
# ---------------------------------------------------------------------------

.classmate_known_languages <- c(
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

.classmate_lang_path <- function()
  file.path(tools::R_user_dir("classmate", "config"), "language.rds")

#' Set the language classmate responds in
#'
#' Saves a language preference that is applied to all AI responses in both
#' \code{talk()} and \code{whisper()}. R code is always written in English;
#' only explanatory text (and optionally code comments) changes language.
#' Call \code{classmate_speaks("English")} to revert to British English.
#'
#' @param language A language name, e.g. \code{"French"}, \code{"Spanish"},
#'   \code{"Welsh"}. Minor misspellings are accepted.
#' @return Invisible canonical language name, or invisible NULL on failure.
#' @export
classmate_speaks <- function(language) {
  lang <- trimws(language)
  if (!nzchar(lang)) {
    message("Please supply a language name, e.g. classmate_speaks(\"French\").")
    return(invisible(NULL))
  }
  matches <- agrep(lang, .classmate_known_languages,
                   ignore.case = TRUE, value = TRUE, max.distance = 0.25)
  if (length(matches) == 0) {
    message(
      "classmate does not recognise '", lang, "' as a language.\n",
      "Examples of supported languages: Arabic, Chinese, Dutch, French, German, ",
      "Hindi, Italian, Japanese, Korean, Polish, Portuguese, Russian, Spanish, ",
      "Swahili, Turkish, Ukrainian, Welsh — and many more.\n",
      "Check the spelling and try again."
    )
    return(invisible(NULL))
  }
  canonical <- matches[1]
  config_dir <- tools::R_user_dir("classmate", "config")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
  saveRDS(canonical, .classmate_lang_path())
  if (tolower(canonical) == "english") {
    message("classmate will respond in British English.",
            " Restart talk() or whisper() to apply.")
  } else {
    message("classmate will respond in ", canonical, ".",
            " Restart talk() or whisper() to apply.")
  }
  invisible(canonical)
}

#' @keywords internal
classmate_language <- function() {
  path <- .classmate_lang_path()
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
