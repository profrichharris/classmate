`%||%` <- function(x, y) if (is.null(x)) y else x

CLASSMATE_GITHUB_REPO <- "profrichharris/classmate"

#' Launch the Classmate AI assistant Shiny app
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
#' ask()
#' }
ask <- function() {

  # --- Dependency checks and installation ------------------------------------
  cran_pkgs <- c("shiny", "shinyjs", "shinyFiles", "callr", "rstudioapi", "httr2")
  for (pkg in cran_pkgs) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      message("classmate: installing missing dependency '", pkg, "' ...")
      utils::install.packages(pkg)
    }
  }

  # --- Preflight: check for updates (once per R session, not after a Pause) -
  updated <- classmate_preflight()
  if (isTRUE(updated)) return(invisible(NULL))   # new ask() launched inside preflight

  # --- Pass the caller's working directory to the app -----------------------
  options(.classmate_project_root = getwd())

  app_dir <- system.file("app", package = "classmate")
  if (!nzchar(app_dir)) {
    stop("Could not locate the classmate app directory. Try reinstalling the package.")
  }

  suppressWarnings(suppressMessages(
    shiny::runApp(app_dir, launch.browser = TRUE)
  ))
  invisible(NULL)
}

# ---------------------------------------------------------------------------
# Preflight update check
# Returns TRUE if a new version was installed and a fresh ask() was launched.
# Returns FALSE (silently) in every other case, including all error paths.
# ---------------------------------------------------------------------------
classmate_preflight <- function(relaunch = "ask") {

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

  classmate_do_update(latest_version, download_url, relaunch)
}

# ---------------------------------------------------------------------------
# Downloads and installs the new version silently, then relaunches.
# relaunch = "ask"   → calls classmate::ask()
# relaunch = "watch" → calls classmate::watch()
# Returns TRUE on success, FALSE if anything goes wrong (caller continues
# with the existing version in that case).
# ---------------------------------------------------------------------------
classmate_do_update <- function(latest_version, download_url, relaunch = "ask") {

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

  relaunch_fn  <- if (relaunch == "watch") "watch" else "ask"
  relaunch_msg <- if (relaunch == "watch")
    paste0("Classmate updated to v", latest_version, ". Please call watch() to continue.")
  else
    paste0("Classmate updated to v", latest_version, ". Please call ask() to launch.")

  tryCatch({
    if ("package:classmate" %in% search())
      detach("package:classmate", unload = TRUE, force = TRUE)
    library(classmate)
    do.call(getExportedValue("classmate", relaunch_fn), list())
    return(TRUE)
  }, error = function(e) {
    message(relaunch_msg)
    return(TRUE)
  })
}


#' Chicago city boundary
#'
#' An \code{sf} polygon representing the boundary of the city of Chicago.
#'
#' @format An \code{sf} object with a \code{NAME} column and a \code{geometry}
#'   column (MULTIPOLYGON, WGS 84).
#' @source Derived from the City of Chicago open data portal.
"chicago_boundary"

#' Chicago census tracts with income data
#'
#' An \code{sf} object of census tracts for Chicago with associated income
#' attributes.
#'
#' @format An \code{sf} object with tract-level variables and a \code{geometry}
#'   column (MULTIPOLYGON).
#' @source Derived from US Census / ACS data.
"chicago_tracts_income"

#' Rogers Park residential buildings
#'
#' An \code{sf} object of residential building footprints in the Rogers Park
#' neighbourhood of Chicago.
#'
#' @format An \code{sf} object with building-level attributes and a
#'   \code{geometry} column (MULTIPOLYGON / POLYGON).
#' @source Derived from the City of Chicago open data portal.
"chicago_rogerspark_residential_buildings"
