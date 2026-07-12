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
`%||%` <- function(x, y) if (is.null(x)) y else x

CLASSMATE_GITHUB_REPO <- "profrichharris/classmate"

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
classmate_preflight <- function() {

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

  classmate_do_update(latest_version, download_url)
}

# ---------------------------------------------------------------------------
# Shows a small Shiny update page, installs the new version in a background
# callr process, then re-launches the app under the new package version.
# Returns TRUE on success, FALSE if anything goes wrong (install silently
# falls back to the existing version).
# ---------------------------------------------------------------------------
classmate_do_update <- function(latest_version, download_url) {

  ui <- shiny::fluidPage(
    shiny::tags$head(shiny::tags$style("
      body { font-family: 'Helvetica Neue', Helvetica, Arial, sans-serif;
             background: #f4f4f4; margin: 0; }
      .update-box { max-width: 420px; margin: 120px auto; background: #fff;
                    border-radius: 8px; padding: 36px 40px;
                    box-shadow: 0 2px 12px rgba(0,0,0,0.10); text-align: center; }
      h3 { margin: 0 0 6px 0; font-size: 1.35em; }
      .sub { color: #666; font-size: 0.92em; margin-bottom: 22px; }
      .progress { height: 8px; border-radius: 4px; background: #e9ecef;
                  overflow: hidden; margin-bottom: 18px; }
      .progress-bar { height: 100%; background: #337ab7; border-radius: 4px;
                      animation: slide 1.4s linear infinite; width: 40%; }
      @keyframes slide { from { margin-left: -40%; } to { margin-left: 100%; } }
      .note { color: #aaa; font-size: 0.8em; }
    ")),
    shiny::div(class = "update-box",
      shiny::tags$h3("Updating Classmate"),
      shiny::p(class = "sub", paste0("Installing version ", latest_version, "…")),
      shiny::div(class = "progress", shiny::div(class = "progress-bar")),
      shiny::p(class = "note", "This window will close automatically when done.")
    )
  )

  captured_url <- download_url

  server <- function(input, output, session) {
    proc <- callr::r_bg(
      func = function(url) {
        tmp <- tempfile(fileext = ".tar.gz")
        utils::download.file(url, tmp, quiet = TRUE, mode = "wb")
        utils::install.packages(tmp, repos = NULL, type = "source", quiet = TRUE)
        invisible(NULL)
      },
      args    = list(url = captured_url),
      package = FALSE
    )

    shiny::observe({
      shiny::invalidateLater(600, session)
      if (!proc$is_alive()) {
        shiny::stopApp(returnValue = proc$get_exit_status())
      }
    })
  }

  exit_status <- tryCatch(
    shiny::runApp(shiny::shinyApp(ui, server), launch.browser = TRUE, quiet = TRUE),
    error = function(e) 1L
  )

  # exit_status == 0 means the install process completed without error.
  # We still verify by checking the new package version on disk.
  new_version <- tryCatch(
    as.character(utils::packageVersion("classmate",
      lib.loc = .libPaths()[.libPaths() != ""])),
    error = function(e) ""
  )

  if (!identical(new_version, latest_version)) {
    # Install did not produce the expected version — record failure, carry on
    options(classmate.failed_update_version = latest_version)
    return(FALSE)
  }

  # Detach old namespace and reload the freshly installed version
  tryCatch({
    if ("package:classmate" %in% search())
      detach("package:classmate", unload = TRUE, force = TRUE)
    library(classmate)
    message("Classmate updated to v", latest_version, ". Launching…")
    classmate::ask()
    return(TRUE)
  }, error = function(e) {
    message("Update installed. Please call ask() to launch the updated app.")
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
