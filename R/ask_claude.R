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

  # --- Pass the caller's working directory to the app -----------------------
  # shiny::runApp() changes the working directory to the app folder before
  # sourcing the app script.  We capture the real project root here and pass
  # it via an option so the app can create outputs/ in the right place.
  options(.classmate_project_root = getwd())

  app_dir <- system.file("app", package = "classmate")
  if (!nzchar(app_dir)) {
    stop("Could not locate the classmate app directory. Try reinstalling the package.")
  }

  shiny::runApp(app_dir, launch.browser = TRUE)
  invisible(NULL)
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
