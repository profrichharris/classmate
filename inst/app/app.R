library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyAce)
library(httr2)

# --- Project root (= the R working directory) and output paths --------------
# talk() stores the user's working directory in this option before calling
# runApp(), which would otherwise change the wd to this inst/app/ folder.
# Fall back to getwd() when the app is run directly for development.
# The root can change during a session (Change Working Directory button, or
# setwd() typed in the Quick Console); talk() applies the final value to the R
# session when the app closes.
.cm <- new.env()
.cm$root <- normalizePath(getOption(".classmate_project_root", getwd()),
                          winslash = "/", mustWork = FALSE)
project_root <- function() {
  if (!dir.exists(.cm$root)) cm_ensure_root()
  .cm$root
}
outputs_dir  <- function() file.path(project_root(), "outputs")
pause_file   <- function() file.path(project_root(), "claude_assistant_pause.rds")
dir.create(outputs_dir(), showWarnings = FALSE, recursive = TRUE)

# If the project folder has disappeared (moved, deleted, drive unplugged), fall back to the nearest
# folder that still exists, so nothing downstream fails. The server shows a notice (.cm$fallback).
cm_ensure_root <- function() {
  if (dir.exists(.cm$root)) return(invisible(NULL))
  lost   <- .cm$root
  parent <- lost
  repeat {
    up <- dirname(parent)
    if (identical(up, parent)) break
    parent <- up
    if (dir.exists(parent)) break
  }
  for (d in unique(c(if (dir.exists(parent)) parent, path.expand("~"), tempdir()))) {
    if (!dir.exists(d)) next
    .cm$root <- normalizePath(d, winslash = "/")
    options(.classmate_project_root = .cm$root)
    suppressWarnings(dir.create(file.path(.cm$root, "outputs"), showWarnings = FALSE, recursive = TRUE))
    .cm$fallback <- list(from = lost, to = .cm$root)
    return(invisible(.cm$root))
  }
  invisible(NULL)
}

# Switches to the project folder for the duration of a run and returns a restore() that never
# errors (the folder Classmate itself runs from may have been replaced, e.g. by a reinstall).
cm_enter_root <- function() {
  root <- project_root()
  old  <- tryCatch(getwd(), error = function(e) NULL)
  ok   <- tryCatch({ setwd(root); TRUE }, error = function(e) FALSE)
  list(ok = ok, restore = function() {
    if (!is.null(old) && nzchar(old) && dir.exists(old)) try(setwd(old), silent = TRUE)
  })
}

# Makes `new_dir` the project root. Returns list(ok, changed, path, outputs_ok, error).
cm_set_root <- function(new_dir) {
  p <- tryCatch(normalizePath(path.expand(new_dir), winslash = "/", mustWork = TRUE),
                error = function(e) NA_character_)
  if (is.na(p) || !dir.exists(p))
    return(list(ok = FALSE, changed = FALSE, error = "That folder does not exist."))
  if (identical(p, .cm$root)) return(list(ok = TRUE, changed = FALSE, path = p, outputs_ok = TRUE))
  outputs_ok <- suppressWarnings(
    dir.create(file.path(p, "outputs"), showWarnings = FALSE, recursive = TRUE) ||
      dir.exists(file.path(p, "outputs")))
  .cm$root <- p
  options(.classmate_project_root = p)
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable())
    try(rstudioapi::filesPaneNavigate(p), silent = TRUE)
  list(ok = TRUE, changed = TRUE, path = p, outputs_ok = outputs_ok)
}

# Serve temp plot files captured during code runs
addResourcePath("classmate_plots", tempdir())

# --- Constants ---------------------------------------------------------------
MAX_HISTORY_TURNS     <- 8
KEY_CHECK_INTERVAL_MS <- 10 * 60 * 1000

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pinned to Sonnet.  Haiku hallucinated a non-existent tmap function in
# side-by-side testing so it is not offered here.
MODEL_SONNET <- "claude-sonnet-5-5"
MODEL_SONNET_LABEL <- "Claude Sonnet 5.5"
MODEL_HAIKU  <- "claude-haiku-5-5"

# Extra request fields each model needs. Sonnet 5.5 and Haiku 5.5 "think" before answering by
# default (slower, more output tokens, and a thinking block ahead of the text), so that is
# switched off. (Sonnet 5.5 accepts "between_tools" but not "disabled"; Haiku 5.5 the reverse.)
model_extra_params <- function(model) {
  if (grepl("^claude-sonnet-5", model)) list(thinking = list(type = "between_tools"))
  else if (grepl("^claude-haiku-5", model)) list(thinking = list(type = "disabled"))
  else list()
}
# The reply text, whichever order the content blocks come in
first_text_block <- function(content) {
  for (b in content %||% list()) if (identical(b$type, "text")) return(b$text %||% "")
  ""
}

CLASSMATE_LANGUAGES <- c(
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

# --- System-prompt building blocks -------------------------------------------

r_system_prompt <- paste(
  "The user is working in R, inside RStudio.",
  "Please prefer to answer with R code.",
  "If another programming language is genuinely necessary (for example",
  "Python via the reticulate package), make sure any code you provide can",
  "be run from within this R/RStudio session, and briefly explain how.",
  "COLUMN NAME RULE: When files or objects are provided with a known schema",
  "(listed as columns: ... in the context), you MUST use only those exact",
  "column names in any code you write. Do not guess, abbreviate, or paraphrase",
  "column names. If the user refers to a variable by a name that does not",
  "exactly match a listed column, identify the closest actual match and use that."
)

tmap_v4_instruction <- paste(
  "IMPORTANT: Always use tmap version 4 syntax. tmap v4 introduced breaking",
  "changes from v3. Key differences to follow strictly:",
  "(1) Use tm_shape() + tm_polygons(), tm_lines(), tm_dots(), tm_symbols() etc.",
  "    as in v3, but note that many argument names changed.",
  "(2) 'col' replaces the old 'palette' argument for colour mapping,",
  "    e.g. tm_polygons(col = 'variable', fill.palette = 'Blues').",
  "    In v4 use fill = 'variable' and fill.palette or col.palette.",
  "(3) Use fill = 'variable' (not col =) for choropleth polygon fills.",
  "(4) tm_layout() arguments have changed: use frame = FALSE not frame = FALSE",
  "    and check v4 docs; legend.outside is still supported.",
  "(5) tmap_mode('plot') and tmap_mode('view') still work.",
  "(6) Do NOT use deprecated v3 functions or argument names such as",
  "    'palette =', 'n =', 'style =' at the top level of tm_polygons();",
  "    in v4 these are fill.scale = tm_scale_intervals(n=, style=, values=) etc.",
  "(7) For classified maps use fill.scale = tm_scale_intervals() or",
  "    tm_scale_fixed() rather than passing style/breaks directly.",
  "Never produce tmap v3 code even if the user's existing code uses v3."
)

tmap_breaks_instruction <- paste(
  "If you produce a tmap classification with manual or fixed breaks (e.g.",
  "via tm_fill()/tm_polygons() with style = \"fixed\" or explicit breaks/labels),",
  "the class labels must NEVER be ambiguous about which class a boundary",
  "value belongs to. Do not use plain ranges like \"0-10\", \"10-20\", \"20-30\"",
  "(it is unclear which class the value 10 belongs to). Instead use clear",
  "interval notation showing exactly one inclusive (closed) end per",
  "boundary, e.g. \"(0,10]\", \"(10,20]\", \"(20,30]\" or \"[0,10)\",",
  "\"[10,20)\", \"[20,30)\" - choosing whichever convention (closed-left or",
  "closed-right) is more natural for the variable being mapped, and",
  "applying it consistently across all classes. The square bracket marks",
  "the side where the boundary value itself belongs."
)

filename_timestamp_instruction <- function() paste0(
  "HARD RULE — do NOT add any file-saving or export code unless the user's prompt ",
  "explicitly asks to save or export something (e.g. 'save the map', 'export to CSV', ",
  "'write the results to a file'). If the prompt only asks to create a plot, map, or ",
  "object, just create it — do not append saving code automatically. ",

  "When saving IS requested: always save into this exact folder, which already exists: ",
  outputs_dir(), ". ",
  "Use file.path(\"", outputs_dir(), "\", filename) as the save path. ",
  "Do NOT call dir.create() — the folder is guaranteed to exist. ",
  "If the user has NOT specified a filename, default to a timestamp-based name: ",
  "paste0(format(Sys.time(), \"%Y-%m-%d-%H-%M-%S\"), \".ext\") ",
  "replacing .ext with the appropriate extension. ",

  "HARD RULE for saving image output (png, pdf, tiff, jpeg, bmp) when saving IS requested: ",
  "NEVER use tmap_save(), ggsave(), or any package-level save function for images. ",
  "Always use the explicit device pattern: open the device, print/plot the object, ",
  "close the device. For example: ",
  "  png(file.path(\"", outputs_dir(), "\", fname), width=W, height=H, units=\"mm\", res=DPI) ",
  "  print(map_or_plot_object) ",
  "  dev.off() ",
  "Replace png() with pdf(), tiff(), jpeg(), or bmp() as appropriate for the chosen format. ",
  "This applies to tmap, ggplot2, base R graphics, and any other plotting system."
)

format_preferences_clause <- function(coding, plotting, mapping, max_lines, comment_density) {
  comment_instruction <- switch(comment_density,
    "None"    = paste(
      "Do not include any comments in the code — no # lines at all.",
      "The code should be entirely self-explanatory through naming."
    ),
    "Minimal" = paste(
      "Include only brief section-marker comments that divide the code into",
      "its major logical steps (e.g. '# Load data', '# Fit model', '# Plot results').",
      "No inline comments, no explanatory prose comments."
    ),
    "Most"    = paste(
      "Include section-marker comments (e.g. '# Load data') plus brief inline",
      "notes on any lines that would not be immediately obvious to a typical student.",
      "Do not comment every line — restrict inline notes to genuinely non-obvious choices.",
      "Keep each comment concise (≤ 8 words)."
    ),
    ""
  )
  paste0(
    "The user has the following default style preferences (not hard ",
    "requirements): for general data wrangling/coding, prefer ", coding,
    "; for plotting, prefer ", plotting, "; for mapping, prefer ", mapping, ". ",
    "Follow these by default, but always use the simplest approach that is ",
    "clear to students — for example, use base R summary() rather than a ",
    "tidyverse equivalent when that is more natural and easier to understand. ",
    "Defer to whatever the user's specific prompt asks for (e.g. 'use base R for this'), ",
    "and feel free to deviate from a preference if it would be unusually awkward, ",
    "inelegant, or unable to do what's being asked - if you do deviate, ",
    "say so briefly in a one-line comment. ",
    if (is.infinite(max_lines))
      "CODE LENGTH: There is no code length limit set. Produce the most concise solution possible and do not pad with boilerplate. "
    else
      paste0("CODE LENGTH: This app is designed for focused, self-contained code chunks rather ",
             "than complete scripts. Aim for no more than approximately ", max_lines, " lines of R code ",
             "(excluding blank lines and comments). If the task genuinely requires more, produce the ",
             "most concise solution possible and do not pad with boilerplate. "),
    comment_instruction
  )
}

format_image_preferences_clause <- function(img_format, img_quality, img_size) {
  quality_note <- switch(img_quality,
    "Low"    = "low resolution, suitable for screen/web display (~72-96 dpi)",
    "Medium" = "medium resolution, suitable for standard print quality (~150-200 dpi)",
    "High"   = "high resolution, suitable for high-quality print (~300+ dpi)",
    img_quality
  )
  paste0(
    "For image export, the user's default preferences are: file format = ", img_format,
    "; quality/resolution = ", img_quality, " (", quality_note, ")",
    "; target output size = ", img_size, ". ",
    "Apply these defaults when writing image-saving code (e.g. ggsave(), png(), ",
    "pdf(), tiff(), jpeg()) — use appropriate width, height, and res/dpi arguments. ",
    "Defer to the user's explicit prompt if they specify something different, and feel ",
    "free to deviate if the format or size is clearly inappropriate for the output type."
  )
}

code_description_rule <- paste(
  "CODE DESCRIPTION RULE: Whenever your response includes an R code block,",
  "place the following on the very first line of your response, before any",
  "other text or code:",
  "DESCRIPTION: <a short phrase of no more than eight words summarising what the code does>",
  "Example: DESCRIPTION: Map income by census tract",
  "Do not use this line if your response contains no code."
)

code_only_rule <- paste(
  "CODE-ONLY RULE: Reply with the DESCRIPTION line and the code block, and nothing else. Add no",
  "explanation, summary, notes or commentary before or after the code (the student can press",
  "Explain for that). This does not apply to OUT_OF_SCOPE, NEEDS_CLARIFICATION or",
  "DISCLOSURE_RISK replies, which keep their own formats."
)

scope_rule <- paste(
  "SCOPE RULE: Your only job is to help students write or understand R code.",
  "If the user's request has no conceivable connection to writing, running,",
  "debugging, or understanding R code — for example, they are asking for a poem,",
  "a joke, a recipe, sports scores, or general knowledge with no R angle —",
  "respond with exactly two words and nothing else: OUT_OF_SCOPE.",
  "If there is any plausible R or data connection, answer normally.",
  "When in doubt, answer normally."
)

clarity_rule <- paste(
  "CLARITY RULE: Before generating code, ask: can I write code that directly uses",
  "this student's actual named objects, files, or variables — not generic placeholder",
  "code (e.g. df, x, y, your_data, myfile)? If yes, proceed — even if the exact",
  "plot type or model type has not been specified; pick the most sensible default.",
  "Only respond with NEEDS_CLARIFICATION if: (a) there is no context at all and",
  "the prompt cannot be grounded in any real data or object; (b) the intent is",
  "clear but you cannot identify which specific variable or object to work with; or",
  "(c) the prompt is so ambiguous that any code you write would use placeholder",
  "names unconnected to the student's actual data.",
  "Do NOT trigger clarification just because a plot type or model type is unspecified",
  "— if the data and variables are discernible, pick a sensible default and proceed.",
  "When clarification IS needed, respond with exactly this format and nothing else:",
  "NEEDS_CLARIFICATION",
  "REASON: <plain English explanation of what is missing and why it prevents grounded code>",
  "SUGGESTIONS: <brief guidance on what the student should add to the prompt>"
)

research_integrity_rule <- paste(
  "RESEARCH INTEGRITY RULE: In all responses, conduct yourself as an academic",
  "researcher upholding the highest standards of research ethics and integrity.",
  "This means: always use statistically appropriate methods for the data and",
  "question at hand; report results honestly, including limitations, assumptions,",
  "and the possibility of null or inconclusive findings; never suggest manipulating,",
  "fabricating, selectively omitting, or misrepresenting data or results; produce",
  "visualisations with truthful scales and representations that do not mislead",
  "(for example, do not truncate axes to exaggerate differences without clear",
  "justification); write code that is transparent and reproducible. If a request",
  "would lead to misleading or ethically questionable analysis, note the concern",
  "briefly in a comment before proceeding — or decline if the request is clearly",
  "intended to misrepresent data."
)

disclosure_risk_rule <- paste(
  "DISCLOSURE RISK RULE: Never generate code that would produce a publishable or",
  "exportable output containing personally identifiable information about named",
  "individuals. This includes: (1) captions, titles, annotations, or labels that",
  "name a specific real person alongside any personal attribute (salary, address,",
  "health condition, relationship status, or any other personal detail); (2) tables",
  "formatted for publication or export (e.g. using knitr::kable(), gt(),",
  "flextable(), write.csv(), or saved to PDF/Excel/image) that contain rows",
  "clearly identifiable to specific named individuals with personal attributes.",
  "Exploratory console output such as print(), head(), str(), and summary() is",
  "acceptable. If the request would produce such a disclosure, respond with exactly",
  "this format and nothing else:",
  "DISCLOSURE_RISK",
  "REASON: <plain English explanation of what personal information would be revealed",
  "and why this raises a disclosure concern>"
)

web_key_ui <- function(url_id, btn_id) {
  tagList(
    div(style = "text-align: center; margin: 4px 0 8px 0;", tags$strong("Or")),
    div(style = "display: flex; align-items: flex-start; gap: 8px;",
      actionButton(btn_id, "Load from Web", class = "btn-default",
                   style = "flex-shrink: 0;"),
      div(style = "flex-grow: 1;",
        textInput(url_id, NULL, value = "", width = "100%",
                  placeholder = "Enter web address"))
    )
  )
}

build_language_clause <- function(language) {
  if (tolower(trimws(language)) == "english") {
    paste(
      "LANGUAGE RULE: Write all responses in British English",
      "(e.g. 'colour' not 'color', 'analyse' not 'analyze', 'centre' not 'center')."
    )
  } else {
    paste0(
      "LANGUAGE RULE: Write all explanatory text and responses in ", language, ". ",
      "Always write R code itself — including all variable names, function names, ",
      "and object names — in English. ",
      "You may write # comments within code blocks in ", language, " if that is ",
      "natural and helpful for the student. ",
      "Do not translate R syntax, package names, or argument names."
    )
  }
}

build_system_prompt <- function(coding, plotting, mapping, img_format, img_quality, img_size,
                                max_lines = 50, comment_density = "Minimal",
                                loaded_pkgs = character(0),
                                language = "English") {
  library_clause <- paste0(
    "PACKAGE LOADING RULE: Always include an explicit library() call for every ",
    "package your code uses. ",
    if (length(loaded_pkgs) > 0)
      paste0("The following packages have already been loaded with library() earlier ",
             "in this session — do NOT include library() for them again: ",
             paste(loaded_pkgs, collapse = ", "), ". ")
    else "",
    "For any package that is not available on CRAN, add a comment on the line ",
    "immediately before its library() call in exactly this form: ",
    "# github: username/repo (for example: # github: r-tmap/tmap). ",
    "Use this comment only for genuinely non-CRAN packages."
  )
  blocks <- c(
    r_system_prompt,
    library_clause,
    if (tolower(mapping) == "tmap") tmap_v4_instruction,
    tmap_breaks_instruction,
    filename_timestamp_instruction(),
    format_preferences_clause(coding, plotting, mapping, max_lines, comment_density),
    format_image_preferences_clause(img_format, img_quality, img_size),
    build_language_clause(language),
    code_description_rule,
    code_only_rule,
    scope_rule,
    clarity_rule,
    research_integrity_rule,
    disclosure_risk_rule
  )
  paste(blocks, collapse = "\n\n")
}

# --- Pure helper functions ---------------------------------------------------

parse_needs_clarification <- function(text) {
  lines  <- strsplit(trimws(text), "\n")[[1]]
  reason <- ""; suggestions <- ""
  for (ln in lines) {
    if (grepl("^REASON:",      ln)) reason      <- trimws(sub("^REASON:",      "", ln))
    if (grepl("^SUGGESTIONS:", ln)) suggestions <- trimws(sub("^SUGGESTIONS:", "", ln))
  }
  list(reason = reason, suggestions = suggestions)
}

parse_disclosure_risk <- function(text) {
  lines  <- strsplit(trimws(text), "\n")[[1]]
  reason <- ""
  for (ln in lines) {
    if (grepl("^REASON:", ln)) reason <- trimws(sub("^REASON:", "", ln))
  }
  list(reason = reason)
}

take_last_n <- function(lst, n) {
  if (length(lst) <= n) return(lst)
  lst[(length(lst) - n + 1):length(lst)]
}

format_already_run_block <- function(run_history) {
  if (length(run_history) == 0) return("")
  code_concat <- paste(vapply(run_history, function(e) e$code, character(1)), collapse = "\n\n")
  paste0(
    "The following code has ALREADY been run successfully in this R session. ",
    "Do not repeat, reload, or recreate anything it already did - any files it ",
    "loaded or objects it created already exist. Only provide the NEW code ",
    "needed for the next step:\n```r\n", code_concat, "\n```"
  )
}

# ---------------------------------------------------------------------------
# "Filter by type" for the Select objects dialog.
# Matches objects either by their formal R class ("sf", "integer", "lm") or by
# everyday wording ("data table", "mappable", "model", "text"). Entirely local:
# nothing is sent to the API.
# ---------------------------------------------------------------------------

type_norm <- function(x) trimws(gsub("[._/\\-]+", " ", tolower(x)))

.has_coord_cols <- function(o) {
  if (!is.data.frame(o)) return(FALSE)
  nm <- tolower(names(o))
  any(nm %in% c("lon", "long", "lng", "longitude")) && any(nm %in% c("lat", "latitude")) ||
    (any(nm %in% c("easting", "eastings")) && any(nm %in% c("northing", "northings")))
}
.sf_geom_class <- function(o) {
  if (inherits(o, "sfc")) return(toupper(class(o)))
  if (!inherits(o, "sf")) return(character(0))
  col <- attr(o, "sf_column")
  if (is.null(col)) return(character(0))
  toupper(class(o[[col]]))
}
.is_spatial_vector <- function(o)
  inherits(o, c("sf", "sfc", "sfg", "Spatial", "SpatVector", "SpatialPolygons",
                "SpatialPoints", "SpatialLines"))
.is_raster <- function(o)
  inherits(o, c("RasterLayer", "RasterStack", "RasterBrick", "SpatRaster",
                "SpatRasterDataset", "stars", "stars_proxy"))
.is_model <- function(o)
  inherits(o, c("lm", "glm", "nls", "lme", "lmerMod", "glmerMod", "gam", "aov",
                "Arima", "arima", "coxph", "loess", "randomForest", "rpart",
                "svm", "nnet", "train", "kmeans", "prcomp", "princomp", "hclust",
                "sarlm", "splm", "lagsarlm", "errorsarlm", "spautolm", "mlm", "manova")) ||
    (is.list(o) && !is.data.frame(o) && !is.null(o$coefficients))

# word lists are matched after type_norm(); the first word is the canonical name
.TYPE_CATS <- list(
  table = list(
    words = c("table", "data table", "tabular", "dataset", "data set", "data frame",
              "dataframe", "df", "spreadsheet", "data", "rectangular", "records",
              "rows and columns"),
    pred = function(o) is.data.frame(o) && !inherits(o, "sf")),
  tibble = list(
    words = c("tibble", "tbl", "tbl df", "tidy table"),
    pred = function(o) inherits(o, "tbl_df")),
  datatable = list(
    words = c("data table package", "dt", "datatable"),
    pred = function(o) inherits(o, "data.table")),
  spatial = list(
    words = c("spatial", "sf", "geographic", "geographical", "geospatial", "gis",
              "geometry", "geometries", "shapefile", "simple features", "simple feature",
              "sfc", "spatial vector", "vector layer", "geodata", "boundaries",
              "boundary", "spatial data"),
    pred = .is_spatial_vector),
  raster = list(
    words = c("raster", "grid", "gridded", "image", "imagery", "satellite", "pixel",
              "pixels", "terra", "stars", "cell", "cells", "surface", "spatraster"),
    pred = .is_raster),
  mappable = list(
    words = c("mappable", "map", "maps", "can be mapped", "plottable on a map",
              "geographic", "mapping", "has coordinates", "coordinates"),
    pred = function(o) .is_spatial_vector(o) || .is_raster(o) || .has_coord_cols(o)),
  point = list(
    words = c("point", "points", "point data", "pts"),
    pred = function(o) any(grepl("POINT", .sf_geom_class(o))) || inherits(o, "SpatialPoints")),
  line = list(
    words = c("line", "lines", "linestring", "linestrings", "polyline", "polylines",
              "roads", "routes", "network lines"),
    pred = function(o) any(grepl("LINE", .sf_geom_class(o))) || inherits(o, "SpatialLines")),
  polygon = list(
    words = c("polygon", "polygons", "area", "areas", "areal", "zones", "zone",
              "choropleth", "multipolygon", "boundary polygons"),
    pred = function(o) any(grepl("POLYGON", .sf_geom_class(o))) || inherits(o, "SpatialPolygons")),
  weights = list(
    words = c("weights", "spatial weights", "neighbours", "neighbors", "nb", "listw",
              "neighbour list", "contiguity", "weights matrix"),
    pred = function(o) inherits(o, c("nb", "listw", "knn"))),
  model = list(
    words = c("model", "models", "regression", "fitted", "fitted model", "fit",
              "statistical model", "estimate", "estimates", "lm", "glm", "ols"),
    pred = .is_model),
  test = list(
    words = c("test", "hypothesis test", "htest", "t test", "chi squared", "significance test"),
    pred = function(o) inherits(o, "htest")),
  plot = list(
    words = c("plot", "plots", "chart", "charts", "figure", "figures", "visualisation",
              "visualization", "ggplot", "graphic", "graphics", "diagram", "picture"),
    pred = function(o) inherits(o, c("gg", "ggplot", "recordedplot", "trellis",
                                     "grob", "gtable", "patchwork", "gTree"))),
  `function` = list(
    words = c("function", "functions", "fn", "func", "closure", "method", "callable"),
    pred = function(o) is.function(o)),
  numeric = list(
    words = c("numeric", "number", "numbers", "numerical", "double", "dbl", "num",
              "quantitative", "decimal", "float"),
    pred = function(o) is.numeric(o) && !is.factor(o) && !inherits(o, c("Date", "POSIXt", "difftime"))),
  integer = list(
    words = c("integer", "integers", "int", "whole number", "whole numbers", "count"),
    pred = function(o) is.integer(o) && !is.factor(o)),
  character = list(
    words = c("character", "characters", "text", "string", "strings", "chr", "words",
              "textual", "labels"),
    pred = function(o) is.character(o)),
  logical = list(
    words = c("logical", "boolean", "bool", "lgl", "true false", "flag", "flags"),
    pred = function(o) is.logical(o)),
  factor = list(
    words = c("factor", "factors", "categorical", "category", "categories", "fct",
              "levels", "ordinal", "nominal"),
    pred = function(o) is.factor(o)),
  datetime = list(
    words = c("date", "dates", "time", "times", "datetime", "date time", "timestamp",
              "dttm", "posixct", "posixt", "temporal", "calendar"),
    pred = function(o) inherits(o, c("Date", "POSIXt", "difftime"))),
  timeseries = list(
    words = c("time series", "timeseries", "ts", "series", "xts", "zoo", "tsibble"),
    pred = function(o) inherits(o, c("ts", "xts", "zoo", "tsibble", "mts"))),
  list = list(
    words = c("list", "lists", "nested", "named list"),
    pred = function(o) identical(class(o), "list")),
  matrix = list(
    words = c("matrix", "matrices", "array", "arrays", "grid of numbers"),
    pred = function(o) is.matrix(o) || is.array(o)),
  vector = list(
    words = c("vector", "vectors", "atomic", "column", "variable"),
    pred = function(o) is.atomic(o) && is.null(dim(o)) && !is.factor(o) && length(o) != 1L),
  scalar = list(
    words = c("scalar", "single value", "one value", "constant", "parameter", "value", "values"),
    pred = function(o) is.atomic(o) && length(o) == 1L),
  distance = list(
    words = c("distance", "distances", "distance matrix", "dist", "dissimilarity"),
    pred = function(o) inherits(o, "dist")),
  crosstab = list(
    words = c("contingency", "crosstab", "cross tab", "cross tabulation", "frequency table",
              "tabulation", "counts table"),
    pred = function(o) inherits(o, "table")),
  network = list(
    words = c("network", "graph", "igraph", "nodes", "edges", "social network"),
    pred = function(o) inherits(o, c("igraph", "network", "tbl_graph", "graphNEL"))),
  formula = list(
    words = c("formula", "formulae", "formulas", "equation"),
    pred = function(o) inherits(o, "formula")),
  environment = list(
    words = c("environment", "env", "namespace"),
    pred = function(o) is.environment(o)),
  empty = list(
    words = c("empty", "null", "nothing", "zero length", "blank"),
    pred = function(o) length(o) == 0L && !is.environment(o) && !is.function(o))
)

.TYPE_VOCAB <- local({
  v <- list()
  for (cat in names(.TYPE_CATS)) {
    for (w in unique(type_norm(c(cat, .TYPE_CATS[[cat]]$words))))
      v[[w]] <- union(v[[w]], cat)
  }
  v
})

type_object_info <- function(o) {
  cats <- names(.TYPE_CATS)[vapply(.TYPE_CATS, function(ct)
    isTRUE(tryCatch(ct$pred(o), error = function(e) FALSE)), logical(1))]
  list(formal = unique(type_norm(tryCatch(class(o), error = function(e) character(0)))),
       cats = cats)
}

# Looks a typed term up in the vocabulary: exact, plural-stripped, then fuzzy.
type_term_categories <- function(term) {
  cands <- unique(c(term, sub("(ies)$", "y", term), sub("es$", "", term), sub("s$", "", term)))
  for (cd in cands) if (!is.null(.TYPE_VOCAB[[cd]])) return(.TYPE_VOCAB[[cd]])
  if (nchar(term) >= 4) {
    hits <- tryCatch(agrep(term, names(.TYPE_VOCAB), max.distance = 0.2,
                           ignore.case = TRUE, value = TRUE), error = function(e) character(0))
    hits <- hits[abs(nchar(hits) - nchar(term)) <= 3]
    if (length(hits) > 0)
      return(unique(unlist(.TYPE_VOCAB[head(hits, 3)], use.names = FALSE)))
  }
  character(0)
}

# Filters object names by a type query such as "sf, data table, mappable".
# Terms separated by commas, "or", "and", "/" or "+" are alternatives.
# Returns list(matched = <names>, unrecognised = <terms nothing understood>).
filter_objects_by_type <- function(obj_names, query, env = .GlobalEnv) {
  q <- trimws(query %||% "")
  if (!nzchar(q)) return(list(matched = obj_names, unrecognised = character(0)))
  terms <- trimws(unlist(strsplit(tolower(q), "\\s*(,|;|\\+|\\bor\\b|\\band\\b|/)\\s*", perl = TRUE)))
  terms <- unique(type_norm(terms[nzchar(terms)]))
  if (length(terms) == 0) return(list(matched = obj_names, unrecognised = character(0)))

  info <- lapply(setNames(obj_names, obj_names), function(nm)
    tryCatch(type_object_info(get(nm, envir = env)),
             error = function(e) list(formal = character(0), cats = character(0))))

  keep <- setNames(rep(FALSE, length(obj_names)), obj_names)
  unrec <- character(0)
  for (t in terms) {
    tcats <- type_term_categories(t)
    hit <- vapply(info, function(i) {
      length(intersect(tcats, i$cats)) > 0 || t %in% i$formal ||
        (nchar(t) >= 4 && any(grepl(t, i$formal, fixed = TRUE)))
    }, logical(1))
    if (length(tcats) == 0 && !any(hit)) unrec <- c(unrec, t)
    keep <- keep | hit
  }
  list(matched = obj_names[keep], unrecognised = unrec)
}

schema_from_r_object <- function(obj) {
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

extract_file_schema <- function(path) {
  if (!file.exists(path)) return(NULL)
  ext <- tolower(tools::file_ext(path))
  tryCatch({
    switch(ext,
      csv = {
        df   <- utils::read.csv(path, nrow = 0, check.names = FALSE)
        cols <- names(df)
        list(type = "CSV", cols = cols, suspicious = detect_suspicious_headers(cols))
      },
      tsv = , txt = {
        df   <- utils::read.delim(path, nrow = 0, check.names = FALSE)
        cols <- names(df)
        list(type = "TSV", cols = cols, suspicious = detect_suspicious_headers(cols))
      },
      xlsx = , xls = {
        if (!requireNamespace("readxl", quietly = TRUE)) return(NULL)
        df   <- readxl::read_excel(path, n_max = 0)
        cols <- names(df)
        list(type = "Excel", cols = cols, suspicious = detect_suspicious_headers(cols))
      },
      rds = {
        schema_from_r_object(readRDS(path))
      },
      rdata = , rda = {
        e    <- new.env(parent = emptyenv())
        nms  <- load(path, envir = e)
        if (length(nms) == 0) return(NULL)
        schemas <- Filter(Negate(is.null),
                          lapply(nms, function(nm)
                            schema_from_r_object(get(nm, envir = e))))
        if (length(schemas) == 0) return(NULL)
        if (length(schemas) == 1) return(schemas[[1]])
        list(type = "RData", multi = schemas, multi_names = nms)
      },
      shp = {
        # Read column names from the .dbf sidecar — no need to load geometries
        dbf <- sub("\\.shp$", ".dbf", path, ignore.case = TRUE)
        if (file.exists(dbf)) {
          df <- foreign::read.dbf(dbf, as.is = TRUE)
          list(type = "shapefile", cols = names(df))
        } else if (requireNamespace("sf", quietly = TRUE)) {
          schema_from_r_object(sf::st_read(path, quiet = TRUE))
        }
      },
      gpkg = , fgb = {
        if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
        tryCatch({
          lyr    <- sf::st_layers(path)$name[1]
          sf_obj <- sf::st_read(path, layer = lyr,
                                query = paste0('SELECT * FROM "', lyr, '" LIMIT 0'),
                                quiet = TRUE)
          schema_from_r_object(sf_obj)
        }, error = function(e) schema_from_r_object(sf::st_read(path, quiet = TRUE)))
      },
      geojson = , json = , kml = , gml = {
        if (!requireNamespace("sf", quietly = TRUE)) return(NULL)
        schema_from_r_object(sf::st_read(path, quiet = TRUE))
      },
      sav = {
        if (!requireNamespace("haven", quietly = TRUE)) return(NULL)
        df <- haven::read_sav(path, n_max = 0)
        list(type = "SPSS (.sav)", cols = names(df))
      },
      dta = {
        if (!requireNamespace("haven", quietly = TRUE)) return(NULL)
        df <- haven::read_dta(path, n_max = 0)
        list(type = "Stata (.dta)", cols = names(df))
      },
      sas7bdat = {
        if (!requireNamespace("haven", quietly = TRUE)) return(NULL)
        df <- haven::read_sas(path, n_max = 0)
        list(type = "SAS (.sas7bdat)", cols = names(df))
      },
      NULL
    )
  }, error = function(e) NULL)
}

format_schema_line <- function(label, schema, max_cols = 100) {
  if (is.null(schema)) return(paste0("- ", label))
  if (!is.null(schema$multi)) {
    lines <- mapply(function(s, nm) {
      format_schema_line(paste0(label, " [object: ", nm, "]"), s, max_cols)
    }, schema$multi, schema$multi_names, SIMPLIFY = TRUE)
    return(paste(lines, collapse = "\n"))
  }
  cols     <- schema$cols
  type_str <- schema$type %||% "file"
  nrow_str <- if (!is.null(schema$nrow))
    paste0(format(schema$nrow, big.mark = ","), " rows, ") else ""
  if (is.null(cols) || length(cols) == 0)
    return(paste0("- ", label, " [", type_str, "]"))
  n_cols   <- length(cols)
  omitted  <- max(0L, n_cols - max_cols)
  col_str  <- paste(cols[seq_len(min(n_cols, max_cols))], collapse = ", ")
  if (omitted > 0) col_str <- paste0(col_str, " ... and ", omitted, " more")
  line <- paste0("- ", label, " [", type_str, ", ", nrow_str, n_cols,
                 " column", if (n_cols != 1) "s" else "", ": ", col_str, "]")
  if (isTRUE(schema$suspicious))
    line <- paste0(line, " [WARNING: these may be data values, not column headers — file may lack a header row]")
  line
}

summarise_workspace_object <- function(obj_name, envir = .GlobalEnv) {
  if (!exists(obj_name, envir = envir, inherits = FALSE))
    return(paste0("`", obj_name, "`: [no longer exists in workspace]"))
  obj    <- get(obj_name, envir = envir, inherits = FALSE)
  schema <- schema_from_r_object(obj)
  type_str <- schema$type %||% paste(class(obj), collapse = "/")
  header <- if (!is.null(schema$nrow)) {
    paste0("`", obj_name, "` [", type_str, ", ",
           format(schema$nrow, big.mark = ","), " rows, ",
           length(schema$cols %||% character(0)),
           if (inherits(obj, "sf")) " columns plus a geometry column]" else " columns]")
  } else {
    paste0("`", obj_name, "` [", type_str, "]")
  }
  if (!is.null(schema$cols) && length(schema$cols) > 0) {
    n       <- length(schema$cols)
    shown   <- min(n, 50L)
    col_str <- paste(schema$cols[seq_len(shown)], collapse = ", ")
    if (n > shown) col_str <- paste0(col_str, " ... and ", n - shown, " more")
    paste0(header, "\n  columns: ", col_str)
  } else {
    header
  }
}

scrub_console_output <- function(text) {
  if (!nzchar(trimws(text))) return(text)
  lines <- strsplit(text, "\n", fixed = TRUE)[[1]]
  # Data frame printed rows: line starts with optional whitespace, an integer row
  # index, then more whitespace and a non-whitespace value.
  # Exclude lines like "1 warning message:" which begin with an integer but are not data.
  is_df_row <- grepl("^\\s*\\d+\\s+\\S", lines) &
               !grepl("^\\s*\\d+\\s+(warning|error|message)s?\\b", lines, ignore.case = TRUE)
  # str() output lines that show actual values: " $ varname: type val ..."
  is_str_row <- grepl("^\\s+\\$\\s+\\S+\\s*:", lines)

  result <- character(0)
  i <- 1L
  while (i <= length(lines)) {
    if (is_str_row[i]) {
      # Find the run of str() lines and suppress the whole block
      j <- i
      while (j <= length(lines) && is_str_row[j]) j <- j + 1L
      n <- j - i
      result <- c(result, paste0("[", n, " variable", if (n != 1L) "s" else "",
                                 " — values not shown]"))
      i <- j
    } else if (is_df_row[i]) {
      # Find the run of data rows; suppress only if 2+ consecutive
      j <- i
      while (j <= length(lines) && is_df_row[j]) j <- j + 1L
      n <- j - i
      if (n >= 2L) {
        result <- c(result, paste0("[", n, " rows of data — values not shown]"))
      } else {
        result <- c(result, lines[i])
      }
      i <- j
    } else {
      result <- c(result, lines[i])
      i <- i + 1L
    }
  }
  paste(result, collapse = "\n")
}

detect_suspicious_headers <- function(cols) {
  if (length(cols) == 0L) return(FALSE)
  n_numeric <- sum(grepl("^-?[0-9]+\\.?[0-9]*$", cols))
  n_date    <- sum(grepl("^\\d{1,2}[/\\-]\\d{1,2}[/\\-]\\d{2,4}$", cols) |
                   grepl("^\\d{4}[/\\-]\\d{2}[/\\-]\\d{2}$", cols))
  n_long    <- sum(nchar(cols) > 40L)
  (n_numeric / length(cols) > 0.5) || (n_date > 0L) || (n_long > 1L)
}

build_user_message <- function(file_paths, object_names = character(0), current_code,
                                last_known_code, run_history, user_prompt,
                                last_console_output = character(0),
                                object_envir = .GlobalEnv,
                                file_schemas = list()) {
  if (is.null(current_code))    current_code    <- ""
  if (is.null(last_known_code)) last_known_code <- ""
  pieces <- character(0)
  already_run_block <- format_already_run_block(run_history)
  if (nzchar(already_run_block)) pieces <- c(pieces, already_run_block)
  if (length(file_paths) > 0) {
    schema_lines <- vapply(file_paths, function(p) {
      format_schema_line(p, file_schemas[[p]])
    }, character(1))
    pieces <- c(pieces, paste0(
      "The following files are available on disk and may be relevant:\n",
      paste(schema_lines, collapse = "\n")
    ))
  }
  if (length(object_names) > 0) {
    summaries <- vapply(object_names, summarise_workspace_object, character(1),
                        envir = object_envir)
    pieces <- c(pieces, paste0(
      "The following R objects from the current workspace may be relevant:\n\n",
      paste(summaries, collapse = "\n\n")
    ))
  }
  if (nzchar(trimws(current_code)) && !identical(trimws(current_code), trimws(last_known_code))) {
    pieces <- c(pieces, paste0(
      "Here is the current code in my editor (I may have edited it since your last reply):\n```r\n",
      current_code, "\n```"
    ))
  }
  # Include the last R console output so Claude can see what the code produced.
  # Data values (printed table rows, str() variable lines) are scrubbed first
  # so that individual records never leave the user's machine.
  console_text <- scrub_console_output(paste(last_console_output, collapse = "\n"))
  if (nzchar(trimws(console_text))) {
    pieces <- c(pieces, paste0(
      "The R console output from the most recent code run was:\n```\n",
      console_text, "\n```"
    ))
  }
  pieces <- c(pieces, user_prompt)
  paste(pieces, collapse = "\n\n")
}

split_response_into_text_and_code <- function(raw_response) {
  pattern     <- "```(?:r|R)?[ \t]*\n([\\s\\S]*?)```"
  full_matches <- regmatches(raw_response, gregexpr(pattern, raw_response, perl = TRUE))[[1]]
  # Extract DESCRIPTION line if present
  desc <- ""
  desc_match <- regmatches(raw_response,
    regexpr("(?m)^DESCRIPTION:[ \t]*(.+)$", raw_response, perl = TRUE))
  if (length(desc_match) > 0 && nzchar(desc_match)) {
    desc <- trimws(sub("^DESCRIPTION:[ \t]*", "", desc_match, perl = TRUE))
    raw_response <- sub(paste0(desc_match, "\n?"), "", raw_response, fixed = TRUE)
  }

  if (length(full_matches) == 0 || identical(full_matches, ""))
    return(list(explanation = trimws(raw_response), code = "", description = desc))
  code_blocks <- vapply(full_matches, function(block) {
    trimws(sub("^```(?:r|R)?[ \t]*\n", "", sub("```$", "", block), perl = TRUE))
  }, character(1))
  remainder <- raw_response
  for (block in full_matches) remainder <- sub(block, "", remainder, fixed = TRUE)
  list(explanation = trimws(remainder), code = paste(code_blocks, collapse = "\n\n"),
       description = desc)
}

format_prompt_as_comment <- function(prompt_text) {
  if (is.null(prompt_text) || !nzchar(trimws(prompt_text)))
    return("# (no prompt recorded - code entered/edited manually)")
  lines <- strsplit(prompt_text, "\n", fixed = TRUE)[[1]]
  paste0("# ", lines, collapse = "\n")
}

format_log_entries <- function(entries) {
  header <- paste0(
    "---\ntitle: \"Classmate Code Log\"\ndate: \"",
    format(Sys.time(), "%Y-%m-%d %H:%M"), "\"\noutput: html_notebook\n---\n\n",
    "> **Note:** Please close Classmate (using Pause or Quit with Code Log) before running code in this notebook.",
    " Running code here while the app is active may cause conflicts."
  )
  chunks <- lapply(seq_along(entries), function(i) {
    e <- entries[[i]]
    prompt_text <- if (is.null(e$prompt) || !nzchar(trimws(e$prompt)))
      "_No prompt recorded — code entered or edited manually._"
    else
      paste0("*", trimws(e$prompt), "*")
    paste0(prompt_text, "\n\n```{r chunk_", i, "}\n", trimws(e$code), "\n```")
  })
  paste(c(header, chunks), collapse = "\n\n")
}

# "ok" (the API accepted the request), "invalid" (authentication/permission refused: 401/403) or
# "unknown" (network trouble, overload, rate limit...). Success is the API accepting the request,
# not any particular reply text: with a tiny max_tokens the 5.x models return an empty reply.
claude_key_status <- function(key = classmate:::cm_get_key()) {
  tryCatch({
    call_claude(
      messages     = list(list(role = "user", content = "Hi")),
      model        = MODEL_HAIKU,
      max_tokens   = 8,
      api_key      = key,
      cache_system = FALSE
    )
    "ok"
  },
  httr2_http_401 = function(e) "invalid",
  httr2_http_403 = function(e) "invalid",
  error          = function(e) "unknown")
}

validate_api_key <- function(key) {
  ok <- identical(claude_key_status(key), "ok")
  if (ok) classmate:::cm_set_key(key)    # held privately, not in the ANTHROPIC_API_KEY variable
  ok
}

build_volumes <- function() shinyFiles::getVolumes()()

# Roots offered by the file dialogs; "Working directory" always points at the
# current project root, even after it has been changed.
file_dialog_roots <- function(volumes) c("Working directory" = project_root(), volumes)

open_log_file <- function(log_path, file_edit_fn = file.edit) {
  if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::navigateToFile(log_path)
  } else {
    file_edit_fn(log_path)
  }
}

strip_ansi <- function(x) gsub("\033\\[[0-9;]*m", "", x, perl = TRUE)

# Count non-blank, non-comment lines of R code
count_code_lines <- function(code) {
  lines <- strsplit(code, "\n", fixed = TRUE)[[1]]
  sum(nzchar(trimws(grep("^\\s*#", lines, invert = TRUE, value = TRUE))))
}

# Return TRUE if code exceeds max_lines by more than 20% (Inf = unlimited)
code_too_long <- function(code, max_lines) {
  if (is.infinite(max_lines)) return(FALSE)
  count_code_lines(code) > floor(max_lines * 1.2)
}

# LCS-based diff: matches new lines forward through old lines, extracts just the
# old section that corresponds to the new code, and identifies changed lines in both
# panels. Returns NULL when there is no difference or the change is too substantial (< 60% of lines unchanged).
compute_diff <- function(old_code, new_code, max_changed_frac = 0.4) {
  old_lines <- strsplit(old_code, "\n", fixed = TRUE)[[1]]
  new_lines <- strsplit(new_code, "\n", fixed = TRUE)[[1]]
  # Comments are ignored: a trailing comment is dropped (only when the line has no quotes, so a
  # "#" inside a string is safe) and comment-only lines are treated as blank.
  strip_trailing_op <- function(x) {
    x <- trimws(x)
    x <- ifelse(grepl("[\"']", x), x, sub("\\s*#.*$", "", x))
    x[startsWith(x, "#")] <- ""
    trimws(sub("[+,|&]\\s*$", "", x))
  }
  old_norm  <- strip_trailing_op(old_lines)
  new_norm  <- strip_trailing_op(new_lines)
  n_old <- length(old_lines)
  n_new <- length(new_lines)

  # Longest-common-subsequence match of non-blank lines (robust to inserted/removed lines)
  ni <- which(nzchar(new_norm)); oi <- which(nzchar(old_norm))
  new_to_old <- rep(NA_integer_, n_new)
  if (length(ni) > 0 && length(oi) > 0) {
    L <- matrix(0L, length(ni) + 1L, length(oi) + 1L)
    for (a in rev(seq_along(ni))) for (b in rev(seq_along(oi)))
      L[a, b] <- if (identical(new_norm[ni[a]], old_norm[oi[b]])) L[a + 1L, b + 1L] + 1L
                 else max(L[a + 1L, b], L[a, b + 1L])
    a <- 1L; b <- 1L
    while (a <= length(ni) && b <= length(oi)) {
      if (identical(new_norm[ni[a]], old_norm[oi[b]])) {
        new_to_old[ni[a]] <- oi[b]; a <- a + 1L; b <- b + 1L
      } else if (L[a + 1L, b] >= L[a, b + 1L]) a <- a + 1L
      else b <- b + 1L
    }
  }

  # Changed new lines = non-blank lines with no match in old
  changed_new <- which(is.na(new_to_old) & nzchar(new_norm))
  n_changed   <- length(changed_new)
  if (n_changed == 0) return(NULL)
  if (n_changed / max(length(ni), 1L) > max_changed_frac) return(NULL)   # default: need >= 60% of lines unchanged

  matched_old <- sort(unique(new_to_old[!is.na(new_to_old)]))
  if (length(matched_old) < 1) return(NULL)   # codes too different to be comparable

  # Extract the old section that spans the matching region
  old_start   <- min(matched_old)
  old_end     <- max(matched_old)
  old_section <- old_lines[old_start:old_end]
  n_old_sec   <- length(old_section)

  # Find which old-section lines are "replaced" by the changed new lines
  old_changed <- integer(0)
  for (ci in changed_new) {
    prev_old <- NA_integer_
    if (ci > 1L) for (k in (ci - 1L):1L) {
      if (!is.na(new_to_old[k])) { prev_old <- new_to_old[k]; break }
    }
    next_old <- NA_integer_
    if (ci < n_new) for (k in (ci + 1L):n_new) {
      if (!is.na(new_to_old[k])) { next_old <- new_to_old[k]; break }
    }
    gap_start <- if (is.na(prev_old)) old_start else prev_old + 1L
    gap_end   <- if (is.na(next_old)) old_end   else next_old - 1L
    if (gap_start <= gap_end) {
      rel <- seq(gap_start, gap_end) - old_start + 1L
      old_changed <- c(old_changed, rel[rel >= 1L & rel <= n_old_sec])
    }
  }

  # Comment-only and blank lines on the old side are not "changed" lines (comments are ignored)
  old_changed <- old_changed[nzchar(old_norm[old_start + old_changed - 1L])]

  list(
    old_lines   = old_section,
    new_lines   = new_lines,
    changed     = changed_new,
    old_changed = sort(unique(old_changed))
  )
}

# Extract library()/require() calls from code, with optional # github: hints.
# Returns a list of list(pkg, github) where github is NA if not present.
extract_pkg_info <- function(code) {
  lines  <- strsplit(code, "\n", fixed = TRUE)[[1]]
  results <- list()
  seen   <- character(0)
  for (i in seq_along(lines)) {
    m <- regexec(
      '(?:library|require)\\s*\\(\\s*(?:package\\s*=\\s*)?["\']?([A-Za-z][A-Za-z0-9._]*)["\']?',
      lines[[i]], perl = TRUE)
    rm_out <- regmatches(lines[[i]], m)[[1]]
    if (length(rm_out) < 2) next
    pkg <- rm_out[[2]]
    if (pkg %in% seen) next
    seen <- c(seen, pkg)
    github_ref <- NA_character_
    if (i > 1) {
      prev <- trimws(lines[[i - 1]])
      gm  <- regexec('#\\s*github:\\s*([A-Za-z0-9._/-]+)', prev, perl = TRUE)
      grm <- regmatches(prev, gm)[[1]]
      if (length(grm) >= 2) github_ref <- grm[[2]]
    }
    results[[length(results) + 1]] <- list(pkg = pkg, github = github_ref)
  }
  results
}

# Returns repos to use for install.packages(), respecting any user setting.
get_cran_repos <- function() {
  repos <- getOption("repos")
  if (!is.null(repos) && length(repos) > 0 && !all(unname(repos) == "@CRAN@")) {
    if ("CRAN" %in% names(repos) && identical(unname(repos["CRAN"]), "@CRAN@"))
      repos["CRAN"] <- "https://cloud.r-project.org"
    return(repos)
  }
  c(CRAN = "https://cloud.r-project.org")
}

# Launch a background callr process to install missing packages.
start_pkg_install <- function(cran_pkgs, github_refs, repos) {
  callr::r_bg(
    func = function(cran_pkgs, github_refs, repos) {
      if (length(cran_pkgs) > 0)
        install.packages(cran_pkgs, repos = repos, quiet = FALSE)
      if (length(github_refs) > 0) {
        if (!requireNamespace("remotes", quietly = TRUE))
          install.packages("remotes", repos = repos, quiet = FALSE)
        for (ref in github_refs)
          remotes::install_github(ref, quiet = FALSE)
      }
    },
    args = list(cran_pkgs = cran_pkgs, github_refs = github_refs, repos = repos)
  )
}

# For each plot shown in the R output tab (keyed by the path of its PNG): the lines of code that
# drew it, found while the code ran (see run_code()). Used to write the "save this plot" code.
plot_code_cache <- new.env(parent = emptyenv())

# Width, height and units of a "Target size" choice from Preferences, e.g.
# "Journal double col. (180x120 mm)" -> list(w = 180, h = 120, units = "mm")
plot_export_size <- function(label) {
  m <- regmatches(label, regexec("([0-9.]+)x([0-9.]+) *(mm|in|cm)", label))[[1]]
  if (length(m) < 4) return(list(w = 7, h = 5, units = "in"))
  list(w = as.numeric(m[2]), h = as.numeric(m[3]), units = if (m[4] == "cm") "cm" else m[4])
}
plot_export_dpi <- function(quality) switch(quality, Low = 96, Medium = 150, High = 300, 150)

# A free name for a plot object: map (tmap), p (ggplot2) or fig, with a number added if it is taken
tidy_plot_name <- function(cls, code) {
  base_nm <- if ("tmap" %in% cls) "map" else if (any(c("gg", "ggplot") %in% cls)) "p" else "fig"
  used <- c(ls(envir = globalenv()),
            tryCatch(all.names(parse(text = code, keep.source = FALSE)), error = function(e) character(0)))
  nm <- base_nm; k <- 1L
  while (nm %in% used) { k <- k + 1L; nm <- paste0(base_nm, k) }
  nm
}

# The R code that saves a plot: open the device with the chosen options, draw, close the device.
# `snippet` is the code that draws the plot; `file` is relative to the working directory.
build_plot_export_code <- function(snippet, file, format, quality, size) {
  sz  <- plot_export_size(size); dpi <- plot_export_dpi(quality)
  num <- function(x) format(round(x, 2), trim = TRUE, nsmall = 0)
  q   <- deparse(file)
  dims <- sprintf("width = %s, height = %s, units = \"%s\"", num(sz$w), num(sz$h), sz$units)
  jq   <- switch(quality, Low = 70, Medium = 85, High = 95, 85)
  to_in <- function(x) x / switch(sz$units, mm = 25.4, cm = 2.54, `in` = 1)
  dev_call <- switch(format,
    png = sprintf("png(%s, %s, res = %d)", q, dims, dpi),
    jpg = sprintf("jpeg(%s, %s, res = %d, quality = %d)", q, dims, dpi, jq),
    tif = sprintf("tiff(%s, %s, res = %d, compression = \"lzw\")", q, dims, dpi),
    bmp = sprintf("bmp(%s, %s, res = %d)", q, dims, dpi),
    pdf = sprintf("pdf(%s, width = %s, height = %s)", q, num(to_in(sz$w)), num(to_in(sz$h))),
    stop("Unknown format"))
  what <- if (identical(format, "pdf"))
    sprintf("pdf, %s x %s in", num(to_in(sz$w)), num(to_in(sz$h)))
  else sprintf("%s, %d dpi, %s x %s %s", format, dpi, num(sz$w), num(sz$h), sz$units)
  paste(c(sprintf("# Save the plot to a file (%s)", what), dev_call,
          trimws(snippet, which = "right"), "invisible(dev.off())"), collapse = "\n")
}

run_code <- function(code_text) {
  warnings_seen  <- character(0)
  console_output <- character(0)
  plot_files     <- character(0)

  # Run in the user's project directory, not the Shiny app directory
  .enter <- cm_enter_root()
  on.exit(.enter$restore(), add = TRUE)
  if (!.enter$ok)
    return(list(success = FALSE, warnings = character(0), output = character(0),
                plot_files = character(0), wd_changed = NULL,
                message = paste("Classmate could not open your working directory.",
                                "Use Change Working Directory in the Quick Console to choose a folder.")))

  # Open a numbered PNG device to capture any plots generated
  tmp_base    <- tempfile("classmate_plot_")
  tmp_pattern <- paste0(tmp_base, "%03d.png")
  grDevices::png(tmp_pattern, width = 900, height = 675, res = 120, bg = "white")
  plot_dev <- grDevices::dev.cur()

  # Note which top-level statement starts each new plot page (base, grid, ggplot2, tmap...), so that
  # the code that drew a plot can be found later (see plot_code_cache). The code is evaluated one
  # statement at a time, as source(print.eval = TRUE) would.
  grDevices::dev.control("enable")     # keep the display list, to see which statements draw
  exprs <- NULL; cur_expr <- 0L; starts <- integer(0); drew <- integer(0); n_prev <- 0L
  vis_cls <- list()    # statement number -> class of the value it printed, when it printed one
  note_page <- function(...) starts <<- c(starts, cur_expr)
  hook_names <- c("before.plot.new", "before.grid.newpage")
  old_hooks  <- lapply(hook_names, getHook)
  for (h in hook_names) setHook(h, note_page, "append")
  on.exit({
    for (i in seq_along(hook_names)) setHook(hook_names[i], old_hooks[[i]], "replace")
  }, add = TRUE)

  result <- tryCatch({
    console_output <- capture.output({
      withCallingHandlers({
        exprs <- parse(text = code_text, keep.source = TRUE)
        for (i in seq_along(exprs)) {
          cur_expr <- i
          yy <- withVisible(eval(exprs[[i]], envir = globalenv()))
          if (yy$visible && !is.null(yy$value)) vis_cls[[as.character(i)]] <- class(yy$value)
          if (yy$visible) {
            if (isS4(yy$value)) methods::show(yy$value) else print(yy$value)
          }
          # Did this statement draw anything? (a new page, or a change to the current page)
          n_now <- tryCatch(length(grDevices::recordPlot()[[1]]), error = function(e) n_prev)
          if (n_now != n_prev || i %in% starts) drew <- c(drew, i)
          n_prev <- n_now
        }
      }, warning = function(w) {
        warnings_seen <<- c(warnings_seen, strip_ansi(conditionMessage(w)))
        invokeRestart("muffleWarning")
      })
    })
    list(success = TRUE, message = "Code ran successfully.",
         warnings = warnings_seen, output = strip_ansi(console_output))
  }, error = function(e) {
    list(success = FALSE,
         message  = paste("Error running code:\n", strip_ansi(conditionMessage(e))),
         warnings = warnings_seen, output = strip_ansi(console_output))
  })

  # Close device and collect any non-blank plot files (blank PNG ≈ < 8 KB)
  tryCatch(grDevices::dev.off(plot_dev), error = function(e) invisible(NULL))
  candidates <- sort(Sys.glob(paste0(tmp_base, "*.png")))
  keep       <- file.exists(candidates) & file.size(candidates) > 8192
  plot_files <- candidates[keep]
  # Work out the lines of code behind each plot page. Only when the statements line up one-to-one
  # with the pages written, and each page began in a different statement; otherwise no code is offered.
  if (isTRUE(result$success) && !is.null(exprs) && length(starts) == length(candidates) &&
      length(starts) > 0 && !anyDuplicated(starts)) {
    refs  <- attr(exprs, "srcref")
    lines <- strsplit(code_text, "\n", fixed = TRUE)[[1]]
    if (!is.null(refs)) for (i in which(keep)) {
      span_end <- if (i < length(starts)) starts[i + 1L] - 1L else length(exprs)
      in_span  <- drew[drew >= starts[i] & drew <= span_end]
      last_expr <- if (length(in_span) > 0) max(in_span) else starts[i]   # drop trailing non-drawing statements
      from <- refs[[starts[i]]][1L]; to <- refs[[last_expr]][3L]
      # What kind of statement drew it? "value": one statement whose printed value is the plot (a ggplot
      # or tmap object typed without assigning it); "symbol": just an object's name; "plain": anything
      # else (plot(), lines(), print(p) ... which draw as a side effect and are repeated as they are).
      single <- identical(last_expr, starts[i])
      cls    <- vis_cls[[as.character(starts[i])]]
      kind   <- if (single && !is.null(cls) && is.name(exprs[[starts[i]]])) "symbol"
                else if (single && !is.null(cls)) "value" else "plain"
      assign(candidates[i], list(code = paste(lines[from:to], collapse = "\n"), kind = kind,
                                 cls = cls %||% character(0),
                                 sym = if (identical(kind, "symbol")) as.character(exprs[[starts[i]]]) else ""),
             envir = plot_code_cache)
    }
  }

  # If the code changed the working directory (setwd()), make that the new root
  wd_changed <- NULL
  now_wd <- tryCatch(normalizePath(getwd(), winslash = "/"), error = function(e) NA_character_)
  if (!is.na(now_wd) && !identical(now_wd, project_root())) {
    r <- cm_set_root(now_wd)
    if (isTRUE(r$ok) && isTRUE(r$changed)) wd_changed <- r$path
  }

  c(result, list(plot_files = plot_files, wd_changed = wd_changed))
}

build_fix_system_prompt <- function(language = "English") {
  paste(
    "You are an expert R programming assistant helping a student fix broken R code.",
    "The student will give you their current code and the error message it produced.",
    "Your task:",
    "1. Diagnose the root cause of the error.",
    "2. Produce corrected R code that fixes the problem.",
    "   - Amend the existing code where possible; do not rewrite from scratch unless",
    "     the existing code is fundamentally flawed.",
    "   - Preserve all parts of the code that are not related to the error.",
    "3. Reply in EXACTLY this format and nothing else:",
    "",
    "EXPLANATION: <one or two sentences describing what was wrong and what you changed>",
    "",
    "```r",
    "<corrected R code>",
    "```",
    "",
    "Do not include any other text, commentary, or markdown outside this format.",
    "The EXPLANATION must be concise — suitable for displaying in a small pop-up window.",
    "",
    build_language_clause(language)
  )
}

# Builds the system prompt for the Explain feature.
build_output_explain_system_prompt <- function(language = "English") {
  paste(
    "You are a clear and helpful R programming tutor explaining what R output means.",
    "The student has run R code and you are shown the resulting output — which may",
    "include printed tables, statistical summaries, model results, or one or more plots.",
    "You may also be given the student's question and the code as background context.",
    "Use that context to ground your interpretation, but do not describe or summarise",
    "the code or question — your explanation must focus on the output itself.",
    "Your job is genuine interpretation: describe what is being shown, explain what it",
    "means, and help the student understand how to read and interpret it.",
    "Stick strictly to what the output actually shows — do not speculate, invent",
    "explanations not supported by the evidence, or draw conclusions beyond what the",
    "data and output can reasonably support. If something is ambiguous or would require",
    "more context to interpret confidently, say so rather than guessing.",
    "Always structure your response in EXACTLY this format:",
    "",
    "EXPLANATION:",
    "<your explanation here>",
    "",
    "HELP_PAGES:",
    "<one function per line: package::function — one-line description>",
    "",
    "List 2–5 HELP_PAGES entries for functions most relevant to interpreting or",
    "extending this output. No bullet points — one entry per line.",
    "",
    build_language_clause(language),
    sep = "\n"
  )
}

build_output_explain_content <- function(text_output, plot_files, level,
                                         code = "", prompt = "") {
  level_instruction <- switch(level,
    "Beginner" = paste(
      "Explain this R output to a complete beginner. Use plain, jargon-free language.",
      "Focus on what the numbers or chart mean in everyday terms — what story does",
      "the output tell? Avoid statistical terminology where possible; where you must",
      "use it, briefly say what it means. Keep it short and encouraging. Max 200 words."
    ),
    "Intermediate" = paste(
      "Explain this R output to a student who knows some R but is still learning",
      "statistics and data analysis. Describe what the key figures or patterns mean,",
      "name the relevant statistical concepts, and note anything particularly important",
      "or surprising. Max 280 words."
    ),
    "Advanced" = paste(
      "Explain this R output to a student comfortable with R and statistics.",
      "Interpret the key values in detail, comment on what they imply for the analysis,",
      "flag any caveats or assumptions, and suggest what the student might look at next.",
      "Max 380 words."
    ),
    "Intermediate"
  )
  # Build context preamble (code + prompt) if available — for background only
  context_text <- ""
  has_prompt <- nzchar(trimws(prompt))
  has_code   <- nzchar(trimws(code))
  if (has_prompt || has_code) {
    context_text <- paste0(
      "For context only (do not describe or summarise this — use it solely to help",
      " interpret the output below):\n"
    )
    if (has_prompt)
      context_text <- paste0(context_text, "Student's question: ", trimws(prompt), "\n")
    if (has_code)
      context_text <- paste0(context_text, "Code that was run:\n```r\n", trimws(code), "\n```\n")
    context_text <- paste0(context_text, "\n")
  }
  intro_text <- paste0(context_text, level_instruction, "\n\n")
  if (nzchar(trimws(text_output)))
    intro_text <- paste0(intro_text, "Console output:\n\n", text_output)
  if (length(plot_files) > 0)
    intro_text <- paste0(intro_text,
      if (nzchar(trimws(text_output))) "\n\nThe code also generated these plot(s):"
      else "The code generated these plot(s):")
  content <- list(list(type = "text", text = intro_text))
  for (f in plot_files) {
    raw_data <- readBin(f, "raw", file.size(f))
    b64      <- gsub("\\s+", "", jsonlite::base64_enc(raw_data))
    content  <- c(content, list(list(
      type   = "image",
      source = list(type = "base64", media_type = "image/png", data = b64)
    )))
  }
  content
}

build_error_explain_system_prompt <- function(language = "English") {
  paste(
    "You are a friendly R programming tutor helping a student understand why their",
    "code produced errors or warnings.",
    "Explain what went wrong in plain, simple language. Avoid technical jargon where",
    "possible; where you must use a technical term, briefly say what it means.",
    "Be concise and reassuring — 2 to 5 sentences per distinct issue.",
    "Do not provide corrected code (that is handled separately by the Fix button).",
    "Focus only on helping the student understand WHAT went wrong and WHY.",
    "Always respond in EXACTLY this format:",
    "",
    "EXPLANATION:",
    "<your plain-language explanation here>",
    "",
    build_language_clause(language),
    sep = "\n"
  )
}

build_diff_explain_system_prompt <- function(language = "English") {
  paste(
    "You are a clear and patient R programming tutor.",
    "The student has just had their code modified by an AI assistant and is viewing",
    "the changes. Your job is to explain exactly what was changed in the code,",
    "and — crucially — WHY each change was made: what problem it solves, what",
    "improvement it achieves, or what the student's request required.",
    "Focus entirely on the differences. Do not re-explain unchanged parts of the code.",
    "Be specific: name the functions or arguments that were added, removed, or altered.",
    "Keep it concise and educational — the student should understand both WHAT changed",
    "and WHY, so they learn from the modification.",
    "Always respond in EXACTLY this format:",
    "",
    "EXPLANATION:",
    "<your explanation of what changed and why>",
    "",
    "HELP_PAGES:",
    "<one function per line, in the form: package::function — one-line description>",
    "",
    "The HELP_PAGES section must list only real R functions used in the changes.",
    "List between 1 and 5 entries. No bullet points or numbering.",
    "",
    build_language_clause(language),
    sep = "\n"
  )
}

build_explain_system_prompt <- function(language = "English") {
  paste(
    "You are a patient and clear R programming tutor helping students learn R.",
    "When given R code and an explanation level, you explain the code clearly",
    "and concisely, then list the most relevant R help pages.",
    "Always structure your response in EXACTLY this format, with no deviations:",
    "",
    "EXPLANATION:",
    "<your explanation here>",
    "",
    "HELP_PAGES:",
    "<one function per line, in the form: package::function — one-line description>",
    "",
    "The HELP_PAGES section must list only real R functions the student can look up",
    "with ?function or ?package::function. List between 2 and 6 entries. No bullet",
    "points or numbering — one entry per line only.",
    "",
    build_language_clause(language),
    sep = "\n"
  )
}

# Builds the user message for the Explain feature.
build_explain_prompt <- function(code_text, level) {
  level_instruction <- switch(level,
    "Beginner" = paste(
      "Explain this R code to a complete beginner who is new to programming.",
      "Focus on the big picture: what the code does overall and why, using simple",
      "plain-English analogies where helpful. Avoid jargon. Do not go into detail",
      "about individual parameters or options — just explain what each main step",
      "achieves and how the pieces fit together. Keep it concise and encouraging.",
      "Maximum 200 words for the explanation."
    ),
    "Intermediate" = paste(
      "Explain this R code to a student who knows the basics of R but is still",
      "learning. Describe what each main section does and why, introducing the key",
      "functions by name and briefly saying what they do. You may mention one or two",
      "notable argument choices (e.g. why a particular value was used), but keep it",
      "accessible. Aim for clarity over completeness. Maximum 280 words."
    ),
    "Advanced" = paste(
      "Explain this R code to a student who is comfortable with R and wants to",
      "understand the detail. Go through the key functions and their important",
      "arguments, explain WHY specific parameter values were chosen, and note any",
      "non-obvious design decisions or potential gotchas. Where relevant, mention",
      "alternative approaches and when you might prefer them. Still be concise —",
      "focus on what is genuinely interesting or instructive rather than narrating",
      "every line. Maximum 380 words. Suggest ?function references for things worth",
      "exploring further."
    ),
    "Intermediate"
  )
  paste0(
    level_instruction, "\n\n",
    "Here is the R code to explain:\n```r\n", code_text, "\n```"
  )
}

# Parses Claude's explanation response into (explanation, help_pages vector).
parse_explain_response <- function(raw) {
  expl_match <- regmatches(raw, regexpr("(?s)EXPLANATION:\\s*\n(.+?)(?=\nHELP_PAGES:|$)",
                                         raw, perl = TRUE))
  help_match  <- regmatches(raw, regexpr("(?s)HELP_PAGES:\\s*\n(.+)$", raw, perl = TRUE))

  explanation <- if (length(expl_match) > 0 && nzchar(expl_match)) {
    trimws(sub("EXPLANATION:\\s*\n", "", expl_match, perl = TRUE))
  } else trimws(raw)

  help_lines <- if (length(help_match) > 0 && nzchar(help_match)) {
    raw_lines <- trimws(sub("HELP_PAGES:\\s*\n", "", help_match, perl = TRUE))
    lines <- strsplit(raw_lines, "\n")[[1]]
    lines <- trimws(lines)
    lines[nzchar(lines)]
  } else character(0)

  list(explanation = explanation, help_pages = help_lines)
}

# Converts explanation plain text to simple HTML (newlines -> <br>, **bold**).
# Explanations and "Tell me more" text are formatted by answer_to_html(): it escapes the text
# and handles paragraphs, lists, **bold**, `code` and ``` fenced code blocks. (The earlier,
# simpler version paired up the backticks of a fenced block wrongly, so prose appeared as code
# and code as prose.)
explanation_to_html <- function(text) answer_to_html(text)

# --- Usage tracking helpers --------------------------------------------------

# USD per million tokens (platform pricing page): input, 5-minute cache write, cache read, output
MODEL_PRICES <- list(
  "claude-sonnet-5-5" = c(input = 2.00, cache_write = 2.50,  cache_read = 0.10, output = 10.00),
  "claude-haiku-5-5"  = c(input = 0.10, cache_write = 0.125, cache_read = 0.01, output = 0.50)
)

calc_cost <- function(usage, model = MODEL_SONNET) {
  pr <- MODEL_PRICES[[model]] %||% MODEL_PRICES[["claude-sonnet-5-5"]]   # unknown model: use the Sonnet prices
  PRICE_INPUT <- pr[["input"]]; PRICE_CACHE_WRITE <- pr[["cache_write"]]
  PRICE_CACHE_READ <- pr[["cache_read"]]; PRICE_OUTPUT <- pr[["output"]]
  cache_write <- usage$cache_creation_input_tokens %||% 0L
  cache_read  <- usage$cache_read_input_tokens     %||% 0L
  # The API reports input_tokens EXCLUDING cached tokens (which are listed separately).
  regular_in  <- usage$input_tokens %||% 0L
  out         <- usage$output_tokens  %||% 0L
  (cache_write * PRICE_CACHE_WRITE +
   cache_read  * PRICE_CACHE_READ  +
   regular_in  * PRICE_INPUT       +
   out         * PRICE_OUTPUT) / 1e6
}

usage_data_dir <- function() {
  d <- tools::R_user_dir("classmate", "data")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  d
}

usage_log_path   <- function() classmate:::classmate_state_path("usage")
active_cfg_path  <- function() classmate:::classmate_state_path("config")

prefs_path <- function() {
  d <- tools::R_user_dir("classmate", "config")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.path(d, "user_prefs.rds")
}

load_saved_prefs <- function() {
  p <- prefs_path()
  if (!file.exists(p)) return(list())
  tryCatch(readRDS(p), error = function(e) list())
}

save_prefs <- function(prefs_list) {
  tryCatch(saveRDS(prefs_list, prefs_path()), error = function(e) NULL)
}

read_usage_log <- function() {
  p <- usage_log_path()
  if (!file.exists(p)) return(list())
  tryCatch(readRDS(p), error = function(e) list())
}

append_usage_entry <- function(cost_usd) {
  old_log <- read_usage_log()
  log <- c(old_log, list(list(timestamp = Sys.time(), cost_usd = cost_usd)))
  # Remember which key the usage belongs to, so that deleting and re-loading the same key keeps it
  attr(log, "key_id") <- tryCatch(readRDS(active_cfg_path())$key_id, error = function(e) NULL) %||%
    attr(old_log, "key_id")
  tryCatch(classmate:::classmate_save_state(log, usage_log_path()), error = function(e) invisible(NULL))
  log
}

period_cutoff <- function(reset_period) {
  now <- Sys.time()
  if (reset_period == "rolling_24h") return(now - 86400)
  if (reset_period == "hourly") {
    lt <- as.POSIXlt(now); lt$min <- 0L; lt$sec <- 0
    return(as.POSIXct(lt))
  }
  if (reset_period == "daily") {
    lt <- as.POSIXlt(now); lt$hour <- 0L; lt$min <- 0L; lt$sec <- 0
    return(as.POSIXct(lt))
  }
  if (reset_period == "weekly") {
    lt <- as.POSIXlt(now)
    lt$mday <- lt$mday - (lt$wday - 1L) %% 7L
    lt$hour <- 0L; lt$min <- 0L; lt$sec <- 0
    return(as.POSIXct(lt))
  }
  m <- regmatches(reset_period,
        regexpr("^rolling_([0-9]+)(h|m)$", reset_period, perl = TRUE))
  if (length(m) > 0) {
    n    <- as.integer(sub("^rolling_([0-9]+)(h|m)$", "\\1", m, perl = TRUE))
    unit <- sub("^rolling_([0-9]+)(h|m)$", "\\2", m, perl = TRUE)
    return(now - if (unit == "h") n * 3600 else n * 60)
  }
  now - 86400
}

period_spend <- function(log, reset_period) {
  if (length(log) == 0) return(0)
  cutoff <- period_cutoff(reset_period)
  recent <- Filter(function(e) e$timestamp >= cutoff, log)
  sum(vapply(recent, function(e) e$cost_usd, numeric(1)))
}

next_reset_time <- function(log, reset_period) {
  now <- Sys.time()
  if (reset_period == "hourly") {
    lt <- as.POSIXlt(now); lt$min <- 0L; lt$sec <- 0; lt$hour <- lt$hour + 1L
    return(as.POSIXct(lt))
  }
  if (reset_period == "daily") {
    lt <- as.POSIXlt(now); lt$hour <- 0L; lt$min <- 0L; lt$sec <- 0
    lt$mday <- lt$mday + 1L; return(as.POSIXct(lt))
  }
  if (reset_period == "weekly") {
    lt <- as.POSIXlt(now); lt$hour <- 0L; lt$min <- 0L; lt$sec <- 0
    lt$mday <- lt$mday + (8L - lt$wday) %% 7L + 1L; return(as.POSIXct(lt))
  }
  # Rolling periods: oldest entry in window ages out first
  cutoff <- period_cutoff(reset_period)
  recent <- Filter(function(e) e$timestamp >= cutoff, log)
  if (length(recent) == 0) return(now)
  period_secs <- as.numeric(now - cutoff, units = "secs")
  oldest_ts   <- min(vapply(recent, function(e) as.numeric(e$timestamp), numeric(1)))
  as.POSIXct(oldest_ts + period_secs, origin = "1970-01-01")
}

# --- Direct Anthropic API call with prompt caching ---------------------------

call_claude <- function(messages, model, max_tokens, system_prompt = NULL,
                         api_key = classmate:::cm_get_key(),
                         cache_system = TRUE) {
  system_block <- if (!is.null(system_prompt) && nzchar(system_prompt)) {
    cc <- if (cache_system) list(type = "ephemeral") else NULL
    list(Filter(Negate(is.null),
                list(type = "text", text = system_prompt, cache_control = cc)))
  } else NULL

  body <- c(list(model = model, max_tokens = max_tokens, messages = messages), model_extra_params(model))
  if (!is.null(system_block)) body$system <- system_block

  resp <- request("https://api.anthropic.com/v1/messages") |>
    req_headers(
      "x-api-key"         = api_key,
      "anthropic-version" = "2023-06-01",
      "content-type"      = "application/json"
    ) |>
    req_body_json(body) |>
    req_perform()

  result <- resp_body_json(resp)
  list(
    text        = first_text_block(result$content),
    stop_reason = result$stop_reason,
    cost_usd    = calc_cost(result$usage %||% list(), model)
  )
}


# --- UI ----------------------------------------------------------------------

ui <- fluidPage(
  useShinyjs(),

  # Capture Ace editor selection and explanation text selection
  tags$script(HTML("
    $(document).on('shiny:connected', function() {
      setTimeout(function() {
        var editor = ace.edit('code_editor');
        if (!editor) return;
        editor.on('changeSelection', function() {
          var sel = editor.getSelectedText().trim();
          Shiny.setInputValue('code_selection', sel);
        });
      }, 500);
    });

    // Detect text selection within the explanation text area
    document.addEventListener('selectionchange', function() {
      var sel = window.getSelection();
      var text = sel ? sel.toString().trim() : '';
      if (text && sel.anchorNode) {
        var node = sel.anchorNode.nodeType === 3
          ? sel.anchorNode.parentElement : sel.anchorNode;
        while (node) {
          if (node.id === 'explain_text_ui') {
            Shiny.setInputValue('explain_selection', text, {priority: 'event'});
            return;
          }
          node = node.parentElement;
        }
      }
      Shiny.setInputValue('explain_selection', '', {priority: 'event'});
    });

    // R expression completeness check for Quick Console smart-Enter
    function isCompleteR(code) {
      var depth_paren = 0, depth_brace = 0, depth_bracket = 0;
      var in_string = false, string_char = 0;
      for (var i = 0; i < code.length; i++) {
        var c = code[i], cc = code.charCodeAt(i);
        if (in_string) {
          if (c === '\\\\') { i++; continue; }
          if (cc === string_char) { in_string = false; }
          continue;
        }
        // 34 = double-quote, 39 = single-quote
        if (cc === 34 || cc === 39) { in_string = true; string_char = cc; continue; }
        if (c === '#') { while (i < code.length && code[i] !== '\\n') i++; continue; }
        if      (c === '(') depth_paren++;
        else if (c === ')') depth_paren--;
        else if (c === '{') depth_brace++;
        else if (c === '}') depth_brace--;
        else if (c === '[') depth_bracket++;
        else if (c === ']') depth_bracket--;
      }
      if (depth_paren !== 0 || depth_brace !== 0 || depth_bracket !== 0 || in_string) return false;
      // Strip comments, then check for trailing continuation operators
      var trimmed = code.replace(/#[^\\n]*/gm, '').replace(/\\s+$/, '');
      if (!trimmed) return false;
      if (/[+\\-*\\/^|&~=,]$/.test(trimmed))        return false;
      if (/(\\|>|%>%|<-|->|%%|\\$)$/.test(trimmed)) return false;
      return true;
    }
  ")),

  # Side-by-side diff styling
  tags$style(HTML("
    .diff-panel { display: flex; gap: 10px; margin-top: 8px; }
    .diff-panel > div { flex: 1; min-width: 0; }
    .diff-panel h6 { margin: 0 0 4px 0; font-size: 0.78em;
                     text-transform: uppercase; color: #888; letter-spacing: 0.05em; }
    .diff-pre { font-size: 0.82em; font-family: monospace; background: #f8f8f8;
                border: 1px solid #e3e3e3; border-radius: 4px;
                overflow-x: auto; overflow-y: auto;
                height: 220px; margin: 0; padding: 4px 0; }
    .diff-line-changed { white-space: pre; display: block; padding: 0 8px;
                         line-height: 1.45; background: rgba(255, 220, 0, 0.45); }
    .diff-line-normal  { white-space: pre; display: block; padding: 0 8px;
                         line-height: 1.45; }
  ")),

  # Help-mode styles and overlay intercept
  tags$style(HTML("
    body.help-mode { background-color: #d8d8d8 !important; }
    body.help-mode .container-fluid,
    body.help-mode .well,
    body.help-mode .tab-content,
    body.help-mode .shiny-input-container { background-color: #d8d8d8 !important; }
    /* All buttons — enabled or disabled — go black in help mode */
    body.help-mode button:not(#help_toggle):not(#about_classmate):not(#usage_help),
    body.help-mode a.action-button:not(#help_toggle):not(#about_classmate):not(#usage_help) {
      background-color: #111 !important;
      color: #fff !important;
      border-color: #111 !important;
      opacity: 1 !important;
    }
    /* Tab headers go black in help mode */
    body.help-mode .nav-tabs > li > a,
    body.help-mode .nav-tabs > li.active > a,
    body.help-mode .nav-tabs > li.active > a:focus,
    body.help-mode .nav-tabs > li.active > a:hover {
      background-color: #111 !important;
      color: #fff !important;
      border-color: #111 !important;
    }
    /* ? button: black normally, inverts in help mode; sits above the overlay */
    #help_toggle {
      background-color: #111;
      color: #fff;
      border-color: #111;
      font-weight: bold;
      min-width: 32px;
      position: relative;
      z-index: 9991;
    }
    body.help-mode #help_toggle {
      background-color: #fff !important;
      color: #111 !important;
      border-color: #111 !important;
    }
    /* About Classmate button: hidden normally, shown in help mode, above overlay */
    #about_classmate {
      display: none;
      position: relative;
      z-index: 9991;
      background-color: #fff;
      color: #111;
      border-color: #111;
      margin-right: 8px;
    }
    body.help-mode #about_classmate {
      display: inline-block !important;
    }
    /* Usage text: shown normally, hidden in help mode */
    .usage-text-normal { display: block; }
    body.help-mode .usage-text-normal { display: none !important; }
    /* Usage help button: hidden normally, shown as black button in help mode */
    .usage-text-help {
      display: none;
      position: relative;
      z-index: 9991;
      background-color: #111 !important;
      color: #fff !important;
      border-color: #111 !important;
      font-size: 0.82em;
      line-height: 1.25;
      padding: 2px 8px;
      white-space: nowrap;
      text-align: left;
    }
    body.help-mode .usage-text-help {
      display: inline-block !important;
    }
    body.help-mode #help-mode-label { display: block !important; }

    /* Full-page overlay that intercepts all clicks in help mode */
    #classmate-help-overlay {
      display: none;
      position: fixed;
      top: 0; left: 0;
      width: 100%; height: 100%;
      z-index: 9990;
      cursor: help;
    }
    body.help-mode #classmate-help-overlay { display: block; }
    body.help-mode.modal-open #classmate-help-overlay { display: none; }
  ")),
  tags$div(id = "classmate-help-overlay"),
  tags$script(HTML("
    (function() {
      var overlay = document.getElementById('classmate-help-overlay');
      overlay.addEventListener('click', function(e) {
        // Briefly suppress the overlay so we can find the element underneath
        overlay.style.pointerEvents = 'none';
        var el = document.elementFromPoint(e.clientX, e.clientY);
        overlay.style.pointerEvents = '';
        if (!el) return;

        // Walk up the DOM looking for a button or tab header
        var target = el;
        while (target && target !== document.body) {
          // Shiny action button or regular button with an id
          var id = target.id || '';
          if ((target.tagName === 'BUTTON' || target.classList.contains('action-button')) && id) {
            Shiny.setInputValue('help_button_clicked', {id: id, label: (target.textContent || '').trim().replace(/\\s+/g, ' '), nonce: Math.random()}, {priority: 'event'});
            return;
          }
          // Elements that name their own help entry (the Code / Answer switch)
          if (target.getAttribute && target.getAttribute('data-help-id')) {
            Shiny.setInputValue('help_button_clicked', {id: target.getAttribute('data-help-id'), label: '', nonce: Math.random()}, {priority: 'event'});
            return;
          }
          // Shiny checkbox: the wrapper div.checkbox contains the input that carries the id
          if (target.classList && target.classList.contains('checkbox')) {
            var cb = target.querySelector('input[type=checkbox]');
            if (cb && cb.id) {
              Shiny.setInputValue('help_button_clicked', {id: cb.id, label: (target.textContent || '').trim(), nonce: Math.random()}, {priority: 'event'});
              return;
            }
          }
          // Tab header: Bootstrap tabs use <a data-toggle='tab' data-value='...'>
          if (target.tagName === 'A' && target.getAttribute('data-toggle') === 'tab') {
            var val = target.getAttribute('data-value') || target.textContent.trim();
            Shiny.setInputValue('help_button_clicked', {id: 'tab__' + val, nonce: Math.random()}, {priority: 'event'});
            return;
          }
          target = target.parentElement;
        }
      });
    })();
  ")),

  # Clean "session ended" overlay — shown by do_quit() before stopApp()
  tags$style(HTML("
    #session-ended-overlay {
      display: none;
      position: fixed; top: 0; left: 0; width: 100%; height: 100%;
      background: #fff; z-index: 99999;
      display: none; align-items: center; justify-content: center;
      flex-direction: column; text-align: center;
    }
    #session-ended-overlay.visible { display: flex !important; }
    #session-ended-overlay h3 { color: #333; margin-bottom: 8px; }
    #session-ended-overlay p  { color: #888; font-size: 0.9em; }
  ")),
  tags$div(id = "session-ended-overlay",
    tags$h3("Session ended"),
    tags$p("You may now close this tab.")
  ),

  # Splash screen — shown on fresh open only (hidden on pause-resume via server)
  tags$style(HTML("
    #classmate-splash {
      position: fixed; top: 0; left: 0; width: 100%; height: 100%;
      background: #fff; z-index: 9999;
      display: flex; align-items: center; justify-content: center;
      flex-direction: column; text-align: center;
      opacity: 1;
      transition: opacity 0.8s ease;
    }
    #classmate-splash.fade-out { opacity: 0; }
    #classmate-splash.hidden   { display: none; }
    #classmate-splash h2 {
      font-size: 2em; margin-bottom: 18px; color: #333; font-weight: 600;
    }
    #classmate-splash p {
      margin-top: 18px; font-size: 1em; color: #888; font-style: italic;
    }
  ")),
  tags$div(id = "classmate-splash",
    tags$h2("Welcome to Classmate"),
    tags$img(src = "logo.png", height = "220px",
      style = "mix-blend-mode: multiply;"),
    tags$p("Programmed by Claude. Guided by Richard.")
  ),
  tags$script(HTML("
    (function() {
      var splash = document.getElementById('classmate-splash');
      if (!splash) return;
      setTimeout(function() {
        splash.classList.add('fade-out');
        setTimeout(function() { splash.classList.add('hidden'); }, 850);
      }, 3000);
    })();
  ")),

  # Folder entries in the Change Working Directory browser
  tags$script(HTML("
    document.addEventListener('click', function(e) {
      var a = e.target.closest ? e.target.closest('.cm-chdir-item') : null;
      if (!a) return;
      e.preventDefault();
      Shiny.setInputValue('chdir_enter',
        {name: a.getAttribute('data-name'), nonce: Math.random()}, {priority: 'event'});
    });
  ")),

  # Answer / Past answers panes: same fixed height as the Code editor, scrolling inside
  tags$style(HTML("
    .cm-answer-pane {
      height: 220px; overflow-y: auto; box-sizing: border-box;
      border: 1px solid #ddd; border-radius: 3px; padding: 8px 12px; background: #fff;
    }
    .cm-answer-pane > .shiny-html-output > :first-child { margin-top: 0; }
  ")),

  # Optional "filter by name" box in the Import files dialog. Folders always stay
  # visible so the user can still navigate; only files are filtered.
  tags$script(HTML("
    (function() {
      function applyFilter(modal, input) {
        var q = input.value.trim().toLowerCase();
        var list = modal.querySelector('.sF-fileList');
        if (!list) return;
        Array.prototype.forEach.call(list.querySelectorAll('.sF-file'), function(el) {
          var nm = (el.querySelector('.sF-file-name div') || {}).textContent || '';
          var show = !q || nm.toLowerCase().indexOf(q) !== -1;
          el.style.display = show ? '' : 'none';
          if (!show) el.classList.remove('selected');
        });
      }
      function addFilter(modal) {
        if (modal.querySelector('.classmate-name-filter')) return;
        var footer = modal.querySelector('.sF-responseButtons');
        if (!footer) return;
        var wrap = document.createElement('div');
        wrap.className = 'classmate-name-filter';
        wrap.style.cssText = 'padding: 8px 15px 0 15px;';
        var input = document.createElement('input');
        input.type = 'text';
        input.className = 'form-control input-sm';
        input.placeholder = 'Filter files by name (optional)';
        input.autocomplete = 'off';
        wrap.appendChild(input);
        footer.parentNode.insertBefore(wrap, footer);
        input.addEventListener('input', function() { applyFilter(modal, input); });
        input.addEventListener('keydown', function(e) {
          e.stopPropagation();
          if (e.key === 'Escape') {
            var c = modal.querySelector('#sF-cancelButton');
            if (c) c.click();
          }
        });
        var list = modal.querySelector('.sF-fileList');
        if (list) new MutationObserver(function() { applyFilter(modal, input); })
          .observe(list, { childList: true });
      }
      new MutationObserver(function() {
        var m = document.getElementById('file_select-modal');
        if (m) addFilter(m);
      }).observe(document.body, { childList: true });
    })();
  ")),

  # Tell-me-more inner window (sits above the explanation modal)
  tags$div(
    id = "tell-more-overlay",
    style = paste0(
      "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%;",
      "z-index: 99997; align-items: center; justify-content: center;",
      "background: rgba(0,0,0,0.35);"
    ),
    tags$div(
      style = paste0(
        "background: white; border-radius: 6px;",
        "box-shadow: 0 6px 28px rgba(0,0,0,0.35);",
        "max-width: 520px; width: 92%; padding: 22px 24px 18px;"
      ),
      tags$h5("More detail", style = "margin: 0 0 12px 0; font-weight: bold;"),
      uiOutput("tell_more_content"),
      tags$div(style = "margin-top: 18px; text-align: right;",
        actionButton("tell_more_ok", "OK",
          style = "background-color: white; border-color: #bbb; color: #333;",
          onclick = "document.getElementById('tell-more-overlay').style.display='none';")
      )
    )
  ),

  # Custom header: title left, Help Mode centre, usage bar right
  div(style = paste0(
        "display: flex; align-items: center; justify-content: space-between;",
        "padding: 10px 0 4px 0; border-bottom: 1px solid #e3e3e3; margin-bottom: 12px;"),
    div(style = "display: flex; align-items: center; gap: 10px;",
      tags$img(src = "logo.png", height = "52px",
        style = "mix-blend-mode: multiply; flex-shrink: 0;"),
    div(style = "display: flex; flex-direction: column; gap: 1px;",
      div(style = "display: flex; align-items: baseline; gap: 8px;",
        tags$h3("Classmate", style = "margin: 0;"),
        tags$span(
          style = "color: #888; font-size: 0.75em; white-space: nowrap;",
          paste0("v", utils::packageVersion("classmate"))
        )
      )
    )
    ),
    tags$h3(id = "help-mode-label", "Help Mode",
            style = "margin: 0; display: none;"),
    uiOutput("usage_bar_ui")
  ),

  conditionalPanel(
    condition = "output.has_key == false",
    wellPanel(
      uiOutput("key_status_message"),
      tags$h5("Load a key file"),
      tags$p(em("Load the classmate.key file provided by your instructor,",
                "or enter the web address where it is stored. You will be asked for the PIN",
                "your instructor gave you.")),
      fileInput("key_file", NULL, accept = ".key", width = "100%",
                buttonLabel = "Browse...", placeholder = "classmate.key"),
      web_key_ui("key_web_url", "load_key_web_btn"),
      hr(),
      div(style = "display: flex; justify-content: flex-end; align-items: center;",
        actionButton("quit_setup", "Quit", class = "btn-danger")
      )
    )
  ),

  conditionalPanel(
    condition = "output.has_key == true",

    tabsetPanel(id = "prompt_tabs", type = "tabs",
      tabPanel("Your prompt",
        tags$div(style = "margin-top: 8px;",
          textAreaInput("prompt", label = NULL, rows = 4, width = "100%")
        )
      ),
      tabPanel("Past prompts",
        tags$div(
          style = paste0(
            "margin-top: 8px; height: 108px; overflow-y: auto;",
            "border: 1px solid #e3e3e3; border-radius: 4px;",
            "padding: 4px 8px; background: #fafafa;"
          ),
          uiOutput("prompt_history_ui")
        )
      )
    ),

    tags$p(
      style = "margin: 8px 0 6px 0;",
      "Are there any files or objects you want the prompt to work with?"
    ),
    fluidRow(
      column(12,
        shinyFilesButton("file_select", "Import files",
          title = "Select one or more files", multiple = TRUE),
        tags$span(style = "display: inline-block; width: 4px;"),
        actionButton("add_objects", "Select objects"),
        tags$span(style = "display: inline-block; width: 4px;"),
        actionButton("load_package_data", "Load Package Data"),
        tags$span(style = "display: inline-block; width: 4px;"),
        shinyFilesButton("load_workspace", "Load Workspace",
          title = "Select a workspace file (.RData / .rda)", multiple = FALSE),
      )
    ),
    # Remove links directly above the list of files and objects added for context (shown only when there
    # is something to remove)
    tags$div(id = "context_links", style = "display: none; margin: 6px 0 6px 0; font-size: 0.85em;",
      actionLink("remove_context", "Remove deselected"), " \u00b7 ", actionLink("remove_all_context", "Remove all")),
    uiOutput("context_list_ui"),

    hr(),
    fluidRow(
      column(8,
        actionButton("ask_plain", "Answer"),
        actionButton("ask_code", "Ask for Code", class = "btn-primary"),
        actionButton("clear_prompt", "Clear"),
        tags$span(style = "display: inline-block; margin-left: 14px; vertical-align: middle;",
          tags$div(class = "autorun-box", style = "margin: 0;",
            checkboxInput("autorun_code", "AutoRun Code", value = FALSE)))
      ),
      column(4, div(style = "text-align: right;",
        actionButton("preferences", "Preferences")
      ))
    ),

    tags$div(style = "margin-top: 10px;"),
    tabsetPanel(id = "main_tabs", type = "tabs",
      tabPanel("Code",
        tags$div(style = "margin-top: 8px;",
          aceEditor("code_editor", value = "", mode = "r", theme = "textmate",
            height = "220px", fontSize = 13, showLineNumbers = TRUE,
            highlightActiveLine = TRUE, wordWrap = FALSE,
            autoScrollEditorIntoView = TRUE, debounce = 100)
        )
      ),
      tabPanel("R output",
        tags$div(style = "margin-top: 8px; height: 220px; overflow-y: auto;",   # same height as the code editor
          uiOutput("console_output_ui")
        )
      ),
      tabPanel("Past code",
        tags$div(style = "margin-top: 8px; min-height: 220px;",
          uiOutput("past_code_ui")
        )
      ),
      tabPanel("Answer",
        tags$div(class = "cm-answer-pane", style = "margin-top: 8px;",
          uiOutput("answer_ui")
        )
      ),
      tabPanel("Past answers",
        tags$div(class = "cm-answer-pane", style = "margin-top: 8px;",
          uiOutput("past_answers_ui")
        )
      ),
    ),

    # Code / Answer switch: placed at the end of the tab bar (moved there by the script below).
    # Shown once both code and an answer exist; it follows the current mode and can be clicked.
    tags$div(style = "display: none;",
      tags$li(id = "mode_switch_li", class = "cm-switch-li", `data-help-id` = "mode_switch",
              style = "display: none;",
        tags$span(id = "mode_switch_code_lbl", class = "cm-switch-lbl", "Code"),
        tags$label(class = "cm-switch",
          tags$input(type = "checkbox", id = "mode_switch"),
          tags$span(class = "cm-slider")),
        tags$span(id = "mode_switch_answer_lbl", class = "cm-switch-lbl", "Answer"))
    ),
    tags$script(HTML("
      (function() {
        var ul = document.getElementById('main_tabs');
        var li = document.getElementById('mode_switch_li');
        if (li && ul) ul.appendChild(li);
        var cb = document.getElementById('mode_switch');
        function paint() {
          document.getElementById('mode_switch_code_lbl').style.fontWeight   = cb.checked ? 'normal' : 'bold';
          document.getElementById('mode_switch_answer_lbl').style.fontWeight = cb.checked ? 'bold' : 'normal';
        }
        window.cmSetModeSwitch = function(answer) { cb.checked = !!answer; paint(); };
        // Highlight (and scroll to) a prompt in the Past prompts list
        function cmHighlight(msg) {
          document.querySelectorAll('.cm-prompt-row.cm-hl').forEach(function(r) { r.classList.remove('cm-hl'); });
          if (!msg || !msg.text) return;
          var rows = document.querySelectorAll('.cm-prompt-row');
          for (var i = 0; i < rows.length; i++) {
            if (rows[i].getAttribute('data-p') === msg.text) {
              rows[i].classList.add('cm-hl');
              rows[i].scrollIntoView({block: 'nearest'});
              break;
            }
          }
        }
        function cmRegister() {
          if (window.Shiny && Shiny.addCustomMessageHandler) Shiny.addCustomMessageHandler('cm_highlight_prompt',
            function(msg) { setTimeout(function() { cmHighlight(msg); }, 150); });
          else setTimeout(cmRegister, 200);
        }
        cmRegister();
        cb.addEventListener('change', function() {
          paint();
          if (window.Shiny) Shiny.setInputValue('mode_switch_clicked', cb.checked ? 'answer' : 'code', {priority: 'event'});
        });
        paint();
      })();
    ")),
    tags$style(HTML("
      #main_tabs .cm-switch-li { float: left; margin: 0 0 0 16px; padding-top: 7px; font-size: 0.85em; color: #333; }
      #main_tabs .cm-switch-li > * { vertical-align: middle; }
      .cm-switch-lbl { display: inline-block; margin: 0 6px; }
      .cm-prompt-row.cm-hl { background-color: #fff3b0; border-radius: 3px; }
      .pref-grid { display: grid; grid-template-columns: 1fr 1fr; column-gap: 12px; }
      a.action-button.disabled, a.action-button[disabled] { pointer-events: none; opacity: 0.5; }
      /* Boxes in the context list show a cross, not a tick, when selected (selected = to be removed) */
      #context_list_box input[type=checkbox] { -webkit-appearance: none; appearance: none; width: 14px; height: 14px;
        border: 1px solid #999; border-radius: 3px; background: #fff; cursor: pointer; outline: none; }
      #context_list_box input[type=checkbox]:not(:checked)::after { content: '\\2713'; display: block; text-align: center;
        font-size: 12px; line-height: 12px; color: #1e7e34; font-weight: bold; }
      #context_list_box input[type=checkbox]:checked { border-color: #c0392b; }
      #context_list_box input[type=checkbox]:checked + span { text-decoration: line-through; color: #aaa; }
      #context_list_box input[type=checkbox]:checked::after { content: '\\2715'; display: block; text-align: center;
        font-size: 11px; line-height: 12px; color: #c0392b; font-weight: bold; }
      /* File dialogs (Import files, Load Script, Save...): the Select / Save button to the left of Cancel,
         both right-justified, as in the other pop-ups */
      .sF-responseButtons.modal-footer { display: flex; flex-direction: row-reverse; justify-content: flex-start; gap: 5px; }
      .cm-switch { position: relative; display: inline-block; width: 36px; height: 18px; margin: 0; }
      .cm-switch input { opacity: 0; width: 0; height: 0; }
      .cm-slider { position: absolute; cursor: pointer; top: 0; left: 0; right: 0; bottom: 0;
                   background-color: #bbb; border-radius: 18px; transition: background-color 0.2s; }
      .cm-slider:before { content: ''; position: absolute; height: 14px; width: 14px; left: 2px; bottom: 2px;
                          background-color: white; border-radius: 50%; transition: transform 0.2s; }
      .cm-switch input:checked + .cm-slider { background-color: #2c5f8a; }
      .cm-switch input:checked + .cm-slider:before { transform: translateX(18px); }
    ")),

    div(style = "margin-top: 6px;",
      disabled(actionButton("run_code", "Run", class = "btn-success")),
      tags$span(style = "display: inline-block; width: 4px;"),
      disabled(actionButton("explain_code", "Explain",
        style = "background-color: white; border-color: #bbb; color: #333;")),
      tags$span(style = "display: inline-block; width: 4px;"),
      disabled(actionButton("fix_code", "Fix",
        style = "background-color: white; border-color: #bbb; color: #333;")),
      tags$span(style = "display: inline-block; width: 4px;"),
      shinyFilesButton("load_script", "Load Script",
        title = "Select an R script to load", multiple = FALSE,
        style = "background-color: white; border-color: #bbb; color: #333;")
    ),
    uiOutput("run_status"),

    fluidRow(style = "margin-top: 10px;",
      column(6, div(style = "display: flex; align-items: center; gap: 8px;",
        disabled(actionButton("save_block", "Save Code Block",
          style = "background-color: white; border-color: #bbb; color: #333;")),
        actionButton("view_log", "View Code Log",
          style = "background-color: white; border-color: #bbb; color: #333;")
      )),
      column(6,
        div(style = "display: flex; justify-content: flex-end; align-items: flex-start; white-space: nowrap;",
          actionButton("about_classmate", "About Classmate",
            style = "margin-right: 8px;"),
          actionButton("help_toggle", "?",
            style = "margin-right: 8px;"),
          actionButton("quick_console", "Quick Console",
            style = "background-color: white; border-color: #bbb; color: #333; margin-right: 8px;"),
          actionButton("pause_app", "Pause",
            style = "background-color: white; border-color: #bbb; color: #333; margin-right: 14px;"),
          div(style = "display: inline-flex; flex-direction: column; align-items: flex-start;",
            div(
              actionButton("new_conversation", "New conversation",
                style = "background-color: #e67e22; border-color: #ca6f1e; color: white;"),
              actionButton("quit", "Quit with Code Log", class = "btn-danger",
                style = "margin-left: 4px;")
            ),
            uiOutput("conv_remaining_ui")
          )
        )
      )
    ),

  )
)

# --- Server ------------------------------------------------------------------

# Fetches a classmate key from a web address. The address may point directly
# at a .key file, or at a page/folder listing that links to one or more .key
# files, in which case the most recently created key is used.
# Returns list(payload = <key list>) on success or list(error = <message>).
fetch_key_from_web <- function(url) {
  url <- trimws(url %||% "")
  if (!nzchar(url)) return(list(error = "Please enter a web address."))
  if (!grepl("^[a-zA-Z][a-zA-Z0-9+.-]*://", url)) url <- paste0("https://", url)
  if (!grepl("^https://", url, ignore.case = TRUE))
    return(list(error = "The web address must start with https://"))

  # Friendly rewrites for common sharing services
  url <- sub("^https://github\\.com/([^/]+/[^/]+)/blob/", "https://raw.githubusercontent.com/\\1/", url)
  if (grepl("^https://(www\\.)?dropbox\\.com/", url) && !grepl("[?&]dl=1", url))
    url <- if (grepl("[?&]dl=0", url)) sub("dl=0", "dl=1", url) else
             paste0(url, if (grepl("\\?", url)) "&dl=1" else "?dl=1")

  get_raw <- function(u) {
    resp <- httr2::request(u) |>
      httr2::req_timeout(15) |>
      httr2::req_user_agent("classmate") |>
      httr2::req_error(is_error = \(r) FALSE) |>
      httr2::req_perform()
    list(status = httr2::resp_status(resp), body = httr2::resp_body_raw(resp))
  }
  as_key <- function(raw_body) {
    if (length(raw_body) == 0 || length(raw_body) > 1e6) return(NULL)
    tmp <- tempfile(fileext = ".rds")
    on.exit(unlink(tmp), add = TRUE)
    writeBin(raw_body, tmp)
    p <- tryCatch(readRDS(tmp), error = function(e) NULL, warning = function(w) NULL)
    if (!identical(classmate:::classmate_key_kind(p), "invalid")) p else NULL
  }

  first <- tryCatch(get_raw(url), error = function(e) NULL)
  if (is.null(first))
    return(list(error = paste("Could not connect to that address. Please check it",
                              "and your internet connection, then try again.")))
  if (first$status >= 400)
    return(list(error = paste0("The web address returned an error (HTTP ", first$status,
                               "). Please check the address.")))

  direct <- as_key(first$body)
  if (!is.null(direct)) return(list(payload = direct))

  # Not a key itself: treat as a page listing .key files
  page <- tryCatch(rawToChar(first$body), error = function(e) "")
  page <- iconv(page, from = "UTF-8", to = "UTF-8", sub = "")
  links <- unique(unlist(regmatches(
    page, gregexpr("(?:href=[\"']|https?://)[^\"'<>\\s]*\\.key(?=[\"'<>\\s?#]|$)",
                   page, perl = TRUE))))
  links <- sub("^href=[\"']", "", links)
  if (length(links) == 0)
    return(list(error = paste("No classmate key was found at that address. It should",
                              "point to a .key file, or to a page that links to one.")))
  parts <- regmatches(url, regexec("^(https://[^/]+)(/.*)?$", url))[[1]]
  origin <- parts[2]
  base_dir <- sub("[^/]*$", "", sub("[?#].*$", "", url))
  links <- vapply(links, function(l) {
    if (grepl("^https?://", l)) l
    else if (grepl("^//", l)) paste0("https:", l)
    else if (grepl("^/", l)) paste0(origin, l)
    else paste0(base_dir, l)
  }, character(1), USE.NAMES = FALSE)
  links <- links[grepl("^https://", links)]
  links <- head(unique(links), 20)

  keys <- list()
  for (l in links) {
    r <- tryCatch(get_raw(l), error = function(e) NULL)
    if (is.null(r) || r$status >= 400) next
    k <- as_key(r$body)
    if (!is.null(k)) keys[[length(keys) + 1]] <- k
  }
  if (length(keys) == 0)
    return(list(error = paste("Found .key links at that address, but none could be",
                              "read as a valid classmate key.")))
  created <- vapply(keys, function(k)
    tryCatch(as.numeric(as.POSIXct(k$created)), error = function(e) NA_real_),
    numeric(1))
  idx <- if (all(is.na(created))) length(keys) else which.max(replace(created, is.na(created), -Inf))
  list(payload = keys[[idx]])
}

# ---------------------------------------------------------------------------
# Answer button: answer pipeline
#
# Stage 1 ("plan"): Claude either answers directly from the object/file
#   summaries, or asks for a small read-only calculation.
# Stage 2: the calculation is run in a throw-away environment, its output is
#   scrubbed of individual records, and Claude writes the answer in plain
#   English. The student sees only the answer (code and output are available
#   under "Show my working").
# ---------------------------------------------------------------------------

answer_mode_rule <- paste(
  "ANSWER MODE: The student has chosen the 'Answer' button. They want a plain-English",
  "ANSWER to their question, not code. They will never see any code you write, so",
  "never refer to 'the code above' or tell them to run anything.",
  "First decide whether the question can be answered from the information provided —",
  "the summaries of their objects and files (names, types, dimensions, column names)",
  "plus your general knowledge of R, statistics and GIS.",
  "(A) If it can: begin your reply with the line ANSWER: and then give the complete",
  "answer. Write for a student: short paragraphs or simple lists, refer to their actual",
  "objects and variable names, and explain any technical term you use. Only include a",
  "code snippet if they explicitly ask how to do something in R.",
  "(B) If answering needs a calculation on their actual data (counts, averages, missing",
  "values, distributions, correlations, model results, 'how many', 'which is largest'):",
  "begin your reply with the line COMPUTE: then one short sentence saying what you will",
  "calculate, then exactly one ```r code block. The code is run for you and its summary",
  "output is returned to you so that you can write the answer.",
  "Never put ANSWER: and COMPUTE: in the same reply. Your reply must always begin with",
  "ANSWER: or COMPUTE:, even if earlier replies in this conversation used another format",
  "(such as a DESCRIPTION: line followed by code): do not copy that format.",
  "Text in double square brackets, such as [[NAME_1]], is a placeholder for a text value from",
  "the student's data that has been withheld from you for privacy. Refer to it exactly as",
  "written; it is filled in on the student's screen. Never use such placeholders inside code."
)

answer_compute_rules <- paste(
  "COMPUTE CODE RULES — strict. Code that breaks them is not run.",
  "1. Read-only. Do not create, modify, delete or save any file or object that persists:",
  "no saving or exporting, no plots or graphics devices, no installing packages, and no",
  "setwd(), rm(), assign(), <<-, sink(), options(), source(), eval(), system(), or network",
  "access. Ordinary temporary variables with <- are fine.",
  "2. Summaries only. Print aggregate results: counts, means, medians, quantiles,",
  "correlations, tables of counts, model coefficients. NEVER print individual records,",
  "rows, identifiers, names or free-text values: no head(), tail(), str(), View(), and no",
  "listing of unique names or identifiers. When summarising by group, do not print groups",
  "with fewer than 5 records.",
  "3. Print results with cat(), sprintf(), a named vector, table() or summary() — NOT as a",
  "printed data frame or tibble, because printed data-frame rows are blocked from being",
  "sent back to you.",
  "4. For sf objects call sf::st_drop_geometry() before summarising; never print geometries.",
  "5. Use only the exact column names listed in the context. Call library() for every",
  "package you use. Keep the code short.",
  "IDENTIFIER COLUMNS: In the data your code runs on, text columns holding names, IDs or",
  "addresses contain opaque codes such as [[NAME_7]] instead of the real values (which are",
  "withheld for privacy and restored on the student's screen). Use these columns only to",
  "identify records — for example which.max(), counting, grouping or joining. Do not sort,",
  "compare, match, count characters in, or transform them (no toupper, substr, gsub, grepl,",
  "as.numeric and so on). Never print coordinates, geometries or bounding boxes: the location",
  "of an individual record is identifying.",
  "REQUESTS FOR RECORDS: If the student asks to see individual rows, records or lists of",
  "names or identifiers (for example 'show me the first five rows' or 'list the tracts'), or",
  "asks for part of an identifier (capital letters, a few digits, the first word) or the",
  "location of a single record,",
  "do not attempt it. Decide this BEFORE you start writing, and reply with ANSWER: saying",
  "briefly and kindly that this button gives summaries rather than listing individual records,",
  "and that Ask for Code will give code that shows them on the student's own screen. Never",
  "begin an answer and then retract it. (Looking up a single value, such as which record is",
  "largest, is fine: print its value; identifying text is withheld from you automatically.)"
)

answer_column_rule <- paste(
  "COLUMN NAME RULE: When files or objects are provided with a known schema (listed as",
  "columns: ... in the context), use only those exact column names. Do not guess,",
  "abbreviate, or paraphrase column names. If the student refers to a variable by a name",
  "that does not exactly match a listed column, use the closest actual match."
)

answer_clarity_rule <- paste(
  "CLARITY RULE: If the question depends on the student's own data or objects (for example",
  "'this data' or 'my model') but no object or file has been provided in the context, or",
  "you cannot tell which one they mean, respond with exactly this format and nothing else:",
  "NEEDS_CLARIFICATION",
  "REASON: <plain English explanation of what is missing>",
  "SUGGESTIONS: <for example: use Select objects or Import files to add the data first>",
  "Otherwise do not ask for clarification — choose the most sensible interpretation."
)

build_answer_system_prompt <- function(language = "English") {
  paste(c(
    "You are a friendly teaching assistant helping a student who is learning R, statistics and GIS.",
    answer_mode_rule,
    answer_compute_rules,
    answer_column_rule,
    build_language_clause(language),
    answer_clarity_rule,
    scope_rule,
    research_integrity_rule,
    disclosure_risk_rule
  ), collapse = "\n\n")
}

build_answer_write_system_prompt <- function(language = "English") {
  paste(c(
    "You are a friendly teaching assistant helping a student who is learning R, statistics and GIS.",
    paste(
      "You ran a calculation on the student's data to answer their question, and its summary",
      "output is supplied. Now write the final answer.",
      "Answer in plain English in a few short paragraphs or a simple list. Lead with the direct",
      "answer, then add brief context or caveats. Interpret the numbers rather than just repeating",
      "them, and round sensibly. Do not mention code, R output, 'the console' or how the numbers",
      "were computed unless it is needed to explain a caveat (the student can open 'Show my",
      "working' to see that). Do not include code blocks. If the results are incomplete, truncated,",
      "or do not actually answer the question, say so plainly rather than guessing. Talk about groups",
      "and summaries only — never reproduce or guess values for individual people or records.",
      "Text in double square brackets, such as [[NAME_1]], stands for a text value from the",
      "student's data that was withheld from you for privacy: keep it exactly as written, in",
      "context (for example 'the tract [[NAME_1]]'); it is filled in on the student's screen.",
      "Do not invite follow-up replies; this is a one-shot answer."
    ),
    build_language_clause(language),
    research_integrity_rule
  ), collapse = "\n\n")
}

# Splits a stage-1 reply into list(type = "answer"/"compute", text, code)
parse_answer_plan <- function(raw) {
  txt <- trimws(raw)
  if (grepl("^COMPUTE:", txt)) {
    parts <- split_response_into_text_and_code(sub("^COMPUTE:[ \t]*", "", txt))
    if (nzchar(trimws(parts$code)))
      return(list(type = "compute", text = parts$explanation, code = parts$code))
    return(list(type = "answer", text = trimws(parts$explanation), code = ""))
  }
  # A reply that forgot the ANSWER:/COMPUTE: line but is in the Ask-for-Code format (a
  # DESCRIPTION: line and a code block, copied from earlier turns of the conversation) is a
  # request to calculate; showing it raw would give the student code instead of an answer.
  if (!grepl("^(ANSWER|COMPUTE):", txt)) {
    parts <- split_response_into_text_and_code(txt)
    if (nzchar(trimws(parts$code)))
      return(list(type = "compute", text = parts$explanation, code = parts$code))
  }
  list(type = "answer", text = trimws(sub("^ANSWER:[ \t]*\n?", "", txt)), code = "")
}

# Functions that code run by the Answer button may not call. The code is written by the AI
# from the student's data context, so this is a guard against accidents and
# against instructions smuggled in through object or column names.
.ANSWER_BLOCKED_EXACT <- c(
  "setwd", "rm", "remove", "assign", "delayedAssign", "makeActiveBinding", "lockBinding",
  "unlockBinding", "attach", "detach", "sink", "options", "Sys.setenv", "Sys.setlocale",
  "Sys.chmod", "Sys.setFileTime", "source", "sys.source", "eval", "evalq", "system",
  "system2", "shell", "shell.exec", "file.create", "file.remove", "file.rename",
  "file.copy", "file.append", "file.symlink", "unlink", "dir.create", "dput", "dump",
  "png", "pdf", "jpeg", "tiff", "bmp", "svg", "postscript", "cairo_pdf", "dev.copy",
  "dev.print", "ggsave", "tmap_save", "url", "download.file", "browseURL", "curl",
  "request", "req_perform", "GET", "POST", "readline", "scan", "menu", "View", "edit",
  "fix", "browser", "debug", "quit", "q", "stopApp", "install_github", "install_cran",
  "<<-", "->>",
  # per-record locations are identifying (home addresses, small areas)
  "st_coordinates", "st_centroid", "st_point_on_surface", "st_bbox", "st_as_text",
  "st_as_binary", "st_geometry", "st_geometry<-", "coordinates", "bbox"
)
.ANSWER_BLOCKED_PATTERN <- "^(write|save|install|remove\\.packages|update\\.packages|download)"

answer_code_violations <- function(code) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return("code that does not parse")
  found <- character(0)
  walk <- function(x) {
    if (is.call(x)) {
      f <- x[[1]]
      nm <- if (is.name(f)) as.character(f)
            else if (is.call(f) && length(f) == 3 && as.character(f[[1]]) %in% c("::", ":::"))
              as.character(f[[3]])
            else ""
      if (nzchar(nm) && (nm %in% .ANSWER_BLOCKED_EXACT || grepl(.ANSWER_BLOCKED_PATTERN, nm)))
        found <<- c(found, nm)
      if (nm %in% c("cat", "print", "message", "writeLines") && !is.null(names(x)) &&
          any(c("file", "append") %in% names(x)))
        found <<- c(found, paste0(nm, "(file =)"))
      for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        walk(x[[i]])
      }
    } else if (is.pairlist(x) || is.expression(x)) {
      for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        walk(x[[i]])
      }
    }
  }
  for (e in exprs) walk(e)
  unique(found)
}

# Runs the Answer button's code in a throw-away environment, with graphics going
# nowhere. Returns list(success, message, output, warnings).
# TRUE when the code installs or updates packages (these are exempt from the Quick Console time limit)
code_installs_packages <- function(code) {
  grepl(paste0("\\b(install\\.packages|update\\.packages|install_github|install_gitlab|install_bitbucket|",
               "install_url|install_git|install_cran|install_local|install_version|pkg_install|",
               "pak\\s*\\(|pak::|BiocManager::install|devtools::install|renv::(install|restore))"),
        paste(code, collapse = "\n"), perl = TRUE)
}

run_answer_code <- function(code, timeout = 60, objects = list()) {
  env <- new.env(parent = globalenv())
  .enter <- cm_enter_root()
  on.exit(.enter$restore(), add = TRUE)
  if (!.enter$ok)
    return(list(success = FALSE, message = "Classmate could not open your working directory.",
                output = character(0), warnings = character(0), env = env))
  for (nm in names(objects)) assign(nm, objects[[nm]], envir = env)   # masked copies shadow the real objects
  warnings_seen <- character(0)
  out <- character(0)
  grDevices::pdf(NULL)
  dev_now <- grDevices::dev.cur()
  on.exit({
    if (dev_now %in% grDevices::dev.list())
      tryCatch(grDevices::dev.off(dev_now), error = function(e) invisible(NULL))
  }, add = TRUE)
  con <- textConnection(code)
  on.exit(close(con), add = TRUE)
  setTimeLimit(elapsed = timeout, transient = TRUE)
  on.exit(setTimeLimit(elapsed = Inf, transient = FALSE), add = TRUE)
  res <- tryCatch({
    out <- utils::capture.output(withCallingHandlers(
      source(con, local = env, print.eval = TRUE),
      warning = function(w) {
        warnings_seen <<- c(warnings_seen, strip_ansi(conditionMessage(w)))
        invokeRestart("muffleWarning")
      }
    ))
    list(success = TRUE, message = "")
  }, error = function(e) list(success = FALSE, message = strip_ansi(conditionMessage(e))))
  c(res, list(output = strip_ansi(out), warnings = warnings_seen, env = env))
}

# What may be sent back to Claude: printed records/str() lines are removed (the
# same scrubbing used elsewhere), then length is capped.
scrub_results_for_answer <- function(output, warnings = character(0),
                                     max_chars = 6000L, max_lines = 150L) {
  txt   <- scrub_console_output(paste(output, collapse = "\n"))
  lines <- strsplit(txt, "\n", fixed = TRUE)[[1]]
  geom  <- grepl("\\b(POINT|POLYGON|LINESTRING|MULTI[A-Z]+|GEOMETRYCOLLECTION)\\s*Z?\\s*\\(", lines)
  lines[geom] <- "[geometry — values not shown]"
  if (length(lines) > max_lines)
    lines <- c(lines[seq_len(max_lines)],
               sprintf("[... %d further lines not shown]", length(lines) - max_lines))
  txt <- paste(lines, collapse = "\n")
  if (nchar(txt) > max_chars) txt <- paste0(substr(txt, 1, max_chars), "\n[... output truncated]")
  if (length(warnings) > 0)
    txt <- paste0(txt, "\nWarnings: ", paste(utils::head(unique(warnings), 5), collapse = "; "))
  txt
}

# --- Withholding identifying text values ---------------------------------------
# Printed single values (e.g. cat(top_row$NAME)) slip past the row filter. So, before
# results go to Claude, text values from columns that look like names/identifiers
# (by column name, or many distinct values) are replaced with placeholders such as
# [[NAME_1]]. Claude writes the answer around the placeholders and Classmate fills
# the real values back in locally, so they never leave the student's computer.
.NAMEY_COLUMN <- "(^|[^a-z])(name|names|id|ids|ref|address|postcode|zip|surname|forename|email|phone|geoid|code|label)([^a-z]|$)"

build_redaction_vocab <- function(objs, max_values = 200000L) {
  values <- character(0); labels <- character(0)
  for (o in objs) {
    if (!is.data.frame(o)) next
    for (cn in names(o)) {
      col <- o[[cn]]
      if (!(is.character(col) || is.factor(col))) next
      u <- unique(as.character(col))
      u <- u[!is.na(u) & nchar(u) >= 3L & !grepl("^\\[\\[[A-Za-z0-9_]+\\]\\]$", u)]   # skip values that are already codes
      if (length(u) == 0) next
      namey <- grepl(.NAMEY_COLUMN, tolower(cn))
      high  <- length(u) > 12L && length(u) / max(length(col), 1L) >= 0.05
      if (namey || high) {
        values <- c(values, u)
        labels <- c(labels, rep(toupper(gsub("[^A-Za-z0-9]+", "_", cn)), length(u)))
      }
      if (length(values) > max_values) break
    }
  }
  keep <- !duplicated(values)
  data.frame(value = values[keep], label = labels[keep], stringsAsFactors = FALSE)
}

redact_values <- function(text, vocab, map = character(0)) {
  if (is.null(vocab) || nrow(vocab) == 0 || !nzchar(text)) return(list(text = text, map = map))
  appears <- vapply(vocab$value, function(v) grepl(v, text, fixed = TRUE), logical(1))
  hit <- vocab[appears, , drop = FALSE]
  hit <- hit[order(-nchar(hit$value)), , drop = FALSE]
  for (i in seq_len(nrow(hit))) {
    v   <- hit$value[i]
    pat <- paste0("(?<![[:alnum:]])", gsub("([.|()\\\\^{}+$*?\\[\\]])", "\\\\\\1", v, perl = TRUE), "(?![[:alnum:]])")
    if (!grepl(pat, text, perl = TRUE)) next
    token <- if (length(map) > 0) names(map)[match(v, unname(map))] else NA_character_  # reuse an existing token
    if (is.na(token)) {
      lab    <- hit$label[i]
      prefix <- paste0("[[", lab, "_")
      map_names <- names(map) %||% character(0)
      used   <- map_names[startsWith(map_names, prefix)]
      n      <- max(c(0L, suppressWarnings(as.integer(sub(".*_([0-9]+)\\]\\]$", "\\1", used)))), na.rm = TRUE) + 1L
      token  <- sprintf("[[%s_%d]]", lab, n)
      map[token] <- v
    }
    text <- gsub(pat, token, text, perl = TRUE)
  }
  list(text = text, map = map)
}

# Which columns of a data frame hold identifying text (same rule as the vocabulary above)
identifier_columns <- function(o) {
  if (!is.data.frame(o)) return(character(0))
  Filter(function(cn) {
    col <- o[[cn]]
    if (!(is.character(col) || is.factor(col))) return(FALSE)
    u <- unique(as.character(col)); u <- u[!is.na(u)]
    if (length(u) == 0) return(FALSE)
    grepl(.NAMEY_COLUMN, tolower(cn)) ||
      (length(u) > 12L && length(u) / max(length(col), 1L) >= 0.05)
  }, names(o))
}

# Replaces identifying text columns, in COPIES of the data, with opaque codes such as
# [[NAME_7]]. The calculation then never sees real names/IDs, so nothing it does with
# them (capitals, substrings, arithmetic) can reveal them. `map` is token -> value; tokens
# already in it are reused so that they stay stable through the conversation.
# Returns list(objs, map (all tokens used in this run), id_cols, failed).
pseudonymise_objects <- function(objs, map = character(0)) {
  tok_to_val <- map
  val_to_tok <- if (length(map) > 0) setNames(names(map), unname(map)) else character(0)
  id_cols <- character(0); failed <- character(0)
  out <- objs
  for (nm in names(objs)) {
    o <- objs[[nm]]
    cols <- identifier_columns(o)
    if (length(cols) == 0) next
    ok <- tryCatch({
      for (cn in cols) {
        col <- o[[cn]]
        chr <- as.character(col)
        u   <- unique(chr[!is.na(chr)])
        new <- u[!(u %in% names(val_to_tok))]
        if (length(new) > 0) {
          lab      <- toupper(gsub("[^A-Za-z0-9]+", "_", cn))
          prefix   <- paste0("[[", lab, "_")
          names_now <- names(tok_to_val) %||% character(0)
          used     <- suppressWarnings(as.integer(sub("\\]\\]$", "",
                        sub(paste0("^", gsub("([\\[\\]])", "\\\\\\1", prefix, perl = TRUE)), "",
                            names_now[startsWith(names_now, prefix)]))))
          idx      <- setdiff(seq_len(length(new) + length(used)), used)[seq_along(new)]
          toks     <- sprintf("[[%s_%d]]", lab, idx)
          val_to_tok[new] <- toks
          tok_to_val[toks] <- new
        }
        coded <- unname(val_to_tok[chr])
        o[[cn]] <- if (is.factor(col)) factor(coded) else coded
        id_cols <- c(id_cols, cn)
      }
      TRUE
    }, error = function(e) FALSE)
    if (isTRUE(ok)) out[[nm]] <- o else failed <- c(failed, nm)
  }
  list(objs = out, map = tok_to_val, id_cols = unique(id_cols), failed = failed)
}

# String constants in the code that equal a real identifier value (e.g. a tract name the
# student typed in the question) are swapped for that value's code so that lookups work.
replace_known_literals <- function(code, val_to_tok) {
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs) || length(val_to_tok) == 0) return(code)
  swap <- function(x) {
    if (is.character(x) && length(x) == 1L && !is.na(x) && x %in% names(val_to_tok))
      return(unname(val_to_tok[[x]]))
    if (is.call(x) || is.pairlist(x) || is.expression(x))
      for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        x[[i]] <- swap(x[[i]])
      }
    x
  }
  paste(vapply(exprs, function(e) paste(deparse(swap(e), width.cutoff = 500L), collapse = "\n"),
               character(1)), collapse = "\n")
}

# When identifying columns exist, text/pattern functions on them would give wrong answers
# (the calculation only sees opaque codes) or try to extract pieces of them, so they are refused.
.ANSWER_STRING_FNS <- c("toupper", "tolower", "casefold", "chartr", "substr", "substring",
  "strsplit", "sub", "gsub", "regmatches", "regexpr", "gregexpr", "regexec", "grepl", "grep",
  "startsWith", "endsWith", "nchar", "strtoi", "utf8ToInt", "intToUtf8", "iconv")
.ANSWER_ORDER_FNS <- c("sort", "order", "rank", "min", "max", "range", "arrange", "sort_by", "xtfrm")
answer_identifier_violations <- function(code, id_cols) {
  if (length(id_cols) == 0) return(character(0))
  exprs <- tryCatch(parse(text = code, keep.source = FALSE), error = function(e) NULL)
  if (is.null(exprs)) return(character(0))
  found <- character(0)
  mentions_id <- function(x) {
    if (is.name(x)) return(as.character(x) %in% id_cols)
    if (is.character(x)) return(any(x %in% id_cols))
    if (is.call(x) || is.pairlist(x) || is.expression(x) || is.list(x))
      return(any(vapply(seq_along(x), function(i)
        !identical(x[[i]], quote(expr = )) && mentions_id(x[[i]]), logical(1))))
    FALSE
  }
  walk <- function(x) {
    if (is.call(x)) {
      f  <- x[[1]]
      nm <- if (is.name(f)) as.character(f)
            else if (is.call(f) && length(f) == 3 && as.character(f[[1]]) %in% c("::", ":::"))
              as.character(f[[3]]) else ""
      if (nm %in% .ANSWER_STRING_FNS || grepl("^(str_|stri_)", nm))
        found <<- c(found, nm)
      else if (nm %in% .ANSWER_ORDER_FNS && length(x) > 1 && mentions_id(as.list(x)[-1]))
        found <<- c(found, paste0(nm, "() on an identifier column"))
      for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        walk(x[[i]])
      }
    } else if (is.pairlist(x) || is.expression(x)) {
      for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next
        walk(x[[i]])
      }
    }
  }
  for (e in exprs) walk(e)
  unique(found)
}

restore_placeholders <- function(text, map) {
  if (length(map) == 0 || !nzchar(text)) return(text)
  toks <- unique(unlist(regmatches(text, gregexpr("\\[\\[[A-Za-z0-9_]+\\]\\]", text))))
  for (tok in intersect(toks, names(map))) text <- gsub(tok, map[[tok]], text, fixed = TRUE)
  text
}

# Text from the AI is untrusted: escape it, then apply light formatting
# (paragraphs, bullet/numbered lists, **bold**, `code`, ``` fenced blocks ```).
answer_to_html <- function(text) {
  esc <- function(x) {
    x <- gsub("&", "&amp;", x, fixed = TRUE)
    x <- gsub("<", "&lt;", x, fixed = TRUE)
    gsub(">", "&gt;", x, fixed = TRUE)
  }
  inline <- function(x) {
    vapply(x, function(one) {
      one <- esc(one)
      # Code spans first, so that * inside code (e.g. `i * 2`) is never treated as emphasis
      spans <- regmatches(one, gregexpr("`[^`]+`", one))[[1]]
      for (k in seq_along(spans)) one <- sub(spans[k], paste0("@@CODE", k, "@@"), one, fixed = TRUE)
      one <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", one)
      one <- gsub("(?<![*\\w])\\*([^*\\s][^*]*?)\\*(?![*\\w])", "<em>\\1</em>", one, perl = TRUE)
      for (k in seq_along(spans))
        one <- sub(paste0("@@CODE", k, "@@"), paste0("<code>", substr(spans[k], 2, nchar(spans[k]) - 1), "</code>"),
                   one, fixed = TRUE)
      one
    }, character(1), USE.NAMES = FALSE)
  }
  text_block <- function(seg) {
    paras <- trimws(strsplit(seg, "\n{2,}")[[1]])
    paras <- paras[nzchar(paras)]
    vapply(paras, function(p) {
      lines <- trimws(strsplit(p, "\n", fixed = TRUE)[[1]])
      if (all(grepl("^[-*] ", lines)))
        return(paste0("<ul>", paste0("<li>", inline(sub("^[-*] ", "", lines)), "</li>", collapse = ""), "</ul>"))
      if (all(grepl("^[0-9]+[.)] ", lines)))
        return(paste0("<ol>", paste0("<li>", inline(sub("^[0-9]+[.)] ", "", lines)), "</li>", collapse = ""), "</ol>"))
      if (length(lines) == 1 && grepl("^#+ ", lines))
        return(paste0("<p><strong>", inline(sub("^#+ ", "", lines)), "</strong></p>"))
      paste0("<p>", paste(inline(lines), collapse = "<br>"), "</p>")
    }, character(1), USE.NAMES = FALSE)
  }
  pat <- "```[A-Za-z]*[ \t]*\n[\\s\\S]*?```"
  blocks <- regmatches(text, gregexpr(pat, text, perl = TRUE))[[1]]
  rest   <- regmatches(text, gregexpr(pat, text, perl = TRUE), invert = TRUE)[[1]]
  out <- character(0)
  for (i in seq_along(rest)) {
    out <- c(out, text_block(rest[i]))
    if (i <= length(blocks)) {
      body <- sub("```$", "", sub("^```[A-Za-z]*[ \t]*\n", "", blocks[i], perl = TRUE))
      out <- c(out, paste0("<pre>", esc(trimws(body, which = "right")), "</pre>"))
    }
  }
  paste(out, collapse = "\n")
}

# ---------------------------------------------------------------------------
# Help mode text
#
# HELP_TEXTS     — one entry per button id that is visible on the main screen.
# HELP_TAB_TEXTS — one entry per tab title.
# HELP_EXEMPT    — ids that deliberately have no entry above (with the reason).
#
# tools/check_help.R compares these tables with the buttons and tabs defined in
# this file and fails if one is missing, so run it before every release and
# whenever a button, tab or label is added, renamed or removed.
# ---------------------------------------------------------------------------

HELP_TEXTS <- list(
  help_toggle = paste(
    "You are in Help mode. Click any button or tab to read what it does.",
    "Press the ? button again to return to normal."
  ),
  ask_plain = paste(
    "Answer — Ask a question and get a plain-English answer rather than code.",
    "Classmate answers from what it knows about your objects and files where it can; for",
    "questions that need a calculation it runs one behind the scenes, reads the summary",
    "results and explains them to you. The answer appears in the Answer tab, and \"Show my",
    "working\" under it reveals the code and output used. The Code, R output and Past code",
    "tabs are replaced by Answer and Past answers while you use it, and Run, Explain, Fix,",
    "Load Script and Save Code Block are switched off. Use Ask for Code to get them back.",
    "To produce its answer, summary results (not individual records) are sent to Claude;",
    "names and identifiers in your data are replaced by placeholders and filled back in",
    "only on your own screen. It will not list individual rows or records."
  ),
  ask_code = paste(
    "Ask for Code — Send a prompt to the AI and have the generated code placed",
    "in the Code editor for you to inspect before running. Press Run when you are",
    "ready to execute it. Use Ask for Code when you want to review or edit the code",
    "first. (If AutoRun Code is ticked the code is run for you straight away.)"
  ),
  autorun_code = paste(
    "AutoRun Code — When ticked, code generated by Ask for Code is run automatically as soon as it",
    "appears in the Code editor, without you pressing Run. It is off by default, so that you can",
    "read and check the code first. It applies to Ask for Code only (not Fix), and the result",
    "appears on the R output tab as usual."
  ),
  clear_prompt = paste(
    "Clear — Erases all text in the prompt box. When Classmate first opens it is",
    "highlighted yellow: press it to clear the data-protection reminder, which",
    "unlocks the other buttons."
  ),
  preferences = paste(
    "Preferences — Opens the preferences panel, where you can load a new key file from your",
    "instructor (from your computer or a web address), delete the saved key file (after a",
    "confirmation; Classmate then asks for a key again), and set your preferred",
    "response language, coding style (base R, tidyverse, data.table), plotting",
    "library, mapping package, image export format, quality and size, and comment",
    "density. The Claude model in use (Sonnet 5.5) and the maximum code length are",
    "shown there too, but they cannot be changed (the code length is set by your key)."
  ),
  file_select = paste(
    "Import files — Opens a file browser so you can bring files from your working",
    "directory into the conversation as background context. An optional box at the",
    "bottom of the window lets you filter the list by name. The files are not loaded",
    "into R: the AI is told the file names and paths, and the column names of data",
    "files, so that its code is accurate (for example, when reading your data). The",
    "data values themselves are not sent."
  ),
  add_objects = paste(
    "Select objects — Lets you choose objects from your current R workspace to share",
    "as context with the AI. You can narrow the list by name, or by type, using R",
    "classes (such as sf or integer) or plain words (such as mappable, data table",
    "or model). A structural summary of each object is shared — names, types,",
    "dimensions and column names — never the data values."
  ),
  load_package_data = paste(
    "Load Package Data — Opens a list of every installed R package that comes with datasets or spatial files.",
    "Pick a package to see its datasets (with their type and size) and any shapefiles, GeoPackages or",
    "GeoJSON files it ships (read in with sf when added), filter them by name or type,",
    "tick the ones you want, and go on to tick datasets in other packages too. Add puts the",
    "ticked datasets in your R workspace and adds them to the context for your prompt; the window",
    "stays open so you can add more, and Close ends it. If you already have an object with the same",
    "name you are asked before it is replaced."
  ),
  load_workspace = paste(
    "Load Workspace — Loads a saved R workspace file (.RData, .rda or .rds) into",
    "your current R session, making its objects available as context."
  ),
  remove_context = paste(
    "Remove deselected — Everything added for context starts with a green tick, meaning it is used.",
    "Click a tick to deselect that item: it becomes a red cross, is struck through and greyed, and is",
    "not passed to your prompts. Press this to remove the deselected ones from the context for good",
    "(click the cross again to bring one back).",
    "This link appears above the list once you have added something."
  ),
  remove_all_context = paste(
    "Remove all — Clears all attached files and objects from the context.",
    "This link appears above the list once you have added something."
  ),
  mode_switch = paste(
    "Code / Answer switch — Appears at the end of the tab bar once you have both code and an",
    "answer. The slider shows which you are looking at (Code on the left, Answer on the right):",
    "click it to switch between the code tabs and the Answer tabs; the Past prompts tab then opens",
    "with the prompt behind what you are now viewing highlighted (none for a loaded script). It also",
    "moves by itself when you use Ask for Code (to Code) or the Answer button (to Answer)."
  ),
  run_code = paste(
    "Run — Executes the code currently in the editor. Results and any plots",
    "appear in the R output tab. If the code runs successfully it is added to",
    "Past Code and your code log. (Not available while the Answer view is showing.)"
  ),
  explain_code = paste(
    "Explain — Asks the AI to explain the current code or R output. On the",
    "Code tab it explains what the code does; on the R output tab it interprets",
    "what the results mean. If you have highlighted a passage in an explanation",
    "window, it unpacks that specific part in more detail. (Not available while the",
    "Answer view is showing.)"
  ),
  fix_code = paste(
    "Fix — Available after a failed run. Asks the AI to diagnose the error and",
    "produce corrected code. The fix is applied to the editor and run automatically.",
    "If the fix does not work you can try again, or cancel to restore the original code.",
    "(Not available while the Answer view is showing.)"
  ),
  load_script = paste(
    "Load Script — Opens a file browser so you can load an existing R script",
    "from your project folder into the Code editor. Only available on the Code tab.",
    "Scripts longer than the code-length limit set by your key will be refused.",
    "(Not available while the Answer view is showing.)"
  ),
  save_block = paste(
    "Save Code Block — Saves the code currently in the editor to a named .R",
    "file in your project folder (using the code description as the filename) and",
    "opens it in RStudio. (Not available while the Answer view is showing.)"
  ),
  view_log = paste(
    "View Code Log — Opens a read-only window listing the code that has been run successfully in",
    "this conversation, each piece with the prompt that produced it. Press Save in that window to",
    "save the log as an R Notebook (.Rmd) in your project folder and open it in RStudio, as",
    "happens with Pause, Quit with Code Log and New conversation, but part-way through. You can",
    "carry on in Classmate afterwards; a later save contains everything so far."
  ),
  quick_console = paste(
    "Quick Console — Opens a small R console inside the app where you can run",
    "R code directly, inspect objects, or create new variables — all within the",
    "same workspace and project folder as the main app. The rest of the app is",
    "frozen while the Quick Console is open. Close it when you are done to return",
    "to the app. Typing q() or quit() will just close the Quick Console, not end",
    "your R session, and Classmate's own functions (talk(), whisper() and so on)",
    "are not available there — run those from your main R console.",
    "Along the bottom of the Quick Console, Clear output wipes the displayed history;",
    "Clear Workspace removes every object from your R workspace — the equivalent of",
    "rm(list = ls()) — and cannot be undone; and Change Working Directory lets you",
    "pick a new working directory in a point-and-click folder browser. Typing",
    "setwd() in the console does the same. The folder you choose is used for running",
    "code and saving files, and your R session (for example RStudio) is switched to it",
    "when you leave Classmate."
  ),
  pause_app = paste(
    "Pause — Saves your entire session — conversation history, code, context, and",
    "preferences — and closes Classmate. Your code log is saved too. Reopen the app",
    "with talk() to pick up exactly where you left off.",
    "Use this if you want to take a longer break and return to the same conversation later.",
    "For a quick step out to run some R code, use Quick Console instead."
  ),
  new_conversation = paste(
    "New Conversation — Starts a fresh session, clearing the Code editor,",
    "R output, Past code, and Past prompts. The current conversation is saved",
    "to Past conversations and the code log is saved automatically.",
    "Each new conversation counts towards the limit set by your key; the number",
    "remaining is shown beneath this button."
  ),
  quit = paste(
    "Quit with Code Log — Closes Classmate and saves your code log as an R Notebook",
    "(.Rmd), which opens in your editor. Your session is not kept for resuming (use",
    "Save for that), and your R workspace is not affected."
  ),

  # Buttons inside Explanation and Fix windows
  tell_more = paste(
    "Tell Me More — Available in Explanation windows after highlighting a",
    "passage of text. Asks the AI to go a little deeper into just that passage —",
    "clarifying it, adding detail, or giving a brief example — without",
    "introducing new topics."
  ),
  explain_regenerate = paste(
    "Regenerate — Requests a fresh explanation at the currently selected detail",
    "level. Useful if you have changed the level dropdown since the last explanation",
    "was generated."
  ),
  explain_close = "Close — Closes the Explanation window.",
  fix_cancel    = "Cancel — Abandons the current fix attempt and restores the original code.",
  fix_retry     = "Continue Fixing — Asks the AI to try again after an unsuccessful fix attempt."
)

HELP_TAB_TEXTS <- list(
  "Your prompt" = paste(
    "Your Prompt tab — This is where you type your question or instruction for the AI.",
    "Once submitted with Answer or Ask for Code, the prompt box clears ready for the next step."
  ),
  "Past prompts" = paste(
    "Past Prompts tab — Shows a history of all the prompts you have sent to the AI",
    "in this conversation, marked Answer or Code according to what they produced; code added",
    "to save a plot appears as an Export entry. Use the Recall button beside an entry to bring",
    "it all back: the prompt returns to the prompt box (blank for an Export entry), and you are",
    "taken back to its answer, or to its code with the R output, plots and warnings it produced."
  ),
  "Past conversations" = paste(
    "Past Conversations tab — Appears once you have started a New Conversation and",
    "lists your earlier conversations. Choose one to restore its prompts, code and",
    "output; your current conversation is saved first."
  ),
  "Code" = paste(
    "Code tab — Contains the R code editor. Code generated by Ask for Code appears here",
    "for you to review. You can also type or paste code directly. Press Run to execute it."
  ),
  "R output" = paste(
    "R Output tab — Displays the results of running your code: printed values, tables,",
    "statistical summaries, and plots. When more than one type of output is produced,",
    "separate Plots, Results, and Warnings sub-tabs appear. Click a plot to enlarge it; in",
    "that window you can choose a file name, format, quality and size and press Add to code, which",
    "appends the R code that saves the plot to the end of the Code window."
  ),
  "Past code" = paste(
    "Past Code tab — A record of the code in this conversation, newest first. A block marked",
    "(confirmed) has been run successfully; one marked (unconfirmed) was generated or loaded",
    "but has not been run successfully, and is replaced by a confirmed entry once it is. Use",
    "the Recall button next to any entry to reload that code into the editor. A confirmed",
    "entry also brings back its R output, plots and warnings; an unconfirmed one brings back",
    "the changes from the previous version, if it had them, but no output."
  ),
  "Answer" = paste(
    "Answer tab — Shows the plain-English answer to your last question asked with the Answer button.",
    "Open \"Show my working\" under the answer to see the code Classmate ran and what it",
    "produced. This tab replaces Code, R output and Past code until you use Ask for Code."
  ),
  "Past answers" = paste(
    "Past Answers tab — Appears once you have asked more than one question with the",
    "Answer button, and lists the earlier questions from this conversation, newest first.",
    "Press Recall beside a question to bring its answer back into the Answer tab."
  ),
  "Changes" = paste(
    "Changes tab — Appears when the AI makes targeted edits to existing code.",
    "Shows the new version alongside the previous version, with changed lines",
    "highlighted in yellow, so you can see exactly what was altered."
  ),
  "Plots" = paste(
    "Plots sub-tab — Image output from your code, shown as thumbnails.",
    "Click any thumbnail to expand it to full size."
  ),
  "Results" = paste(
    "Results sub-tab — The printed text output from running the code: values,",
    "data summaries, model output, and any messages printed to the console."
  ),
  "Warnings" = paste(
    "Warnings sub-tab — Any warning messages produced when the code ran.",
    "Warnings do not stop the code from running but may indicate something",
    "worth checking, such as missing values or a coordinate reference system mismatch."
  )
)

# Buttons that are on the main screen but are handled by their own observer
# (they open their own explanatory window), so need no entry in HELP_TEXTS.
# "tell_more_ok" is the OK button of the "More detail" popup, which sits above
# the help overlay and so behaves normally in Help mode.
# (pkgdata_*: buttons that live only inside the Load Package Data pop-up, where Help mode is off)
HELP_EXEMPT <- c("usage_help", "about_classmate", "tell_more_ok",
                 "pkgdata_tick_all", "pkgdata_untick_all", "pkgdata_add")

# Generated ids that follow a pattern (e.g. one Recall button per past code block).
HELP_ID_PATTERNS <- list(
  "^repeat_code_" = paste(
    "Recall (Past code) — Reloads this earlier code block into the Code editor so you can",
    "run it again or use it as a starting point. If the run of that code was captured, its",
    "R output, plots and warnings come back with it. You are always taken to the Code tab, and",
    "the prompt box is left as it is; recall a prompt from Past prompts to bring its text back."
  ),
  "^recall_unconf_" = paste(
    "Recall (Past code, unconfirmed) — Reloads this earlier code, which was generated or loaded but",
    "has not been run successfully, into the Code editor, with the changes from the previous version if it",
    "had any. There is no R output to bring back until it has been run."
  ),
  "^recall_prompt_" = paste(
    "Recall (Past prompts) — Brings back this earlier prompt and everything it produced: the",
    "prompt returns to the prompt box (blank for an Export entry), and you are taken back to",
    "its answer, or to its code with the R output, plots and warnings if it was run."
  ),
  "^recall_answer_" = paste(
    "Recall (Past answers) — Brings this earlier answer back into the Answer tab. The answer it",
    "replaces moves into the list, so nothing is lost."
  ),
  "^recall_conv_" = paste(
    "Recall (Past conversations) — Restores this earlier conversation (its prompts, code,",
    "answers and context) after saving the current one."
  )
)

server <- function(input, output, session) {

  # Show a clean "session ended" page, then stop after 400 ms so the JS has
  # time to render before the WebSocket drops.
  do_quit <- function() {
    runjs("document.getElementById('session-ended-overlay').classList.add('visible');")
    later::later(function() stopApp(), delay = 0.4)
  }

  # Drop the student's API key when the session ends (covers Quit button, Pause, browser-tab close
  # and network drop; talk() also drops it if R is interrupted). A personal ANTHROPIC_API_KEY set
  # by the user is left alone.
  session$onEnded(function() {
    classmate:::cm_clear_key()
    # Remove any package-bound symbols the app may have left in .GlobalEnv
    # (e.g. q/quit/stopApp/talk shadowed during Quick Console if R was restarted
    # while the modal was open). Prevents "package:X may not be available" warning
    # when R auto-saves .RData on exit.
    for (nm in c("q", "quit", "stopApp", "ask", "talk")) {
      tryCatch({
        if (exists(nm, envir = .GlobalEnv, inherits = FALSE)) {
          fn <- get(nm, envir = .GlobalEnv, inherits = FALSE)
          if (is.function(fn) && !is.primitive(fn) &&
              identical(environmentName(environment(fn)), "R_GlobalEnv") == FALSE)
            rm(list = nm, envir = .GlobalEnv)
        }
      }, error = function(e) invisible(NULL))
    }
  })

  show_student_disclaimer <- function() {
    showModal(modalDialog(
      title = "Before you begin",
      tags$p(
        "classmate uses Anthropic's Claude AI (operated by Anthropic, Inc., USA)",
        "to help you with your R code. Before continuing, please note:"
      ),
      tags$ul(
        tags$li(
          "Your prompts and any R code you write are sent to Anthropic's servers",
          "for processing. classmate is designed to minimise what is shared —",
          "only your questions, code, and variable names are transmitted;",
          "your actual data values are not. The one exception is the Answer button:",
          "to write its answer, Classmate may run a short calculation on your data",
          "and send the summary results (such as counts or averages, never individual",
          "records) to Claude."
        ),
        tags$li(
          tags$strong(style = "color: #8b0000;",
            "Do not include personal information about yourself or others in your prompts.")
        ),
        tags$li(
          tags$strong(style = "color: #8b0000;",
            "Do not use classmate with datasets that contain identifiable personal data.")
        ),
        tags$li(
          "Use of classmate must comply with your university's policies on the",
          "use of AI tools in academic work. If you are unsure whether AI",
          "assistance is permitted for a particular assessment, check with your",
          "module leader before proceeding."
        ),
        tags$li(
          "Anthropic does not use API interactions to train its models.",
          "Data is retained for up to 30 days for safety monitoring and then deleted."
        )
      ),
      tags$p(tags$em("By continuing, you acknowledge that you have read and understood the above.")),
      footer = tagList(
        actionButton("disclaimer_ok",   "I understand", class = "btn-primary"),
        actionButton("disclaimer_quit", "Quit",         class = "btn-danger")
      ),
      easyClose = FALSE
    ))
  }

  observeEvent(input$disclaimer_ok,   removeModal())
  observeEvent(input$disclaimer_quit, { removeModal(); do_quit() })

  # --- App mode and usage state ----------------------------------------------
  # Restore student mode from the previously saved config (survives restarts).
  .saved_cfg       <- tryCatch(readRDS(active_cfg_path()), error = function(e) NULL)
  .resuming_pause  <- file.exists(pause_file())   # checked before pause state is consumed
  if (.resuming_pause) {
    session$onFlushed(function() {
      shinyjs::runjs("
        var s = document.getElementById('classmate-splash');
        if (s) s.classList.add('hidden');
      ")
    }, once = TRUE)
  }
  cost_limit_val        <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$cost_limit        else NULL)
  reset_period_val      <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$reset_period      else "weekly")
  final_expiry_val      <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$final_expiry      else NULL)
  max_conversations_val <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$max_conversations else NULL)
  # Code length is set by the key (Inf = unlimited) and is not a user choice.
  # Keys made before this field existed fall back to 50 lines.
  max_lines_cap_val     <- reactiveVal(if (!is.null(.saved_cfg)) (.saved_cfg$max_lines %||% 50) else 50)
  usage_log_rv    <- reactiveVal(read_usage_log())

  # Show disclaimer once per R session for student users (not on pause resume).
  # The option resets automatically when R is restarted.
  .disclaimer_needed <- !is.null(.saved_cfg) &&
                        !.resuming_pause &&
                        !isTRUE(getOption(".classmate_disclaimer_shown"))
  if (.disclaimer_needed) {
    session$onFlushed(function() {
      show_student_disclaimer()
      options(.classmate_disclaimer_shown = TRUE)
    }, once = TRUE)
  }

  # On every fresh open (not a pause-resume), pre-fill the prompt box with the
  # data-protection notice and freeze Ask/Ask for Code until the student clears it.
  .PROTECTION_NOTICE <- paste0(
    "Enter your prompt here. Always prioritise data protection. Never include ",
    "personal data or information in your prompts, and do not make any reference ",
    "to real individuals.\n\n𝗣𝗿𝗲𝘀𝘀 𝗖𝗹𝗲𝗮𝗿 𝘁𝗼 𝗰𝗼𝗻𝘁𝗶𝗻𝘂𝗲."  # bold: a text box cannot hold bold text, so Unicode bold letters

  )
  if (!.resuming_pause) {
    session$onFlushed(function() {
      updateTextAreaInput(session, "prompt", value = .PROTECTION_NOTICE)
    }, once = TRUE)
  }

  current_spend <- reactive({
    period_spend(usage_log_rv(), reset_period_val())
  })

  key_expired <- reactive({
    expiry <- final_expiry_val()
    if (is.null(expiry)) return(FALSE)
    Sys.Date() > as.Date(expiry)
  })

  quota_exceeded <- reactive({
    if (key_expired()) return(TRUE)
    limit <- cost_limit_val()
    if (is.null(limit)) return(FALSE)
    current_spend() >= limit
  })

  show_quota_modal <- function() {
    if (isTRUE(key_expired())) {
      expiry <- final_expiry_val()
      showModal(modalDialog(
        title     = "Key expired",
        tags$p("Your Classmate key expired on ",
               tags$strong(format(as.Date(expiry), "%d %B %Y")), "."),
        tags$p("Please contact your instructor for a new key. If you have been given a new key file,",
               "you can load it under Preferences."),
        easyClose = FALSE,
        footer    = tagList(
          actionButton("quota_open_prefs", "Open Preferences", class = "btn-primary"),
          modalButton("Close")
        )
      ))
      return(invisible(NULL))
    }
    rt  <- next_reset_time(isolate(usage_log_rv()), isolate(reset_period_val()))
    reset_line <- if (!is.null(rt))
      paste0("Your allowance is scheduled to top up at approximately ",
             format(rt, "%H:%M on %d %B %Y"), ".")
    else
      "Contact your instructor if you need more information about when your allowance refreshes."
    showModal(modalDialog(
      title     = "Usage allowance reached",
      tags$p("You have used your full allowance for this period. ",
             "The Answer, Ask for Code, Explain, and Fix buttons are now disabled."),
      tags$p(reset_line),
      tags$p("Any code already in the editor can still be run and edited."),
      tags$p("If you have been given a new key file, you can load it under Preferences."),
      easyClose = FALSE,
      footer    = tagList(
        actionButton("quota_open_prefs", "Open Preferences", class = "btn-primary"),
        modalButton("Close")
      )
    ))
  }
  observeEvent(input$quota_open_prefs, show_preferences_modal())

  # Proactively show quota modal the moment a call tips the student over their limit.
  # Uses a shadow reactiveVal so the modal fires only on the FALSE→TRUE transition,
  # not on every re-render, and resets if the allowance refreshes mid-session.
  .prev_quota_exceeded <- reactiveVal(FALSE)
  observe({
    now_exceeded <- isTRUE(quota_exceeded())
    if (now_exceeded && !.prev_quota_exceeded()) show_quota_modal()
    .prev_quota_exceeded(now_exceeded)
  })

  # Periodically re-evaluate quota (catches rolling-window resets)
  observe({
    invalidateLater(5 * 60 * 1000, session)   # every 5 minutes
    usage_log_rv(read_usage_log())
  })

  record_usage <- function(cost_usd) {
    new_log <- append_usage_entry(cost_usd)
    usage_log_rv(new_log)
  }

  # --- API key gate ----------------------------------------------------------
  # If a student key was saved from a prior session, restore it into the
  # environment so the app starts ready without requiring re-upload.
  if (!is.null(.saved_cfg) && nzchar(.saved_cfg$api_key %||% "")) {
    classmate:::cm_set_key(.saved_cfg$api_key)
  }
  key_is_set   <- reactiveVal(!is.null(.saved_cfg) && nzchar(.saved_cfg$api_key %||% ""))

  output$has_key <- reactive({ key_is_set() })
  outputOptions(output, "has_key", suspendWhenHidden = FALSE)

  show_key_error <- function() {
    showModal(modalDialog(
      title = "API key not recognised",
      paste("The key could not be verified — the API returned no response.",
            "Please check the key and try again."),
      footer = actionButton("key_error_ok", "OK", class = "btn-primary")
    ))
  }

  show_load_key_file_modal <- function() {
    showModal(modalDialog(
      title = "Load key file",
      tags$p("Select the classmate.key file provided by your instructor (you will be asked for its PIN).",
             "If it differs from the currently loaded key, your usage quota",
             "will be reset in line with the new key's settings."),
      fileInput("mid_session_key_file", NULL, accept = ".key", width = "100%",
                buttonLabel = "Browse...", placeholder = "classmate.key"),
      web_key_ui("web_key_url", "load_web_key_btn"),
      footer = actionButton("load_key_cancel", "Cancel")
    ))
  }

  load_key_from_web <- function(url, on_success) {
    res <- NULL
    withProgress(message = "Fetching key...", value = 0.5, { res <- fetch_key_from_web(url) })
    if (!is.null(res$error)) {
      showModal(modalDialog(
        title = "Could not load key from the web",
        res$error,
        footer = modalButton("OK")
      ))
      return(invisible(NULL))
    }
    tmp <- tempfile(fileext = ".key")
    saveRDS(res$payload, tmp)
    on.exit(unlink(tmp), add = TRUE)
    load_key_file(tmp, on_success = on_success)
  }

  observeEvent(input$load_web_key_btn, {
    load_key_from_web(input$web_key_url, on_success = function() removeModal())
  })
  observeEvent(input$load_key_web_btn, {
    load_key_from_web(input$key_web_url, on_success = function() key_is_set(TRUE))
  })

  observeEvent(input$pref_load_key, show_load_key_file_modal())

  # Delete the saved key: Classmate goes back to the "load a key file" screen, as if no key had been
  # set (usage so far is kept, so deleting and re-loading the same key does not reset the allowance)
  observeEvent(input$pref_delete_key, {
    if (!file.exists(active_cfg_path())) {
      showNotification("There is no saved key file to delete.", type = "message", duration = 5)
      return(invisible(NULL))
    }
    showModal(modalDialog(
      title = "Delete the key file?",
      tags$p("Are you sure? The key saved on this computer will be deleted and Classmate will ask for a key file",
             "again (and for its PIN), as if no key had been set."),
      tags$p(em("Your usage so far is kept.")),
      footer = tagList(actionButton("delete_key_confirm", "Delete", class = "btn-danger"),
                       actionButton("delete_key_cancel", "Cancel")),
      easyClose = FALSE
    ))
  })
  observeEvent(input$delete_key_cancel, show_preferences_modal())
  observeEvent(input$delete_key_confirm, {
    tryCatch(unlink(active_cfg_path()), error = function(e) NULL)
    classmate:::cm_clear_key()
    cost_limit_val(NULL); reset_period_val("weekly"); final_expiry_val(NULL)
    max_conversations_val(NULL); max_lines_cap_val(50)
    key_status_msg("The key file has been deleted. Load a key file to continue.")
    shinyjs::reset("key_file")           # so that the same file can be chosen again
    key_is_set(FALSE)
    removeModal()
  })
  observeEvent(input$load_key_cancel, show_preferences_modal())

  observeEvent(input$mid_session_key_file, {
    req(input$mid_session_key_file)
    load_key_file(input$mid_session_key_file$datapath, on_success = function() {
      removeModal()
    })
  })

  # A key file is "sealed" with a PIN (see R/key_pin.R). The PIN is asked for once, when a key is
  # first loaded on this computer; the same key is not asked for again.
  pending_key <- reactiveVal(NULL)    # list(x = sealed key, on_success = function)
  pin_msg     <- reactiveVal("")
  output$key_pin_msg <- renderUI({
    m <- pin_msg()
    if (nzchar(m)) tags$p(m, style = "color: #b22222; margin-top: 8px;")
  })
  show_pin_modal <- function() {
    showModal(modalDialog(
      title = "Enter your PIN",
      tags$p("This key is protected by a 6-digit PIN, which your instructor will have given you.",
             "You only need to enter it once."),
      passwordInput("key_pin", NULL, placeholder = "6-digit PIN", width = "100%"),
      uiOutput("key_pin_msg"),
      footer = tagList(actionButton("key_pin_ok", "Unlock", class = "btn-primary"),
                       actionButton("key_pin_cancel", "Cancel")),
      easyClose = FALSE
    ))
  }
  load_key_file <- function(path, on_success = function() invisible(NULL)) {
    x    <- tryCatch(readRDS(path), error = function(e) NULL)
    kind <- classmate:::classmate_key_kind(x)
    if (identical(kind, "legacy")) {
      showModal(modalDialog(
        title = "Key file no longer supported",
        "This key file is from an older version of Classmate. Please ask your instructor for a new one.",
        footer = modalButton("OK")))
      return(invisible(NULL))
    }
    if (identical(kind, "invalid")) {
      showModal(modalDialog(
        title = "Invalid key file",
        "This file does not appear to be a valid classmate key file.",
        footer = modalButton("OK")
      ))
      return(invisible(NULL))
    }
    cfg <- tryCatch(readRDS(active_cfg_path()), error = function(e) NULL)
    if (!is.null(cfg) && !is.null(x$key_id) && identical(cfg$key_id, x$key_id))
      return(activate_key(cfg, on_success))       # already unlocked on this computer
    if (classmate:::classmate_pin_locked()) {
      showModal(modalDialog(title = "Too many incorrect PINs",
                            classmate:::CLASSMATE_PIN_LOCKED_MSG, footer = modalButton("OK")))
      return(invisible(NULL))
    }
    pending_key(list(x = x, on_success = on_success))
    pin_msg("")
    show_pin_modal()
    invisible(NULL)
  }
  observeEvent(input$key_pin_ok, {
    pk <- pending_key(); req(pk)
    if (classmate:::classmate_pin_locked()) {
      pin_msg(classmate:::CLASSMATE_PIN_LOCKED_MSG); return(invisible(NULL))
    }
    payload <- classmate:::classmate_open_key(pk$x, trimws(input$key_pin %||% ""))
    if (is.null(payload)) {
      classmate:::classmate_pin_fail()
      pin_msg(if (classmate:::classmate_pin_locked()) classmate:::CLASSMATE_PIN_LOCKED_MSG
              else paste0("That PIN is not correct. ", classmate:::classmate_pin_tries_left(),
                          " attempt(s) left."))
      shinyjs::reset("key_pin")
      return(invisible(NULL))
    }
    pending_key(NULL)
    removeModal()
    activate_key(payload, pk$on_success)
  })
  observeEvent(input$key_pin_cancel, { pending_key(NULL); removeModal() })

  activate_key <- function(payload, on_success = function() invisible(NULL)) {
    withProgress(message = "Checking key...", value = 0.5, {
      ok <- validate_api_key(payload$api_key)
    })
    if (!ok) { show_key_error(); return(invisible(NULL)) }

    # If the key_id differs from the active config, this is a newly issued key
    # from the instructor — reset the usage log so the student gets a fresh quota.
    # (The key the usage belongs to is the saved key, or, if that was deleted, the one recorded in the
    # usage log, so deleting and re-loading the same key does not give a fresh allowance.)
    cfg     <- tryCatch(readRDS(active_cfg_path()), error = function(e) NULL)
    prev_id <- cfg$key_id %||% attr(read_usage_log(), "key_id")
    if (!is.null(payload$key_id) && !identical(prev_id, payload$key_id)) {
      tryCatch(unlink(usage_log_path()), error = function(e) invisible(NULL))
      usage_log_rv(list())
    }

    # Save active config for classmate_config_show()
    tryCatch(classmate:::classmate_save_state(payload, active_cfg_path()), error = function(e) invisible(NULL))
    cost_limit_val(       payload$cost_limit)
    reset_period_val(     payload$reset_period)
    final_expiry_val(     payload$final_expiry)
    max_conversations_val(payload$max_conversations)
    new_cap <- payload$max_lines %||% 50
    max_lines_cap_val(new_cap)
    prefs$max_lines <- new_cap
    on_success()
    if (!isTRUE(getOption(".classmate_disclaimer_shown"))) {
      show_student_disclaimer()
      options(.classmate_disclaimer_shown = TRUE)
    }
  }

  observeEvent(input$key_file, {
    req(input$key_file)
    load_key_file(input$key_file$datapath, on_success = function() key_is_set(TRUE))
  })

  observeEvent(input$quit_setup, do_quit())

  observeEvent(input$key_error_ok, removeModal())

  key_status_msg <- reactiveVal("")
  output$key_status_message <- renderUI({
    msg <- key_status_msg()
    if (!nzchar(msg)) return(NULL)
    div(class = "alert alert-warning", style = "margin-bottom: 10px;", msg)
  })

  observe({
    invalidateLater(KEY_CHECK_INTERVAL_MS, session)
    req(key_is_set())
    isolate({
      # Only a genuine refusal of the key signs the student out; a network blip must not
      if (identical(claude_key_status(), "invalid")) {
        key_status_msg(paste(
          "Your key is no longer valid — it may have been suspended or deleted.",
          "Please load a new key file, or load the same one again once it has been reactivated."
        ))
        key_is_set(FALSE)
      }
    })
  })

  # --- Usage bar -------------------------------------------------------------
  output$usage_bar_ui <- renderUI({
    if (!isTRUE(key_is_set())) return(NULL)
    limit <- cost_limit_val()
    spend <- current_spend()
    if (is.null(limit)) {
      spend_str <- paste0("Session spend: $", sprintf("%.3f", spend))
      tagList(
        div(class = "usage-text-normal",
          style = "font-size: 0.85em; color: #444;",
          spend_str),
        actionButton("usage_help", HTML(spend_str),
          class = "usage-text-help")
      )
    } else {
      pct_used      <- min(spend / limit, 1)
      pct_remaining <- max(1 - pct_used, 0)
      bar_colour    <- if (pct_used < 0.5) "#28a745"
                       else if (pct_used < 0.8) "#fd7e14"
                       else "#dc3545"
      reset_period <- reset_period_val()
      reset_label  <- tryCatch({
        rt <- next_reset_time(isolate(usage_log_rv()), reset_period)
        if (grepl("^rolling", reset_period)) "rolling window"
        else paste0("resets ", format(rt, "%d %b %Y"))
      }, error = function(e) "")
      expiry_str <- if (!is.null(final_expiry_val()))
        paste0("expires ", format(as.Date(final_expiry_val()), "%d/%m/%y"))
      else NULL
      # Build button label lines (same content as the text column)
      btn_lines <- paste0(round(pct_remaining * 100), "% remaining")
      if (nzchar(reset_label)) btn_lines <- paste0(btn_lines, "<br>", reset_label)
      if (!is.null(expiry_str))  btn_lines <- paste0(btn_lines, "<br>", expiry_str)
      div(style = "display: flex; align-items: center; gap: 8px;",
        div(style = "background: #ddd; border-radius: 4px; height: 12px; width: 120px; flex-shrink: 0; display: flex; justify-content: flex-end;",
          div(style = paste0("background: ", bar_colour, "; width: ",
                             round(pct_remaining * 100), "%; height: 100%; border-radius: 4px;"))
        ),
        div(class = "usage-text-normal",
          style = "display: flex; flex-direction: column; line-height: 1.25;",
          tags$span(
            style = "color: #222; font-size: 0.82em; white-space: nowrap;",
            paste0(round(pct_remaining * 100), "% remaining")
          ),
          if (nzchar(reset_label)) tags$span(
            style = "color: #888; font-size: 0.75em; white-space: nowrap;",
            reset_label
          ),
          if (!is.null(expiry_str)) tags$span(
            style = "color: #888; font-size: 0.75em; white-space: nowrap;",
            expiry_str
          )
        ),
        actionButton("usage_help", HTML(btn_lines),
          class = "usage-text-help")
      )
    }
  })
  outputOptions(output, "usage_bar_ui", suspendWhenHidden = FALSE)

  # --- R console output tab --------------------------------------------------
  console_output_rv <- reactiveVal(character(0))
  warnings_rv       <- reactiveVal(character(0))
  last_error_msg    <- reactiveVal("")
  plot_files_rv     <- reactiveVal(character(0))
  diff_rv            <- reactiveVal(NULL)   # list(old_lines, new_lines, changed) or NULL
  changes_tab_shown  <- reactiveVal(FALSE)

  show_changes_tab <- function() {
    if (!changes_tab_shown()) {
      insertTab("main_tabs",
        tabPanel("Changes", uiOutput("diff_ui")),
        target = "Code", position = "after", select = FALSE)   # tab order: Code, Changes, R output, Past code
      changes_tab_shown(TRUE)
    }
    goto_tab("Changes")
  }

  hide_changes_tab <- function() {
    if (changes_tab_shown()) {
      if (isTRUE(isolate(input$main_tabs) == "Changes"))
        goto_tab("Code")
      removeTab("main_tabs", "Changes")
      changes_tab_shown(FALSE)
    }
    diff_rv(NULL)
  }

  past_convs_tab_shown <- reactiveVal(FALSE)

  show_past_convs_tab <- function() {
    if (!past_convs_tab_shown()) {
      insertTab("prompt_tabs",
        tabPanel("Past conversations",
          tags$div(
            style = paste0(
              "margin-top: 8px; height: 108px; overflow-y: auto;",
              "border: 1px solid #e3e3e3; border-radius: 4px;",
              "padding: 4px 8px; background: #fafafa;"
            ),
            uiOutput("past_conversations_ui")
          )
        ),
        target = "Past prompts", position = "after", select = FALSE)
      past_convs_tab_shown(TRUE)
    }
  }

  hide_past_convs_tab <- function() {
    if (past_convs_tab_shown()) {
      if (isTRUE(isolate(input$prompt_tabs) == "Past conversations"))
        updateTabsetPanel(session, "prompt_tabs", selected = "Your prompt")
      removeTab("prompt_tabs", "Past conversations")
      past_convs_tab_shown(FALSE)
    }
  }

  # Auto-hide tab when the list becomes empty, auto-show when it becomes non-empty
  observe({
    if (length(saved_conversations()) == 0) hide_past_convs_tab()
    else show_past_convs_tab()
  })

  # Generate a short summary from the prompts list (first prompt, truncated)
  make_conversation_summary <- function(prompts, api_key) {
    if (length(prompts) == 0) return("Unnamed conversation")
    # Try a quick Claude call; fall back to using first prompt directly
    first_few <- paste(rev(head(rev(prompts), 3)), collapse = " / ")
    summary_text <- tryCatch({
      resp <- call_claude(
        messages = list(list(role = "user", content = paste0(
          "In one short sentence (max 10 words), describe what this R session was about based on these prompts: ",
          first_few
        ))),
        model        = MODEL_HAIKU,
        max_tokens   = 60,
        api_key      = api_key,
        cache_system = FALSE
      )
      trimws(resp$text)
    }, error = function(e) {
      lbl <- trimws(prompts[[length(prompts)]])
      if (nchar(lbl) > 70) paste0(substr(lbl, 1, 67), "...") else lbl
    })
    summary_text
  }

  # Save current conversation to saved_conversations (prepend = newest first)
  save_current_conversation_if_nonempty <- function() {
    capture_active_code()
    ph <- prompt_history()
    rh <- run_history()
    if (length(ph) == 0 && length(rh) == 0 && length(unconf_code()) == 0) return(invisible(NULL))
    summary <- make_conversation_summary(ph, api_key_val())
    entry <- list(
      id          = as.numeric(Sys.time()),
      summary     = summary,
      prompts     = ph,
      codes       = rh,
      unconf      = unconf_code(),
      log_entries = all_log_entries(),
      files       = unique(selected_files()),
      objects     = unique(selected_objects()),
      answer_latest = answer_latest(),
      answer_hist   = answer_hist(),
      interactions  = interactions(),
      active_code_id = active_code_id()
    )
    saved_conversations(c(list(entry), saved_conversations()))
    show_past_convs_tab()
  }

  output$diff_ui <- renderUI({
    d <- diff_rv()
    if (is.null(d))
      return(tags$p(em("No incremental changes to show."),
                    style = "color: #999; margin: 12px 4px;"))
    make_panel <- function(lines, changed_idx, label) {
      line_divs <- lapply(seq_along(lines), function(i) {
        cls <- if (i %in% changed_idx) "diff-line-changed" else "diff-line-normal"
        tags$div(class = cls, lines[[i]])
      })
      tags$div(
        tags$h6(label),
        tags$div(class = "diff-pre", do.call(tagList, line_divs))
      )
    }
    new_code_text <- paste(d$new_lines, collapse = "\n")
    confirmed <- !is.null(last_run_code()) &&
                 identical(trimws(new_code_text), trimws(last_run_code()))
    new_label <- if (confirmed) "New code (confirmed)" else "New code (not yet run)"
    tags$div(class = "diff-panel",
      make_panel(d$new_lines, d$changed,     new_label),
      make_panel(d$old_lines, d$old_changed, "Previous code")
    )
  })

  # --- Expanded plot: add code that saves it to a file -----------------------------------
  # The expanded plot offers the Preferences image options plus a file name, and appends standard
  # R code (png(), pdf()... around the statements that drew the plot) to the Code window. Running
  # that block runs only the block, not the whole script (see pending_export).
  expanded_plot  <- reactiveVal(NULL)   # full path of the PNG being shown
  pending_export <- reactiveVal(NULL)   # list(snippet = <block appended>, full = <editor text after>)
  export_ext     <- function(fmt) switch(fmt, jpg = "jpg", tif = "tif", fmt)
  observeEvent(input$plot_expand, {
    f <- plot_files_rv()
    f <- f[basename(f) == input$plot_expand]
    req(length(f) == 1)
    expanded_plot(f)
    has_code <- exists(f, envir = plot_code_cache, inherits = FALSE)
    showModal(modalDialog(
      title = "Plot",
      size  = "l",
      tags$div(style = "text-align: center; max-height: 45vh; overflow: auto;",
        tags$img(src = paste0("/classmate_plots/", basename(f)),
                 style = "max-width: 100%; height: auto; border: 1px solid #ddd;")),
      tags$hr(),
      if (has_code) tagList(
        tags$h5("Save this plot to a file"),
        fluidRow(
          column(3, textInput("plot_exp_name", "File name:", value = paste0("plot_", length(plot_files_rv()), "_", match(f, plot_files_rv()), ""))),
          column(3, selectInput("plot_exp_format", "Format:",
            choices = c("png", "jpg", "pdf", "tif", "bmp"), selected = prefs$img_format)),
          column(3, selectInput("plot_exp_quality", "Quality / resolution:",
            choices = c("Low", "Medium", "High"), selected = prefs$img_quality)),
          column(3, selectInput("plot_exp_size", "Target size:",
            choices = c(
              "Journal single col. (90x90 mm)",
              "Journal double col. (180x120 mm)",
              "A4 (210x297 mm)", "A3 (297x420 mm)", "Letter (8.5x11 in)",
              "4x6 in / 10x15 cm", "5x7 in / 13x18 cm", "8x10 in / 20x25 cm",
              "Square 6x6 in"
            ), selected = prefs$img_size))
        ),
        uiOutput("plot_exp_note"),
        tags$p(em("Add to code appends the R code that saves the plot, in your working directory, to the end of the Code window."),
               style = "font-size: 0.85em; color: #888;")
      ) else tags$p(em("Classmate could not work out which lines of code drew this plot, so it cannot write the saving code for it."),
                    style = "font-size: 0.85em; color: #888;"),
      footer = tagList(
        if (has_code) actionButton("plot_exp_add", "Add to code", class = "btn-primary"),
        modalButton("Close")),
      easyClose = TRUE
    ))
  })
  plot_exp_file <- reactive({
    nm  <- trimws(basename(input$plot_exp_name %||% ""))
    fmt <- input$plot_exp_format %||% "png"
    if (!nzchar(nm)) nm <- "plot"
    nm <- sub("\\.[A-Za-z0-9]{1,4}$", "", nm)      # the extension always follows the format
    paste0(nm, ".", export_ext(fmt))
  })
  output$plot_exp_note <- renderUI({
    if (file.exists(file.path(project_root(), plot_exp_file())))
      tags$p(paste0(plot_exp_file(), " already exists in your working directory and will be overwritten."),
             style = "color: #b36b00; font-size: 0.85em;")
  })
  observeEvent(input$plot_exp_add, {
    f <- expanded_plot(); req(f)
    capture_active_code()    # keep the code and output that made the plot, for Recall
    info <- get(f, envir = plot_code_cache)
    base <- sub("[[:space:]]+$", "", input$code_editor %||% "")
    draw <- info$code
    create_line <- NULL
    if (identical(info$kind, "symbol")) {
      draw <- paste0("print(", info$sym, ")")
    } else if (identical(info$kind, "value")) {
      # An unassigned plot object (a ggplot or tmap map typed out): tidy the code so that the object
      # is created once, shown, and then drawn again to the file.
      nm   <- tidy_plot_name(info$cls, base)
      new_base <- NULL
      if (lengths(regmatches(base, gregexpr(info$code, base, fixed = TRUE))) == 1L) {
        at   <- regexpr(info$code, base, fixed = TRUE)
        cand <- paste0(substr(base, 1L, at - 1L), nm, " <- ", info$code, "\nprint(", nm, ")",
                       substr(base, at + nchar(info$code), nchar(base)))
        ok <- tryCatch({ ex <- parse(text = cand, keep.source = FALSE); TRUE }, error = function(e) FALSE)
        if (ok) new_base <- cand
      }
      if (!is.null(new_base)) {
        base <- new_base; draw <- paste0("print(", nm, ")")
        create_line <- paste0(nm, " <- ", info$code)   # the rewritten statement has not been run yet
      } else draw <- paste0("print(", info$code, ")")
    }
    block <- build_plot_export_code(draw, plot_exp_file(), input$plot_exp_format %||% "png",
                                    input$plot_exp_quality %||% "Medium",
                                    input$plot_exp_size %||% prefs$img_size)
    full <- paste0(base, if (nzchar(base)) "\n\n" else "", block)
    # What is run: the block, preceded by the statement that creates the plot object if that was rewritten
    to_run <- if (!is.null(create_line)) paste0(create_line, "\n\n", block) else block
    file   <- plot_exp_file()
    removeModal()
    pending_export(list(snippet = to_run, full = full, file = file))
    old_code <- input$code_editor %||% ""
    updateAceEditor(session, "code_editor", value = full)
    # Show what was added in the Changes tab (however much of a short script it is)
    d <- if (nzchar(trimws(old_code))) compute_diff(old_code, full, max_changed_frac = Inf) else NULL
    diff_rv(d)
    if (!is.null(d)) show_changes_tab() else goto_tab("Code")
    # Adding the export code counts as a prompt of its own (type Export): it is listed in Past prompts
    # and Recall brings the code back, with a blank prompt box
    what  <- sub("^# Save the plot to a file \\((.*)\\)$", "\\1", strsplit(block, "\n", fixed = TRUE)[[1]][1])
    label <- paste0("Save plot as ", file, " (", what, ")")
    last_extracted_code(full); last_prompt_for_code(label); last_code_description(label)
    sq <- add_interaction(list(kind = "code", export = TRUE, prompt = label, code = full,
                               description = label, editor_code = full,
                               console = console_output_rv(), warnings = warnings_rv(),
                               plots = plot_files_rv(), last_run_code = NULL, failed = FALSE,
                               error = "", diff = d, tab = if (!is.null(d)) "Changes" else "Code"))
    active_code_id(sq)
    prompt_history(c(label, prompt_history()))
    if (isTRUE(input$autorun_code)) do_run_export(to_run, full, file)
  })

  output$console_output_ui <- renderUI({
    lines    <- console_output_rv()
    plots    <- plot_files_rv()
    warnings <- warnings_rv()

    has_text     <- length(lines) > 0 && any(nzchar(lines))
    has_plots    <- length(plots) > 0
    has_warnings <- length(warnings) > 0

    if (!has_text && !has_plots && !has_warnings)
      return(tags$p(em("R output will appear here after running code."),
                    style = "color: #999; margin: 4px 0;"))

    # --- Build each panel's content -------------------------------------------
    pre_style <- paste0(
      "font-size: 0.85em; background: #f8f8f8; padding: 8px;",
      "border: 1px solid #e3e3e3; border-radius: 4px;",
      "white-space: pre-wrap; word-break: break-word; margin: 0;"
    )

    results_ui <- if (has_text)
      tags$pre(style = pre_style, paste(lines, collapse = "\n"))

    plots_ui <- if (has_plots) {
      thumb_items <- lapply(seq_along(plots), function(i) {
        src <- paste0("/classmate_plots/", basename(plots[[i]]))
        tags$div(
          style = "display: inline-block; margin: 4px; vertical-align: top; cursor: pointer;",
          tags$img(
            src   = src,
            title = paste("Plot", i, "— click to expand"),
            style = paste0(
              "width: 140px; height: 105px; object-fit: cover;",
              "border: 1px solid #ccc; border-radius: 4px;",
              "transition: box-shadow 0.15s;"
            ),
            onclick = paste0("Shiny.setInputValue('plot_expand', '", basename(plots[[i]]),
                             "', {priority: 'event'});"),
            onmouseover = "this.style.boxShadow='0 2px 8px rgba(0,0,0,0.25)';",
            onmouseout  = "this.style.boxShadow='';"
          )
        )
      })
      tags$div(style = "padding-top: 4px;", do.call(tagList, thumb_items))
    }

    warnings_ui <- if (has_warnings)
      tags$div(
        style = paste0(
          "font-size: 0.85em; background: #fff8e1; padding: 8px;",
          "border: 1px solid #ffe082; border-radius: 4px; color: #7a5700;"
        ),
        lapply(warnings, function(w)
          tags$p(style = "margin: 2px 0;", tags$strong("Warning: "), w)
        )
      )

    # --- Layout: single type = no tabs; multiple types = sub-tabs ------------
    n_types <- sum(has_text, has_plots, has_warnings)

    if (n_types == 1) {
      results_ui %||% plots_ui %||% warnings_ui
    } else {
      tabs <- list()
      if (has_plots)    tabs <- c(tabs, list(tabPanel("Plots",    div(style = "padding-top: 10px;", plots_ui))))
      if (has_text)     tabs <- c(tabs, list(tabPanel("Results",  div(style = "padding-top: 10px;", results_ui))))
      if (has_warnings) tabs <- c(tabs, list(tabPanel("Warnings", div(style = "padding-top: 10px;", warnings_ui))))
      do.call(tabsetPanel, c(tabs, list(type = "tabs")))
    }
  })
  outputOptions(output, "console_output_ui", suspendWhenHidden = FALSE)

  # --- Run / Explain / Fix button state -------------------------------------
  code_running    <- reactiveVal(FALSE)
  last_run_failed <- reactiveVal(FALSE)
  last_run_code   <- reactiveVal(NULL)
  ui_busy         <- reactiveVal(FALSE)
  # TRUE on a fresh open; FALSE on pause-resume. Cleared when the student presses Clear.
  protection_notice_active <- reactiveVal(!.resuming_pause)
  observe({
    if (ui_busy()) {
      for (btn in c("ask_plain", "ask_code", "explain_code", "fix_code", "run_code",
                    "load_script", "save_block", "view_log", "help_toggle",
                    "quick_console", "pause_app", "new_conversation", "quit"))
        shinyjs::disable(btn)
      return()
    }
    has_code   <- nzchar(trimws(input$code_editor %||% ""))
    exceeded   <- isTRUE(quota_exceeded())
    answer_view <- identical(ask_mode(), "answer")
    on_code_tab     <- is.null(input$main_tabs) || isTRUE(input$main_tabs == "Code")
    on_changes_tab  <- isTRUE(input$main_tabs == "Changes")
    has_unrun_code  <- is.null(last_run_code()) ||
                       !identical(trimws(input$code_editor %||% ""), trimws(last_run_code()))
    if (!answer_view && has_code && has_unrun_code && !code_running() && !exceeded && (on_code_tab || on_changes_tab)) {
      enable("run_code")
    } else {
      disable("run_code")
    }
    # Explain: Code or Changes tab with code, R output tab with content, or after a failed run
    on_output_tab  <- isTRUE(input$main_tabs == "R output")
    has_run_output <- length(console_output_rv()) > 0 || length(plot_files_rv()) > 0 || length(warnings_rv()) > 0
    if (!answer_view && !code_running() && !exceeded &&
        ((has_code && (on_code_tab || on_changes_tab)) || last_run_failed() ||
         (on_output_tab && has_run_output))) {
      enable("explain_code")
    } else {
      disable("explain_code")
    }
    if (!answer_view && last_run_failed() && !code_running() && !exceeded)
      enable("fix_code") else disable("fix_code")
    if (!answer_view && on_code_tab && !exceeded && !is.null(last_run_code())) {
      enable("save_block")
    } else {
      disable("save_block")
    }
    if (!answer_view && on_code_tab && !exceeded) {
      enable("load_script")
    } else {
      disable("load_script")
    }
    if (protection_notice_active()) {
      for (btn in c("ask_plain", "ask_code", "explain_code", "fix_code", "run_code",
                    "load_script", "save_block", "view_log", "help_toggle",
                    "quick_console", "pause_app", "new_conversation",
                    "add_objects", "load_package_data", "remove_context", "remove_all_context",
                    "preferences",
                    "file_select", "load_workspace"))
        shinyjs::disable(btn)
      shinyjs::runjs("
        document.getElementById('clear_prompt').style.backgroundColor = '#f1c40f';
        document.getElementById('clear_prompt').style.borderColor = '#d4ac0d';
        document.getElementById('clear_prompt').style.color = '#000000';
      ")
      return()
    }
    shinyjs::runjs("
      document.getElementById('clear_prompt').style.backgroundColor = '';
      document.getElementById('clear_prompt').style.borderColor = '';
      document.getElementById('clear_prompt').style.color = '';
    ")
    for (btn in c("help_toggle", "quick_console", "pause_app", "quit", "view_log",
                  "add_objects", "load_package_data", "preferences",
                  "file_select", "load_workspace"))
      shinyjs::enable(btn)
    if (!exceeded) {
      enable("ask_code")
      enable("ask_plain")
    } else {
      disable("ask_code")
      disable("ask_plain")
    }
    has_context <- length(selected_files()) > 0 || length(selected_objects()) > 0
    if (has_context) {
      enable("remove_context")
      enable("remove_all_context")
    } else {
      disable("remove_context")
      disable("remove_all_context")
    }
    # Disable New conversation until something has been asked, and when limit reached
    max_c <- max_conversations_val()
    conv_limit_hit <- !is.null(max_c) && conv_count_rv() >= max_c
    has_activity <- length(prompt_history()) > 0
    if (conv_limit_hit || !has_activity) {
      disable("new_conversation")
    } else {
      enable("new_conversation")
    }
  })

  # Update Explain button label to reflect whether a selection is active
  observe({
    sel <- trimws(input$code_selection %||% "")
    label <- if (nzchar(sel)) "Explain selection" else "Explain"
    updateActionButton(session, "explain_code", label = label)
  })

  # --- Help mode -------------------------------------------------------------
  help_mode <- reactiveVal(FALSE)

  # Remove buttons only appear when there is something to remove (and in help
  # mode, so that they can still be explained).
  observe({
    show_remove <- length(selected_files()) > 0 || length(selected_objects()) > 0 ||
                   isTRUE(help_mode())
    shinyjs::toggle("context_links", condition = show_remove)
  })

  observeEvent(input$help_toggle, {
    new_state <- !help_mode()
    help_mode(new_state)
    if (new_state) {
      shinyjs::addClass(selector = "body", class = "help-mode")
    } else {
      shinyjs::removeClass(selector = "body", class = "help-mode")
    }
  })

  observeEvent(input$usage_help, {
    limit  <- cost_limit_val()
    spend  <- current_spend()
    expiry <- final_expiry_val()
    period <- reset_period_val()
    reset_label <- tryCatch({
      rt <- next_reset_time(isolate(usage_log_rv()), period)
      if (grepl("^rolling", period)) "on a rolling window basis"
      else paste0("on ", format(rt, "%d %B %Y"))
    }, error = function(e) "")
    spend_line <- if (!is.null(limit)) {
      pct <- round(min(spend / limit, 1) * 100)
      pct_rem <- 100 - pct
      paste0("You have used ", pct, "% of your allowance ($",
             sprintf("%.3f", spend), " of $", sprintf("%.2f", limit), "). ",
             pct_rem, "% remains.")
    } else {
      paste0("Your session spend so far is $", sprintf("%.3f", spend), ".")
    }
    reset_line <- if (nzchar(reset_label) && !is.null(limit))
      paste0("Your allowance resets ", reset_label, ".")
    else NULL
    expiry_line <- if (!is.null(expiry))
      paste0("Your Classmate key expires on ",
             format(as.Date(expiry), "%d %B %Y"), ".")
    else NULL
    showModal(modalDialog(
      title = "Your usage",
      tags$p("This bar shows how much of your Classmate allowance you have used. Your instructor sets a spending limit to manage costs; the bar turns from green to orange to red as you approach it."),
      tags$p(spend_line),
      if (!is.null(reset_line)) tags$p(reset_line),
      if (!is.null(expiry_line)) tags$p(expiry_line),
      tags$p("If you reach your limit, buttons will be disabled until the allowance resets or your instructor provides a new key (load it under Preferences)."),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })

  observeEvent(input$about_classmate, {
    showModal(modalDialog(
      title = "About Classmate",
      tags$p("Classmate is an AI-powered learning assistant that works alongside RStudio (or R) and opens in your web browser. It acts as an interface between R and Claude — Anthropic’s AI — so instead of switching between RStudio and a separate chat tool, you can ask questions, request code, and get explanations all in one place, with direct access to what’s currently in your R script and workspace."),
      tags$p("When you ask Classmate to write or modify code, it runs the code and displays the output. If the code fails, Classmate can diagnose the error and fix it. If it succeeds, you can ask for an explanation of what the code does and why, pitched at whatever level suits you. Classmate also keeps track of what has changed between versions, so you can see exactly what was added or altered and why."),
      tags$p("Classmate is designed to support your learning, not replace it — to encourage you to engage with the answers, not just the output."),
      tags$br(),
      tags$p("To find out more, please press any of the buttons shaded black."),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
  })

  help_text_for <- function(btn_id, label = "") {
    for (pat in names(HELP_ID_PATTERNS))
      if (grepl(pat, btn_id)) return(HELP_ID_PATTERNS[[pat]])
    if (startsWith(btn_id, "tab__")) {
      tab_val <- sub("^tab__", "", btn_id)
      return(HELP_TAB_TEXTS[[tab_val]] %||% paste0("The ", tab_val, " tab."))
    }
    HELP_TEXTS[[btn_id]] %||% paste0(
      if (nzchar(label)) paste0(label, " — ") else "",
      "no detailed help is available for this button yet."
    )
  }

  observeEvent(input$help_button_clicked, {
    req(help_mode())
    btn_id <- input$help_button_clicked$id
    req(nzchar(btn_id %||% ""))
    showModal(modalDialog(
      title     = "Help",
      tags$p(help_text_for(btn_id, trimws(input$help_button_clicked$label %||% "")),
             style = "line-height: 1.6;"),
      footer    = modalButton("OK"),
      easyClose = TRUE
    ))
  })

  output$prompt_history_ui <- renderUI({
    history <- prompt_history()
    if (length(history) == 0)
      return(tags$p(em("No prompts yet."), style = "color: #999; margin: 0; font-size: 0.9em;"))
    kind_of <- function(p) {
      r <- find_interaction_by_prompt(p)
      if (is.null(r)) "" else if (identical(r$kind, "answer")) "Answer"
      else if (isTRUE(r$export)) "Export" else "Code"
    }
    digest_id <- function(p) match(p, history)
    tagList(lapply(history, function(p) {
      # Escape single quotes in the prompt text so it survives the onclick string.
      p_escaped <- gsub("'", "\\\\'", p, fixed = TRUE)
      div(class = "cm-prompt-row", `data-p` = p,
        style = "display: flex; align-items: flex-start; border-bottom: 1px solid #eee; padding: 2px 0;",
        tags$p(p, style = paste0(
          "margin: 0; flex: 1;",
          "font-size: 0.85em; color: #444;",
          "white-space: pre-wrap; word-break: break-word;"
        )),
        if (nzchar(kind_of(p)))
          tags$span(style = "flex-shrink: 0; margin: 2px 6px 0 6px; font-size: 0.72em; color: #888;",
                    kind_of(p)),
        tags$button("Recall", id = paste0("recall_prompt_", digest_id(p)),
          style = paste0(
            "flex-shrink: 0; margin-left: 6px; padding: 1px 6px;",
            "font-size: 0.75em; background: white;",
            "border: 1px solid #bbb; border-radius: 3px;",
            "cursor: pointer; color: #555;"
          ),
          onclick = paste0(
            "Shiny.setInputValue('copy_past_prompt', '", p_escaped, "',",
            "{priority: 'event'});"
          )
        )
      )
    }))
  })

  output$past_code_ui <- renderUI({
    full_history <- run_history()
    # Keep original indices so button IDs match the observer's lookup
    keep_idx <- which(vapply(full_history, function(e) identical(e$source, "ask_code"), logical(1)))
    unconf   <- unconf_code()
    if (length(keep_idx) == 0 && length(unconf) == 0)
      return(tags$p(em("No code blocks yet."), style = "color: #999; margin: 4px 0; font-size: 0.9em;"))

    make_label <- function(entry) {
      # Prefer AI-generated description, then prompt, then first line of code
      lbl <- if (nzchar(trimws(entry$description %||% ""))) {
        trimws(entry$description)
      } else if (nzchar(trimws(entry$prompt %||% ""))) {
        trimws(entry$prompt)
      } else {
        lines <- grep("^\\s*(#|$)", strsplit(entry$code, "\n")[[1]],
                      invert = TRUE, value = TRUE)
        if (length(lines) > 0) trimws(lines[[1]]) else "Code block"
      }
      if (nchar(lbl) > 60) paste0(substr(lbl, 1, 57), "...") else lbl
    }
    ts_of <- function(e) if (is.null(e$ts)) 0 else as.numeric(e$ts)

    # Confirmed (run successfully) and unconfirmed entries together, newest first
    rows <- c(lapply(keep_idx, function(i) list(kind = "confirmed", i = i, e = full_history[[i]], pos = i)),
              lapply(unconf, function(e) list(kind = "unconfirmed", e = e, pos = 0L)))
    ord  <- order(-vapply(rows, function(r) ts_of(r$e), numeric(1)), -vapply(rows, function(r) r$pos, numeric(1)))
    btn_style <- paste0(
      "flex-shrink: 0; margin-left: 6px; padding: 1px 6px;",
      "font-size: 0.75em; background: white;",
      "border: 1px solid #bbb; border-radius: 3px;",
      "cursor: pointer; color: #555;")
    items <- lapply(rows[ord], function(r) {
      conf <- identical(r$kind, "confirmed")
      div(style = "display: flex; align-items: flex-start; border-bottom: 1px solid #eee; padding: 2px 0;",
        tags$p(make_label(r$e), style = paste0(
          "margin: 0; flex: 1;",
          "font-size: 0.85em; color: #444;",
          "white-space: pre-wrap; word-break: break-word;")),
        tags$span(style = "flex-shrink: 0; margin: 2px 6px 0 6px; font-size: 0.72em; color: #888;",
                  if (conf) "(confirmed)" else "(unconfirmed)"),
        if (conf)
          tags$button("Recall", id = paste0("repeat_code_", r$i), style = btn_style,
                      onclick = paste0("Shiny.setInputValue('repeat_code_", r$i,
                                       "', Math.random(), {priority: 'event'});"))
        else
          tags$button("Recall", id = paste0("recall_unconf_", r$e$uid), style = btn_style,
                      onclick = paste0("Shiny.setInputValue('recall_unconf_code', ", as.integer(r$e$uid),
                                       ", {priority: 'event'});"))
      )
    })
    tagList(items)
  })

  # Recall an unconfirmed entry: its code, and the changes from the previous version if it had them,
  # but no R output (it has not been run)
  observeEvent(input$recall_unconf_code, {
    e <- Filter(function(x) identical(as.integer(x$uid), as.integer(input$recall_unconf_code)), unconf_code())
    req(length(e) == 1)
    e <- e[[1]]
    capture_active_code()
    rec <- NULL
    if (!is.null(e$seq)) for (r in interactions()) if (identical(r$seq, e$seq)) { rec <- r; break }
    if (!is.null(rec)) {
      recall_interaction(rec, code_override = e$code, prefer = "code")
    } else {
      set_ask_mode("code")
      hide_changes_tab()
      updateAceEditor(session, "code_editor", value = e$code)
      last_run_code(NULL)
      last_prompt_for_code(e$prompt %||% "")      # what this code is, if it is later run and confirmed
      last_code_description(if (identical(e$source, "script")) "" else (e$description %||% ""))
      loaded_script_name(if (identical(e$source, "script")) e$description else NULL)
      goto_tab("Code")
    }
    unconf_code(lapply(unconf_code(), function(x) { if (identical(x$uid, e$uid)) x$ts <- Sys.time(); x }))
  })

  # Past code "Recall": reload that code block; if it is the code that produced a captured
  # run, bring back its R output, plots and warnings too (not its prompt: use Past prompts for that). Observers are registered
  # once per button (button i = i-th entry of the current run history).
  repeat_obs <- new.env()
  observe({
    for (i in seq_along(run_history())) {
      key <- as.character(i)
      if (!is.null(repeat_obs[[key]])) next
      repeat_obs[[key]] <- TRUE
      local({
        ii <- i
        observeEvent(input[[paste0("repeat_code_", ii)]], {
          h <- run_history()
          if (ii > length(h)) return()
          entry <- h[[ii]]
          h[[ii]]$ts <- Sys.time()      # brings it to the top of the list
          run_history(h)
          capture_active_code()
          rec <- NULL
          for (r in interactions())
            if (identical(r$kind, "code") && !is.null(r$last_run_code) &&
                identical(trimws(r$last_run_code), trimws(entry$code))) { rec <- r; break }
          if (!is.null(rec)) {
            recall_interaction(rec, code_override = entry$code, prefer = "code")   # the prompt box is left alone: Past prompts recalls prompts
          } else {
            updateAceEditor(session, "code_editor", value = entry$code)
            last_run_code(NULL)
            last_prompt_for_code(entry$prompt %||% "")
            last_code_description(entry$description %||% "")
            goto_tab("Code")
          }
        }, ignoreInit = TRUE)
      })
    }
  })

  # Past prompt repeat: move to top (no duplicate)
  observeEvent(input$copy_past_prompt, {
    p   <- input$copy_past_prompt
    ph  <- prompt_history()
    idx <- match(p, ph)
    if (!is.na(idx)) ph <- c(p, ph[-idx]) else ph <- c(p, ph)
    prompt_history(ph)
    rec <- find_interaction_by_prompt(p)       # bring back everything the prompt produced
    if (!is.null(rec)) recall_interaction(rec)
    # An Export entry has no prompt of its own, so the prompt box is left blank
    updateTextAreaInput(session, "prompt", value = if (isTRUE(rec$export)) "" else p)
    updateTabsetPanel(session, "prompt_tabs", selected = "Your prompt")
  })

  # --- Past Conversations ----------------------------------------------------
  output$past_conversations_ui <- renderUI({
    convs <- saved_conversations()
    if (length(convs) == 0)
      return(tags$p(em("No past conversations yet."),
                    style = "color: #999; margin: 4px 0; font-size: 0.9em;"))
    items <- lapply(seq_along(convs), function(i) {
      conv   <- convs[[i]]
      btn_id <- paste0("recall_conv_", i)
      div(style = "display: flex; align-items: flex-start; border-bottom: 1px solid #eee; padding: 4px 0;",
        tags$p(conv$summary,
          style = "margin: 0; flex: 1; font-size: 0.85em; color: #444; white-space: pre-wrap; word-break: break-word;"),
        actionButton(btn_id, "Recall",
          style = paste0(
            "flex-shrink: 0; margin-left: 6px; padding: 1px 6px;",
            "font-size: 0.75em; background: white;",
            "border: 1px solid #bbb; border-radius: 3px;",
            "cursor: pointer; color: #555; height: auto;"
          )
        )
      )
    })
    tagList(items)
  })

  recall_conv_index <- reactiveVal(NULL)

  observe({
    convs <- saved_conversations()
    lapply(seq_along(convs), function(i) {
      btn_id <- paste0("recall_conv_", i)
      observeEvent(input[[btn_id]], {
        recall_conv_index(i)
        showModal(modalDialog(
          title = "Recall past conversation?",
          tags$p("Your current conversation will be saved to Past conversations, then the selected conversation will be restored."),
          tags$p("The Code editor, Past code, and Past prompts will be replaced with those from the recalled conversation."),
          footer = tagList(
            actionButton("confirm_recall_conv", "Yes, recall this conversation", class = "btn-primary"),
            modalButton("Cancel")
          )
        ))
      }, ignoreInit = TRUE)
    })
  })

  observeEvent(input$confirm_recall_conv, {
    removeModal()
    idx <- recall_conv_index()
    if (is.null(idx)) return()
    convs <- saved_conversations()
    if (idx > length(convs)) return()
    # If the current conversation is blank, recalling gives back one slot
    current_is_blank <- length(prompt_history()) == 0 && length(run_history()) == 0
    # Save current conversation before recalling (no-op if blank)
    save_current_conversation_if_nonempty()
    # Restore recalled conversation
    recalled <- convs[[idx]]
    # Remove from list (it's now the active conversation; will be re-saved on next New Conversation)
    saved_conversations(convs[-idx])
    # Reset Claude conversation history
    conversation_history(list())
    # Restore prompts, code history, and file/object context
    prompt_history(recalled$prompts)
    run_history(recalled$codes)
    unconf_code(recalled$unconf %||% list())
    selected_files(  recalled$files   %||% character(0))
    selected_objects(recalled$objects %||% character(0))
    # Restore log entries so the notebook continues from where this conversation left off
    all_log_entries(recalled$log_entries %||% list())
    # Clear transient state
    hide_changes_tab()
    pending_log_entries(list())
    last_extracted_code("")
    last_prompt_for_code("")
    last_code_description("")
    loaded_script_name(NULL)
    last_run_failed(FALSE)
    last_run_code(NULL)
    code_before_fix(NULL)
    console_output_rv(character(0)); warnings_rv(character(0)); plot_files_rv(character(0))
    # Load most recent code into editor
    codes <- recalled$codes
    ask_code_entries <- Filter(function(e) identical(e$source, "ask_code"), codes)
    if (length(ask_code_entries) > 0)
      updateAceEditor(session, "code_editor", value = ask_code_entries[[length(ask_code_entries)]]$code)
    else
      updateAceEditor(session, "code_editor", value = "")
    output$run_status <- renderUI(NULL)
    answer_latest(recalled$answer_latest)
    answer_hist(recalled$answer_hist %||% list())
    interactions(recalled$interactions %||% list())
    active_code_id(NULL)
    placeholder_map(character(0))
    goto_tab("Code")
    if (length(recalled$codes) == 0 && !is.null(recalled$answer_latest)) set_ask_mode("answer")
    if (current_is_blank) conv_count_rv(max(conv_count_rv() - 1L, 1L))
  })

  # --- File browser and workspace object picker -----------------------------
  volumes <- build_volumes()
  dialog_roots <- function() file_dialog_roots(volumes)

  shinyFileChoose(input, "file_select", roots = dialog_roots, session = session,
                   defaultRoot = "Working directory", defaultPath = "")

  observeEvent(input$file_select, {
    file_info <- parseFilePaths(dialog_roots(), input$file_select)
    if (nrow(file_info) > 0)
      selected_files(union(selected_files(), file_info$datapath))
  })

  shinyFileChoose(input, "load_workspace", roots = dialog_roots, session = session,
                   defaultRoot = "Working directory", defaultPath = "",
                   filetypes = c("RData", "rda", "rds"))

  observeEvent(input$load_workspace, {
    file_info <- parseFilePaths(dialog_roots(), input$load_workspace)
    if (nrow(file_info) > 0) {
      path <- file_info$datapath[[1]]
      tryCatch(
        load(path, envir = .GlobalEnv),
        error = function(e)
          showNotification(paste("Could not load workspace:", conditionMessage(e)),
                           type = "error", duration = 8)
      )
    }
  })

  shinyFileChoose(input, "load_script", roots = dialog_roots, session = session,
                   defaultRoot = "Working directory", defaultPath = "",
                   filetypes = c("R", "r"))

  observeEvent(input$load_script, {
    file_info <- parseFilePaths(dialog_roots(), input$load_script)
    if (nrow(file_info) == 0) return()
    path <- file_info$datapath[[1]]
    fname <- basename(path)
    ext   <- tools::file_ext(path)
    if (!tolower(ext) %in% c("r")) {
      showNotification(
        paste0('"', fname, '" does not appear to be an R script (.R file). Please select an .R file.'),
        type = "error", duration = 8
      )
      return()
    }
    script_text <- tryCatch(
      paste(readLines(path, warn = FALSE), collapse = "\n"),
      error = function(e) {
        showNotification(paste("Could not read file:", conditionMessage(e)),
                         type = "error", duration = 8)
        NULL
      }
    )
    if (is.null(script_text)) return()
    max_l <- isolate(prefs$max_lines)
    if (code_too_long(script_text, max_l)) {
      n_code_lines <- count_code_lines(script_text)
      showModal(modalDialog(
        title = "Script too long to load",
        tags$p(
          tags$strong(fname), " contains ", n_code_lines, " lines of code. ",
          "Classmate is designed to work with short, focused code chunks — ideally ",
          "no more than ", max_l, " lines at a time. Extended scripts are better ",
          "broken into smaller steps, each prompted and run in turn."
        ),
        tags$p("The script has not been loaded. Please select a shorter excerpt ",
               "or paste just the section you want to work with."),
        footer    = modalButton("OK"),
        easyClose = FALSE
      ))
      return()
    }
    loaded_script_name(fname)
    last_prompt_for_code("")    # a loaded script has no prompt behind it
    last_code_description("")   # and is described by its file name (loaded_script_name)
    unconf_add(script_text, "", fname, "script")
    updateAceEditor(session, "code_editor", value = script_text)
    goto_tab("Code")
  })

  # --- Load Package Data -------------------------------------------------------
  # Lists the datasets of every installed package (read from each package's own data index, which
  # needs no data to be loaded), in two stages: (1) tick datasets, in one or several packages, and Load
  # them into the workspace; (2) choose, by type, which of the loaded objects to add to the context.
  # The list is built when the dialog opens (about 0.1-0.3 s for a few hundred packages) and cached
  # until the library folders change, so packages installed, updated or removed while Classmate was
  # paused, or from the Quick Console, are always picked up.
  pkgdata_cache <- new.env()
  PKG_SKIP_DIRS <- c("R", "help", "html", "Meta", "libs", "doc", "demo", "tests", "www", "data")
  scan_pkg_data <- function() {
    libs <- .libPaths()
    sig  <- paste(libs, as.numeric(file.mtime(libs)), collapse = "|")
    if (identical(pkgdata_cache$sig, sig) && !is.null(pkgdata_cache$index)) return(pkgdata_cache$index)
    rows <- list(); seen <- character(0)
    for (lib in libs) for (pk in list.files(lib)) {
      if (pk %in% seen) next
      pdir <- file.path(lib, pk)
      if (!file.exists(file.path(pdir, "DESCRIPTION"))) next
      seen <- c(seen, pk)
      # datasets (from the package's data index)
      f <- file.path(pdir, "Meta", "data.rds")
      d <- if (file.exists(f)) tryCatch(readRDS(f), error = function(e) NULL) else NULL
      if (!is.null(d) && NROW(d) > 0) {
        item <- as.character(d[, 1])
        rows[[length(rows) + 1L]] <- data.frame(
          package = pk, item = item,
          loadname = ifelse(grepl("^.* \\(.*\\)$", item), sub("^.* \\((.*)\\)$", "\\1", item), item),
          title = as.character(d[, 2]), kind = "data", path = NA_character_,
          objname = sub(" \\(.*$", "", item), stringsAsFactors = FALSE)
      }
      # spatial files the package ships (shapefiles, GeoPackages, GeoJSON), which are read with sf
      subs <- setdiff(list.dirs(pdir, recursive = FALSE, full.names = FALSE), PKG_SKIP_DIRS)
      fl <- unlist(lapply(subs, function(sd)
        file.path(sd, list.files(file.path(pdir, sd), pattern = "\\.(shp|gpkg|geojson)$",
                                 recursive = TRUE, ignore.case = TRUE))))
      if (length(fl) > 0) {
        nm  <- make.names(tools::file_path_sans_ext(basename(fl)))
        dup <- duplicated(nm) | duplicated(nm, fromLast = TRUE)
        nm[dup] <- paste0(nm[dup], "_", tolower(tools::file_ext(fl[dup])))
        rows[[length(rows) + 1L]] <- data.frame(
          package = pk, item = basename(fl), loadname = NA_character_,
          title = paste0("spatial file: ", fl), kind = "file", path = file.path(pdir, fl),
          objname = nm, stringsAsFactors = FALSE)
      }
    }
    idx <- if (length(rows)) do.call(rbind, rows)
           else data.frame(package = character(0), item = character(0), loadname = character(0),
                           title = character(0), kind = character(0), path = character(0),
                           objname = character(0), stringsAsFactors = FALSE)
    idx$key <- ifelse(idx$kind == "file", paste0(idx$package, "::file:", idx$item, "#", ave(seq_along(idx$item), idx$package, idx$item, FUN = seq_along)),
                      paste0(idx$package, "::", idx$loadname))
    pkgdata_cache$sig <- sig; pkgdata_cache$index <- idx
    idx
  }

  # The datasets of a chosen package are all loaded into a scratch store (not the workspace) so that their
  # classes and sizes can be shown; only the ones ticked are copied into the workspace and added to the
  # context when Add is pressed. Packages with a very large data folder are not pre-loaded: their datasets
  # are listed by name and loaded only if ticked.
  PKGDATA_BIG_MB <- 30
  pkgdata_store <- new.env()          # package -> data frame (object, class, dims, key) with the values attached
  pkgdata_sel   <- reactiveVal(character(0))   # keys "package::object" ticked so far
  pkgdata_plan  <- reactiveVal(NULL)           # chosen objects waiting for the replace check

  pkg_data_mb <- function(pkg) {
    d <- system.file("data", package = pkg)
    if (!nzchar(d)) return(0)
    sum(file.size(list.files(d, full.names = TRUE, recursive = TRUE)), na.rm = TRUE) / 1e6
  }
  # Load a whole package's datasets into the scratch store (once); returns its info data frame
  pkgdata_load_pkg <- function(pkg) {
    if (!is.null(pkgdata_store[[pkg]])) return(pkgdata_store[[pkg]])
    idx <- scan_pkg_data(); idx <- idx[idx$package == pkg, ]
    ds  <- idx[idx$kind == "data", ]; fl <- idx[idx$kind == "file", ]
    # rows for the spatial files (read with sf only when added)
    file_rows <- if (nrow(fl) > 0) data.frame(
      object = fl$objname, class = "spatial file", dims = "", title = sub("^spatial file: ", "", fl$title),
      loadname = NA_character_, key = fl$key, kind = "file", path = fl$path, stringsAsFactors = FALSE) else NULL
    empty <- data.frame(object = character(0), class = character(0), dims = character(0), title = character(0),
                        loadname = character(0), key = character(0), kind = character(0), path = character(0),
                        stringsAsFactors = FALSE)
    big <- pkg_data_mb(pkg) > PKGDATA_BIG_MB
    vals <- list(); rows <- list()
    if (big && nrow(ds) > 0) {
      rows[[1L]] <- data.frame(object = ds$objname, class = NA_character_, dims = "", title = ds$title,
                               loadname = ds$loadname, key = paste0(pkg, "::", ds$objname), kind = "data",
                               path = NA_character_, stringsAsFactors = FALSE)
    } else for (ln in unique(ds$loadname)) {
      e <- new.env()
      ok <- tryCatch({ suppressWarnings(utils::data(list = ln, package = pkg, envir = e)); TRUE },
                     error = function(err) FALSE)
      if (!ok) next
      for (nm in ls(e)) {
        o <- get(nm, envir = e)
        vals[[nm]] <- o
        ttl <- ds$title[match(nm, ds$objname)]
        rows[[length(rows) + 1L]] <- data.frame(
          object = nm, class = class(o)[1],
          dims = if (!is.null(dim(o)) && length(dim(o)) == 2) paste0(nrow(o), " x ", ncol(o)) else paste0("length ", length(o)),
          title = if (length(ttl) && !is.na(ttl)) ttl else "", loadname = ln, key = paste0(pkg, "::", nm),
          kind = "data", path = NA_character_, stringsAsFactors = FALSE)
      }
    }
    info <- do.call(rbind, c(list(empty), rows, list(file_rows)))
    attr(info, "values") <- vals; attr(info, "big") <- big
    assign(pkg, info, envir = pkgdata_store)
    info
  }
  pkgdata_clear_store <- function() { rm(list = ls(pkgdata_store, all.names = TRUE), envir = pkgdata_store) }

  pkgdata_last <- reactiveVal("")      # the package last chosen, to restore the dialog after a replace check
  observeEvent(input$pkgdata_pkg, pkgdata_last(input$pkgdata_pkg %||% ""))
  show_pkgdata_modal <- function() {
    idx <- scan_pkg_data()
    if (nrow(idx) == 0) {
      showNotification("No installed package comes with datasets.", type = "warning", duration = 5)
      return(invisible(NULL))
    }
    pkgs <- sort(unique(idx$package))
    counts <- vapply(pkgs, function(p) sum(idx$package == p), integer(1))
    showModal(modalDialog(
      title = "Load package data",
      tags$p("Pick a package to see its datasets and any spatial files (shapefiles, GeoPackages, GeoJSON) it comes",
             "with, tick the ones you want (in one or several packages) and press Add: they are put in your R",
             "workspace and added to the context for your prompt."),
      selectInput("pkgdata_pkg", "Package:",
                  choices = c("Choose a package..." = "", setNames(pkgs, paste0(pkgs, " (", counts, ")"))),
                  selected = isolate(pkgdata_last()), width = "100%"),
      textInput("pkgdata_pkgfilter", NULL, placeholder = "Filter the package list (e.g. sp)", width = "100%"),
      uiOutput("pkgdata_filters"),
      uiOutput("pkgdata_items"),
      uiOutput("pkgdata_ticked"),
      footer = tagList(
        uiOutput("pkgdata_add_btn", inline = TRUE),
        actionButton("pkgdata_cancel", "Close")
      ),
      easyClose = FALSE
    ))
  }
  # The text box under Package narrows the packages offered in the drop-down
  observeEvent(input$pkgdata_pkgfilter, ignoreInit = TRUE, {
    idx  <- scan_pkg_data(); pkgs <- sort(unique(idx$package))
    q    <- trimws(input$pkgdata_pkgfilter %||% "")
    if (nzchar(q)) pkgs <- pkgs[grepl(tolower(q), tolower(pkgs), fixed = TRUE)]
    counts <- vapply(pkgs, function(p) sum(idx$package == p), integer(1))
    cur <- isolate(input$pkgdata_pkg %||% "")
    updateSelectInput(session, "pkgdata_pkg",
      choices  = c(if (length(pkgs) == 0) c("No package matches" = "") else c("Choose a package..." = ""),
                   if (length(pkgs) > 0) setNames(pkgs, paste0(pkgs, " (", counts, ")"))),
      selected = if (cur %in% pkgs) cur else "")
  })
  observeEvent(input$load_package_data, {
    pkgdata_sel(character(0)); pkgdata_clear_store(); pkgdata_last("")
    show_pkgdata_modal()
  })
  observeEvent(input$pkgdata_cancel, { pkgdata_clear_store(); pkgdata_sel(character(0)); pkgdata_last(""); removeModal() })

  # Info for the chosen package (loading it on first use)
  pkgdata_info <- reactive({
    p <- input$pkgdata_pkg %||% ""
    if (!nzchar(p)) return(NULL)
    withProgress(message = paste0("Reading the ", p, " datasets..."), value = 0.5, pkgdata_load_pkg(p))
  })
  output$pkgdata_filters <- renderUI({
    info <- pkgdata_info(); req(info)
    big <- isTRUE(attr(info, "big"))
    fluidRow(
      column(if (big) 12 else 6, textInput("pkgdata_filter", NULL, placeholder = "Filter by Data name (optional)", width = "100%")),
      if (!big) column(6, selectInput("pkgdata_type", NULL, c("All types" = "", sort(unique(info$class))), width = "100%")))
  })
  # What is shown: the package's datasets, filtered by name and type
  pkgdata_shown <- reactive({
    info <- pkgdata_info(); req(info)
    q <- trimws(input$pkgdata_filter %||% ""); t <- input$pkgdata_type %||% ""
    if (nzchar(q)) info <- info[grepl(tolower(q), tolower(paste(info$object, info$title)), fixed = TRUE), ]
    if (nzchar(t) && !isTRUE(attr(info, "big"))) info <- info[info$class == t, ]
    info
  })
  output$pkgdata_items <- renderUI({
    d <- pkgdata_shown()
    tagList(
      div(style = "margin: 2px 0 6px 0; font-size: 0.85em;",
          actionLink("pkgdata_tick_all", "Tick all shown"), " · ", actionLink("pkgdata_untick_all", "Untick all shown")),
      div(style = "max-height: 250px; overflow-y: auto; border: 1px solid #e3e3e3; border-radius: 4px; padding: 6px 10px;",
        if (nrow(d) == 0) em("No datasets match.") else
          checkboxGroupInput("pkgdata_choice", NULL,
            choiceNames  = lapply(seq_len(nrow(d)), function(i)
              tagList(tags$strong(d$object[i]),
                      tags$span(style = "color: #666;",
                                if (!is.na(d$class[i])) paste0(" — ", d$class[i], if (nzchar(d$dims[i])) paste0(", ", d$dims[i])),
                                if (nzchar(d$title[i] %||% ""))
                                  if (identical(d$class[i], "spatial file")) paste0(" (", d$title[i], ")")
                                  else paste0(if (!is.na(d$class[i])) "; " else " — ", d$title[i])))),
            choiceValues = d$key,
            selected     = intersect(d$key, isolate(pkgdata_sel())))))
  })
  observeEvent(input$pkgdata_tick_all,   updateCheckboxGroupInput(session, "pkgdata_choice", selected = pkgdata_shown()$key))
  observeEvent(input$pkgdata_untick_all, updateCheckboxGroupInput(session, "pkgdata_choice", selected = character(0)))
  # Keep the ticks from every package: only the keys currently shown are replaced
  observeEvent(input$pkgdata_choice, ignoreNULL = FALSE, {
    shown <- tryCatch(isolate(pkgdata_shown())$key, error = function(e) character(0))
    pkgdata_sel(union(setdiff(pkgdata_sel(), shown), input$pkgdata_choice %||% character(0)))
  })
  output$pkgdata_ticked <- renderUI({
    k <- pkgdata_sel()
    tags$p(style = "margin-top: 8px; font-size: 0.9em;",
           if (length(k) == 0) em("Nothing ticked yet.") else
             tagList(tags$strong(paste0(length(k), " ticked: ")),
                     paste(sub("#[0-9]+$", "", sub("::", " :: ", sub("::file:", "::", k, fixed = TRUE), fixed = TRUE)), collapse = ", ")))
  })
  output$pkgdata_add_btn <- renderUI({
    n <- length(pkgdata_sel())
    lab <- if (n == 0) "Add" else paste0("Add (", n, ")")
    if (n == 0) disabled(actionButton("pkgdata_add", lab, class = "btn-primary"))
    else actionButton("pkgdata_add", lab, class = "btn-primary")
  })

  # Add: copy the ticked datasets into the workspace (replace check first) and add them to the context
  observeEvent(input$pkgdata_add, {
    keys <- pkgdata_sel(); req(length(keys) > 0)
    plan <- list(); failed <- character(0); used <- character(0)
    for (k in keys) {
      pk   <- sub("::.*$", "", k)
      info <- pkgdata_load_pkg(pk)
      row  <- info[match(k, info$key), ]
      nm   <- row$object
      if (identical(row$kind, "file")) {                  # a spatial file: read it with sf
        val <- tryCatch({
          if (!requireNamespace("sf", quietly = TRUE)) stop("sf is not installed")
          suppressWarnings(sf::st_read(row$path, quiet = TRUE))
        }, error = function(err) NULL)
        if (is.null(val)) { failed <- c(failed, k); next }
      } else {
        vals <- attr(info, "values")
        if (is.null(vals) || is.null(vals[[nm]])) {        # big package: load the dataset now
          e <- new.env()
          ok <- tryCatch({ suppressWarnings(utils::data(list = row$loadname, package = pk, envir = e)); TRUE }, error = function(err) FALSE)
          if (!ok || !exists(nm, envir = e, inherits = FALSE)) { failed <- c(failed, k); next }
          val <- get(nm, envir = e)
        } else val <- vals[[nm]]
      }
      final <- if (nm %in% used) make.names(paste0(nm, "_", pk)) else nm
      used  <- c(used, final)
      plan[[length(plan) + 1L]] <- list(name = final, package = pk, value = val)
    }
    if (length(failed) > 0)
      showNotification(paste("Could not load:", paste(failed, collapse = ", ")), type = "error", duration = 8)
    if (length(plan) == 0) return(invisible(NULL))
    conflicts <- Filter(function(it) exists(it$name, envir = .GlobalEnv, inherits = FALSE) &&
                          !identical(get(it$name, envir = .GlobalEnv), it$value), plan)
    if (length(conflicts) > 0) {
      pkgdata_plan(plan)
      showModal(modalDialog(
        title = "Replace existing objects?",
        tags$p("Your workspace already has ", tags$strong(paste(vapply(conflicts, `[[`, "", "name"), collapse = ", ")),
               " with different contents. Loading will replace ",
               if (length(conflicts) == 1) "it." else "them."),
        footer = tagList(
          actionButton("pkgdata_replace", "Replace and load", class = "btn-warning"),
          actionButton("pkgdata_replace_cancel", "Cancel")
        ),
        easyClose = FALSE
      ))
      return(invisible(NULL))
    }
    commit_pkg_plan(plan, reopen = FALSE)
  })
  observeEvent(input$pkgdata_replace, commit_pkg_plan(pkgdata_plan(), reopen = TRUE))
  observeEvent(input$pkgdata_replace_cancel, show_pkgdata_modal())
  # Copy the chosen datasets into the workspace and add them to the context. The dialog stays open (the
  # student may want to add more); the ticks are cleared. `reopen` redraws it after a replace check.
  commit_pkg_plan <- function(plan, reopen = FALSE) {
    for (it in plan) assign(it$name, it$value, envir = .GlobalEnv)
    nms <- vapply(plan, `[[`, "", "name")
    selected_objects(union(selected_objects(), nms))
    pkgdata_sel(character(0))
    if (reopen) show_pkgdata_modal()
    else updateCheckboxGroupInput(session, "pkgdata_choice", selected = character(0))
    showNotification(paste0("Added ", paste(nms, collapse = ", "), " to your workspace and the context."),
                     type = "message", duration = 6)
  }


  obj_pick_sel <- reactiveVal(character(0))

  obj_pick_matches <- reactive({
    name_filter <- tolower(trimws(input$obj_filter %||% ""))
    all_objs    <- ls(envir = .GlobalEnv)
    by_name <- if (nzchar(name_filter))
      all_objs[grepl(name_filter, tolower(all_objs), fixed = TRUE)]
    else all_objs
    tf <- filter_objects_by_type(by_name, input$obj_type_filter %||% "")
    list(n_all = length(all_objs), matched = tf$matched, unrecognised = tf$unrecognised)
  })

  output$obj_picker_list <- renderUI({
    m <- obj_pick_matches()
    if (m$n_all == 0) return(tags$p(em("No objects in workspace yet.")))
    if (length(m$matched) == 0) {
      if (length(m$unrecognised) > 0)
        return(tags$p(em(paste0(
          "Classmate does not recognise '", paste(m$unrecognised, collapse = "', '"),
          "' as an object type. Try, for example: sf, raster, data table, mappable, ",
          "model, text, number, function."))))
      return(tags$p(em("No objects match the filters.")))
    }
    cls <- vapply(m$matched, function(nm)
      tryCatch(class(get(nm, envir = .GlobalEnv))[1], error = function(e) ""), character(1))
    div(
      if (length(m$unrecognised) > 0)
        tags$p(style = "color: #b45f06; font-size: 0.85em;",
          paste0("Not recognised as a type: '", paste(m$unrecognised, collapse = "', '"), "'.")),
      div(style = "max-height: 300px; overflow-y: auto;",
        checkboxGroupInput("obj_picker_checked", label = NULL,
          choiceNames = lapply(seq_along(m$matched), function(i)
            tagList(m$matched[i],
                    tags$span(style = "color: #888; font-size: 0.85em;",
                              if (nzchar(cls[i])) paste0("  (", cls[i], ")") else ""))),
          choiceValues = m$matched,
          selected = intersect(isolate(obj_pick_sel()), m$matched))
      )
    )
  })

  # Keep ticks while the filters change: update only the currently visible items
  observeEvent(input$obj_picker_checked, {
    vis <- isolate(obj_pick_matches()$matched)
    obj_pick_sel(union(setdiff(obj_pick_sel(), vis), input$obj_picker_checked %||% character(0)))
  }, ignoreNULL = FALSE)

  observeEvent(input$add_objects, {
    obj_pick_sel(character(0))
    showModal(modalDialog(
      title = "Select workspace objects as context",
      textInput("obj_filter", "Filter by name:", value = "", placeholder = "Type to filter..."),
      textInput("obj_type_filter", "Filter by type:", value = "",
                placeholder = "e.g. sf, data table, mappable, model"),
      tags$p(style = "color: #888; font-size: 0.85em; margin-top: -8px;",
        "Optional. Use R classes (sf, raster, integer, list) or plain words",
        "(mappable, data table, text, number, function). Separate several with commas."),
      uiOutput("obj_picker_list"),
      footer = tagList(
        actionButton("confirm_add_objects", "Add selected", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$confirm_add_objects, {
    to_add <- obj_pick_sel()
    if (!is.null(to_add) && length(to_add) > 0)
      selected_objects(union(selected_objects(), to_add))
    removeModal()
  })

  # The list of files and objects added for context. A box marked with a cross (shown struck through and
  # greyed) means "do not use this": it is left out of what is passed to the prompt, and "Remove selected"
  # removes the crossed ones for good; "Remove all" clears the list.
  ctx_crossed <- reactiveVal(character(0))     # list values ("file::..." / "obj::...") marked with a cross
  # What is passed to the prompt: everything added for context except what is crossed out
  context_files   <- function() { x <- selected_files();   x[!(paste0("file::", x) %in% ctx_crossed())] }
  context_objects <- function() { x <- selected_objects(); x[!(paste0("obj::",  x) %in% ctx_crossed())] }
  context_choices <- function() {
    files <- selected_files();  files <- files[nzchar(files)]
    objs  <- selected_objects(); objs  <- objs[nzchar(objs)]
    ch <- character(0)
    if (length(files) > 0) ch <- c(ch, stats::setNames(paste0("file::", files), basename(files)))
    # Objects are labelled with their class, e.g. "cars [data.frame]"
    obj_label <- function(nm) {
      cl <- tryCatch(class(get(nm, envir = .GlobalEnv, inherits = FALSE))[1], error = function(e) NA_character_)
      paste0(nm, " [", if (is.na(cl)) "not found" else cl, "]")
    }
    if (length(objs)  > 0) ch <- c(ch, stats::setNames(paste0("obj::", objs), vapply(objs, obj_label, "")))
    ch
  }
  output$context_list_ui <- renderUI({
    choices <- context_choices()
    if (length(choices) == 0)
      return(tags$p(em("None added yet.")))
    div(
      id = "context_list_box",
      style = paste0(
        "max-height: 100px; overflow-y: auto;",
        "border: 1px solid #e3e3e3; border-radius: 4px;",
        "padding: 2px 8px; background: #fafafa;"
      ),
      checkboxGroupInput("context_list_checked", label = NULL, choices = choices,
                         selected = intersect(unname(choices), isolate(ctx_crossed())))
    )
  })
  observeEvent(input$context_list_checked, ignoreNULL = FALSE, {
    all_vals <- unname(isolate(context_choices()))
    ctx_crossed(if (length(all_vals) > 0) intersect(all_vals, input$context_list_checked %||% character(0)) else character(0))
  })
  observeEvent(input$remove_context, {
    to_remove <- ctx_crossed()
    if (!is.null(to_remove) && length(to_remove) > 0) {
      file_hits <- to_remove[startsWith(to_remove, "file::")]
      obj_hits  <- to_remove[startsWith(to_remove, "obj::")]
      if (length(file_hits) > 0)
        selected_files(setdiff(selected_files(), sub("^file::", "", file_hits)))
      if (length(obj_hits)  > 0)
        selected_objects(setdiff(selected_objects(), sub("^obj::", "", obj_hits)))
      ctx_crossed(setdiff(ctx_crossed(), to_remove))
    }
  })

  observeEvent(input$remove_all_context, {
    if (length(selected_files()) == 0 && length(selected_objects()) == 0)
      return(invisible(NULL))
    showModal(modalDialog(
      title = "Remove all context?",
      "Are you sure? This will remove all files and objects from the context list.",
      footer = tagList(
        actionButton("confirm_remove_all_context", "Yes, remove all", class = "btn-warning"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$confirm_remove_all_context, {
    selected_files(character(0))
    selected_objects(character(0))
    removeModal()
  })

  # --- Restore paused state (if any) ----------------------------------------
  ps <- if (file.exists(pause_file())) {
    saved <- tryCatch(readRDS(pause_file()), error = function(e) NULL)
    unlink(pause_file())
    saved
  } else NULL

  # --- Answer view state ------------------------------------------------
  # The tabs follow the most recent kind of ask: "code" shows Code / R output /
  # Past code; "answer" shows Answer (and Past answers once there are earlier ones).
  ask_mode      <- reactiveVal(if (!is.null(ps) && identical(ps$ask_mode, "answer")) "answer" else "code")
  answer_latest <- reactiveVal(if (!is.null(ps)) ps$answer_latest else NULL)
  answer_hist   <- reactiveVal(if (!is.null(ps)) (ps$answer_hist %||% list()) else list())
  # Placeholder -> real value, for the whole conversation (kept locally only)
  placeholder_map <- reactiveVal(if (!is.null(ps)) (ps$placeholder_map %||% character(0)) else character(0))
  CODE_SIDE_TABS <- c("Code", "R output", "Past code", "Changes")

  prior_code_tab   <- reactiveVal("Code")     # the code-side tab showing before the Answer view
  prior_answer_tab <- reactiveVal("Answer")   # the answer tab showing before going back to the code view

  refresh_ask_tabs <- function() {
    answer_view <- identical(isolate(ask_mode()), "answer")
    if (isTRUE(isolate(changes_tab_shown())))
      if (answer_view) hideTab("main_tabs", "Changes", session = session)
      else showTab("main_tabs", "Changes", session = session)
    for (t in c("Code", "R output", "Past code"))
      if (answer_view) hideTab("main_tabs", t, session = session)
      else showTab("main_tabs", t, session = session)
    if (answer_view) {
      showTab("main_tabs", "Answer", session = session)
      if (length(isolate(answer_hist())) > 0) showTab("main_tabs", "Past answers", session = session)
      else hideTab("main_tabs", "Past answers", session = session)
    } else {
      hideTab("main_tabs", "Answer", session = session)
      hideTab("main_tabs", "Past answers", session = session)
    }
  }
  set_ask_mode <- function(mode) {
    if (identical(mode, "answer") && !identical(isolate(ask_mode()), "answer")) {
      cur <- isolate(input$main_tabs)
      prior_code_tab(if (isTRUE(cur %in% CODE_SIDE_TABS)) cur else "Code")
    }
    if (identical(mode, "code") && identical(isolate(ask_mode()), "answer")) {
      cur <- isolate(input$main_tabs)
      prior_answer_tab(if (isTRUE(cur %in% c("Answer", "Past answers"))) cur else "Answer")
    }
    ask_mode(mode)
    refresh_ask_tabs()
    if (identical(mode, "answer")) {
      updateTabsetPanel(session, "main_tabs", selected = "Answer")
    } else if (!isTRUE(isolate(input$main_tabs) %in% CODE_SIDE_TABS)) {
      updateTabsetPanel(session, "main_tabs", selected = "Code")
    }
  }
  goto_tab <- function(tab) {
    if (tab %in% CODE_SIDE_TABS && identical(isolate(ask_mode()), "answer")) set_ask_mode("code")
    updateTabsetPanel(session, "main_tabs", selected = tab)
  }
  # --- Interactions: what each past prompt produced, so Recall can bring it all back -----
  # kind "code":   generated code, plus (captured when you move on) the editor text, R output,
  #                plots, warnings, error, diff and the tab you were on.
  # kind "answer": the Answer-tab entry.
  interactions   <- reactiveVal(if (!is.null(ps)) (ps$interactions %||% list()) else list())
  active_code_id <- reactiveVal(if (!is.null(ps)) ps$active_code_id else NULL)
  next_seq <- function()
    max(c(0L, vapply(isolate(interactions()), function(r) as.integer(r$seq %||% 0L), integer(1)))) + 1L
  add_interaction <- function(rec) {
    if (is.null(rec$seq)) rec$seq <- next_seq()
    interactions(utils::head(c(list(rec), isolate(interactions())), 60L))
    invisible(rec$seq)
  }
  capture_active_code <- function() {
    isolate({
      id <- active_code_id()
      if (is.null(id)) return(invisible(NULL))
      recs <- interactions()
      i <- which(vapply(recs, function(r) identical(r$seq, id), logical(1)))
      if (length(i) == 0) return(invisible(NULL))
      r   <- recs[[i[1]]]
      cur <- input$main_tabs
      r$editor_code   <- input$code_editor %||% ""
      # The run state belongs to this interaction only if the last run was of its code (as generated or
      # as now edited); otherwise it is left over from an earlier interaction and is not kept
      lr       <- last_run_code()
      ran_this <- !is.null(lr) && (identical(trimws(lr), trimws(r$editor_code)) ||
                                   identical(trimws(lr), trimws(r$code %||% "")))
      r$last_run_code <- if (ran_this) lr else NULL
      r$console       <- if (ran_this) console_output_rv() else character(0)
      r$warnings      <- if (ran_this) warnings_rv()       else character(0)
      r$plots         <- if (ran_this) plot_files_rv()     else character(0)
      r$failed        <- ran_this && isTRUE(last_run_failed())
      r$error         <- if (ran_this) last_error_msg() else ""
      r$diff          <- diff_rv()
      r$tab <- if (identical(ask_mode(), "answer")) prior_code_tab()
               else if (isTRUE(cur %in% setdiff(CODE_SIDE_TABS, "Past code"))) cur else (r$tab %||% "Code")   # Past code is only a list, not a place to return to
      recs[[i[1]]] <- r
      interactions(recs)
    })
    invisible(NULL)
  }
  find_interaction_by_prompt <- function(p) {
    for (r in isolate(interactions())) if (identical(r$prompt, p)) return(r)
    NULL
  }
  # `prefer` decides which tab a code recall opens: "prompt" (from Past prompts) takes the R output if
  # there is any, else the Changes tab if there are changes, else Code; "code" (from Past code)
  # always opens the Code tab. An answer always opens the Answer tab.
  recall_interaction <- function(rec, code_override = NULL, prefer = c("prompt", "code")) {
    prefer <- match.arg(prefer)
    capture_active_code()
    if (identical(rec$kind, "answer")) {
      ent    <- rec$entry
      hist   <- Filter(function(e) !identical(e$seq, ent$seq), isolate(answer_hist()))
      latest <- isolate(answer_latest())
      if (!is.null(latest) && !identical(latest$seq, ent$seq)) hist <- c(list(latest), hist)
      answer_hist(hist)
      answer_latest(ent)
      set_ask_mode("answer")
      return(invisible(NULL))
    }
    set_ask_mode("code")
    hide_changes_tab()
    updateAceEditor(session, "code_editor", value = code_override %||% rec$editor_code %||% rec$code)
    console_output_rv(rec$console %||% character(0))
    warnings_rv(rec$warnings %||% character(0))
    plots <- rec$plots %||% character(0)
    plot_files_rv(plots[file.exists(plots)])
    last_run_code(rec$last_run_code)
    last_run_failed(isTRUE(rec$failed))
    last_error_msg(rec$error %||% "")
    last_extracted_code(rec$code %||% "")
    last_prompt_for_code(rec$prompt %||% "")
    last_code_description(rec$description %||% "")
    loaded_script_name(NULL)
    code_before_fix(NULL)
    diff_rv(rec$diff)
    if (!is.null(rec$diff)) show_changes_tab()
    has_output <- (length(rec$console %||% character(0)) > 0 && any(nzchar(rec$console))) ||
                  length(isolate(plot_files_rv())) > 0 || length(rec$warnings %||% character(0)) > 0
    tab <- if (identical(prefer, "code")) "Code"
           else if (has_output) "R output"
           else if (!is.null(rec$diff)) "Changes"
           else "Code"
    goto_tab(tab)
    output$run_status <- renderUI(NULL)
    active_code_id(rec$seq)
    invisible(NULL)
  }

  session$onFlushed(function() {
    refresh_ask_tabs()
    if (identical(isolate(ask_mode()), "answer"))
      updateTabsetPanel(session, "main_tabs", selected = "Answer")
  }, once = TRUE)

  # --- Conversation + run-history state --------------------------------------
  conversation_history  <- reactiveVal(if (!is.null(ps)) ps$conversation_history  else list())
  saved_conversations   <- reactiveVal(if (!is.null(ps)) ps$saved_conversations   else list())
  last_extracted_code   <- reactiveVal(if (!is.null(ps)) ps$last_extracted_code   else "")
  last_prompt_for_code <- reactiveVal(if (!is.null(ps)) ps$last_prompt_for_code else "")
  last_code_description <- reactiveVal("")
  loaded_script_name    <- reactiveVal(NULL)
  run_history          <- reactiveVal(if (!is.null(ps)) ps$run_history          else list())
  # Code that has been generated or loaded but not (yet) run successfully, newest first; each entry:
  # uid, code, prompt, description, source ("ask" / "script"), seq (its interaction, if any), ts.
  # It is listed in Past code as "(unconfirmed)" and replaced by a confirmed entry once run.
  unconf_code          <- reactiveVal(if (!is.null(ps)) (ps$unconf_code %||% list()) else list())
  # Unique ids for the entries (next_seq() only counts interactions, so it cannot be used for this)
  unconf_uid_n <- max(c(0L, vapply(isolate(unconf_code()), function(e) as.integer(e$uid %||% 0L), integer(1))))
  unconf_next_uid <- function() { unconf_uid_n <<- unconf_uid_n + 1L; unconf_uid_n }
  unconf_add <- function(code, prompt = "", description = "", source = "ask", seq = NULL) {
    if (!nzchar(trimws(code %||% ""))) return(invisible(NULL))
    keep <- Filter(function(e) !identical(trimws(e$code), trimws(code)), isolate(unconf_code()))
    unconf_code(c(list(list(uid = unconf_next_uid(), code = code, prompt = prompt %||% "",
                            description = description %||% "", source = source, seq = seq,
                            ts = Sys.time())), keep))
    invisible(NULL)
  }
  unconf_remove_code <- function(code)
    unconf_code(Filter(function(e) !identical(trimws(e$code), trimws(code)), isolate(unconf_code())))
  pending_log_entries  <- reactiveVal(if (!is.null(ps)) ps$pending_log_entries  else list())
  all_log_entries      <- reactiveVal(if (!is.null(ps)) ps$all_log_entries      else list())
  last_logged_code     <- reactiveVal(if (!is.null(ps)) ps$last_logged_code     else "")
  prompt_history       <- reactiveVal(if (!is.null(ps)) ps$prompt_history       else character(0))
  conv_count_rv        <- reactiveVal(if (!is.null(ps)) ps$conv_count_rv        else 1L)

  # log_path is generated fresh each time Save Code Log is pressed (date-time stamped)
  make_log_path <- function()
    file.path(project_root(),
      paste0("classmate_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rmd"))
  log_path <- reactiveVal(file.path(project_root(), "classmate_log.Rmd"))  # placeholder

  clarify_restore_prompt <- reactiveVal("")

  selected_files   <- reactiveVal(character(0))
  selected_objects <- reactiveVal(character(0))
  file_schema_cache <- reactiveVal(list())

  observe({
    paths <- selected_files()
    cache <- isolate(file_schema_cache())
    new_paths <- setdiff(paths, names(cache))
    if (length(new_paths) > 0) {
      new_schemas <- lapply(new_paths, extract_file_schema)
      names(new_schemas) <- new_paths
      suspicious <- new_paths[vapply(new_schemas,
                                     function(s) isTRUE(s$suspicious), logical(1))]
      for (p in suspicious) {
        showNotification(
          paste0("⚠ ", basename(p), " may not have column headers — ",
                 "the first row looks like data, not variable names. ",
                 "If so, load it in R with header=FALSE and add the object to the context instead."),
          type = "warning", duration = 12
        )
      }
      cache <- c(cache, new_schemas)
    }
    stale <- setdiff(names(cache), paths)
    if (length(stale) > 0) cache <- cache[setdiff(names(cache), stale)]
    file_schema_cache(cache)
  })

  # --- Package tracking & installation state ---------------------------------
  loaded_packages   <- reactiveVal(character(0))   # pkgs library()'d this conversation
  install_proc      <- reactiveVal(NULL)            # callr background process
  install_pending   <- reactiveVal(NULL)            # closure to call after install
  install_pkg_names <- reactiveVal(character(0))    # names of packages being installed

  # Freeze/unfreeze main action buttons during background install
  freeze_for_install <- function() {
    for (btn in c("ask_code", "ask_plain", "run_code", "fix_code"))
      shinyjs::disable(btn)
  }
  unfreeze_after_install <- function() {
    for (btn in c("ask_code", "ask_plain", "run_code", "fix_code"))
      shinyjs::enable(btn)
  }

  # Check whether any packages in `code` need installing; if so, launch
  # background install and defer `on_success`.  If nothing is missing,
  # calls on_success() immediately (synchronously).
  check_and_install_if_needed <- function(code, on_success) {
    pkg_info <- extract_pkg_info(code)
    if (length(pkg_info) == 0) { on_success(); return(invisible(NULL)) }

    pkg_names    <- vapply(pkg_info, `[[`, character(1), "pkg")
    missing_mask <- !vapply(pkg_names, requireNamespace, logical(1), quietly = TRUE)
    missing_info <- pkg_info[missing_mask]

    if (length(missing_info) == 0) { on_success(); return(invisible(NULL)) }

    missing_names <- vapply(missing_info, `[[`, character(1), "pkg")
    has_github    <- !is.na(vapply(missing_info, `[[`, character(1), "github"))
    github_refs   <- vapply(missing_info[has_github], `[[`, character(1), "github")
    cran_pkgs     <- missing_names[!has_github]
    repos         <- get_cran_repos()

    install_pending(on_success)
    install_pkg_names(missing_names)
    freeze_for_install()

    showModal(modalDialog(
      title = "Installing packages — please wait",
      tags$p(tags$strong("Installing: "), paste(missing_names, collapse = ", ")),
      tags$p(em("All other functions are paused until installation completes.")),
      footer    = NULL,
      easyClose = FALSE
    ))

    proc <- tryCatch(
      start_pkg_install(cran_pkgs, github_refs, repos),
      error = function(e) NULL
    )
    if (is.null(proc)) {
      removeModal()
      unfreeze_after_install()
      ui_busy(FALSE)
      output$run_status <- renderUI(
        tags$span("Could not start installation — check your internet connection.",
                  style = "color:#b22222; font-weight:bold;")
      )
      install_pending(NULL); install_pkg_names(character(0))
      return(invisible(NULL))
    }
    install_proc(proc)
  }

  # Poll the background install process every second
  observe({
    proc <- install_proc()
    if (is.null(proc)) return()
    invalidateLater(1000, session)
    if (proc$is_alive()) return()

    install_proc(NULL)
    removeModal()

    pkg_names     <- install_pkg_names()
    still_missing <- pkg_names[
      !vapply(pkg_names, requireNamespace, logical(1), quietly = TRUE)]

    unfreeze_after_install()
    pending <- install_pending()
    install_pending(NULL); install_pkg_names(character(0))

    if (length(still_missing) == 0) {
      output$run_status <- renderUI(
        tags$span("Package installation successful.",
                  style = "color:#1a7a1a; font-weight:bold;")
      )
      if (!is.null(pending)) pending()
    } else {
      out_lines <- tryCatch(proc$read_all_output_lines(), error = function(e) character(0))
      err_lines <- tryCatch(proc$read_all_error_lines(),  error = function(e) character(0))
      combined  <- Filter(nzchar, c(out_lines, err_lines))
      output$run_status <- renderUI(
        tags$span(
          paste0("Installation failed for: ", paste(still_missing, collapse = ", "), ". ",
                 "See R output tab for details."),
          style = "color:#b22222; font-weight:bold;"
        )
      )
      if (length(combined) > 0) {
        console_output_rv(combined)
        plot_files_rv(character(0))
      }
      last_run_failed(TRUE)
      last_error_msg(paste(combined, collapse = "\n"))
      goto_tab("R output")
      ui_busy(FALSE)
    }
  })

  # --- Preferences -----------------------------------------------------------
  # Priority: pause state > persisted user prefs > built-in defaults
  .saved_prefs <- load_saved_prefs()
  pref_val <- function(key, default) {
    if (!is.null(ps$prefs[[key]])) ps$prefs[[key]]
    else if (!is.null(.saved_prefs[[key]])) .saved_prefs[[key]]
    else default
  }
  prefs <- reactiveValues(
    coding          = pref_val("coding",          "tidyverse"),
    plotting        = pref_val("plotting",         "ggplot2"),
    mapping         = pref_val("mapping",          "tmap"),
    model           = MODEL_SONNET,   # fixed: a model name saved in old preferences is ignored
    img_format      = pref_val("img_format",       "png"),
    img_quality     = pref_val("img_quality",      "Medium"),
    img_size        = pref_val("img_size",         "Journal double col. (180x120 mm)"),
    max_lines       = isolate(max_lines_cap_val()),
    comment_density = pref_val("comment_density",  "Minimal"),
    language        = {
      .lp <- file.path(tools::R_user_dir("classmate", "config"), "language.rds")
      if (file.exists(.lp)) tryCatch(readRDS(.lp), error = function(e) "English") else "English"
    }
  )


  show_preferences_modal <- function() {
    lang         <- prefs$language
    lang_choices <- if (tolower(lang) == "english") "English" else c(lang, "English")
    showModal(modalDialog(
      title = "Preferences",
      tags$p(style = "margin: 0;", tags$label("Claude Model:"), " ", MODEL_SONNET_LABEL),
      hr(),
      # Same width as the drop-downs below (first grid column), with the Change button beside it
      tags$div(class = "pref-grid",
        selectInput("pref_language", "Response language:", choices = lang_choices,
                    selected = lang, width = "100%"),
        tags$div(style = "align-self: end; margin-bottom: 15px;",
          actionButton("pref_lang_change", "Change", class = "btn-default btn-sm"))),
      tags$p(em(
        "The language classmate uses for explanations. R code is always written in English."
      )),
      hr(),
      tags$h5("Preferred approach for..."),
      tags$div(class = "pref-grid",
        selectInput("pref_coding", "Coding:",
                    choices = c("tidyverse", "Base R"), selected = prefs$coding, width = "100%"),
        selectInput("pref_plotting", "Plotting:",
                    choices = c("ggplot2", "Base R"), selected = prefs$plotting, width = "100%"),
        selectInput("pref_mapping", "Mapping:",
                    choices = c("tmap", "ggplot2", "leaflet", "mapsf", "Base R"),
                    selected = prefs$mapping, width = "100%")),
      tags$p(em(
        "These are defaults Claude will lean towards, not hard rules — ",
        "you can always ask for something different in your prompt."
      )),
      hr(),
      tags$h5("Image export defaults"),
      tags$div(class = "pref-grid",
        selectInput("pref_img_format", "Format:",
          choices = c("png", "jpg", "pdf", "tif", "bmp"), selected = prefs$img_format, width = "100%"),
        selectInput("pref_img_quality", "Quality / resolution:",
          choices = c("Low", "Medium", "High"), selected = prefs$img_quality, width = "100%"),
        selectInput("pref_img_size", "Target size:",
          choices = c(
            "Journal single col. (90x90 mm)",
            "Journal double col. (180x120 mm)",
            "A4 (210x297 mm)", "A3 (297x420 mm)", "Letter (8.5x11 in)",
            "4x6 in / 10x15 cm", "5x7 in / 13x18 cm", "8x10 in / 20x25 cm",
            "Square 6x6 in"
          ), selected = prefs$img_size, width = "100%")),
      tags$p(em(
        "These guide Claude when writing image-saving code (ggsave, png, pdf, tiff, etc.).",
        "Low ≈ web/screen, Medium ≈ standard print, High ≈ high-quality print.",
        "As with other preferences, these are defaults — explicit prompts or a better",
        "fit for the task always take priority."
      )),
      hr(),
      tags$h5("Code style"),
      tags$div(class = "pref-grid",
        tags$div(
          tags$label("Maximum code length:"),
          tags$p(
            style = "margin: 6px 0 0 0;",
            tags$strong(if (is.infinite(isolate(max_lines_cap_val()))) "Unlimited"
                        else paste(isolate(max_lines_cap_val()), "lines")),
            tags$span(style = "color: #888;", " (set by your key)")
          )
        ),
        selectInput("pref_comment_density", "Comments in generated code:",
          choices  = c("None", "Minimal", "Most"),
          selected = prefs$comment_density, width = "100%")
      ),
      tags$p(em(
        "Maximum length is set by your key. It is a soft target: Claude will aim to stay",
        "within it but may exceed it when the task genuinely requires more. Scripts you",
        "load directly will also generate a warning if they exceed this limit.",
        "Comment levels — None: no comments at all; Minimal: brief section markers only",
        "(e.g. '# Load data'); Most: section markers plus short notes on non-obvious lines.",
        "Even at Most, line-by-line commenting is avoided."
      )),
      # Key file buttons at the left of the footer, Close at the right
      footer = tags$div(style = "display: flex; justify-content: space-between; align-items: center;",
        tags$div(
          actionButton("pref_load_key", "Load new key file", class = "btn-default btn-sm"),
          actionButton("pref_delete_key", "Delete key file", class = "btn-default btn-sm",
                       style = "margin-left: 6px;")),
        modalButton("Close"))
    ))
  }

  observeEvent(input$preferences, show_preferences_modal())

  # Language dropdown in Preferences: quick switch between English and set language
  observeEvent(input$pref_language, {
    lang <- input$pref_language
    prefs$language <- lang
    config_dir <- tools::R_user_dir("classmate", "config")
    dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
    tryCatch(saveRDS(lang, file.path(config_dir, "language.rds")),
             error = function(e) invisible(NULL))
  })

  # "Change" button — show filterable language picker
  observeEvent(input$pref_lang_change, {
    showModal(modalDialog(
      title = "Choose response language",
      textInput("lang_filter", NULL, placeholder = "Type to filter languages…",
                width = "100%"),
      div(style = "height: 260px; overflow-y: auto; border: 1px solid #ddd; border-radius: 4px;",
        selectInput("lang_picker_select", NULL,
          choices   = CLASSMATE_LANGUAGES,
          selected  = if (prefs$language %in% CLASSMATE_LANGUAGES) prefs$language else "English",
          size      = 15,
          selectize = FALSE,
          width     = "100%")
      ),
      tags$p(style = "margin-top: 6px; font-size: 0.85em; color: #888;",
        "Click a language, then press OK."),
      footer = tagList(
        actionButton("lang_picker_ok",     "OK",     class = "btn-primary"),
        actionButton("lang_picker_cancel", "Cancel", class = "btn-default")
      ),
      size      = "s",
      easyClose = FALSE
    ))
  })

  # Filter the language list as the user types
  observeEvent(input$lang_filter, {
    filter <- trimws(input$lang_filter %||% "")
    langs  <- if (nzchar(filter))
      CLASSMATE_LANGUAGES[grepl(filter, CLASSMATE_LANGUAGES, ignore.case = TRUE)]
    else
      CLASSMATE_LANGUAGES
    current <- prefs$language
    updateSelectInput(session, "lang_picker_select",
      choices  = langs,
      selected = if (current %in% langs) current else character(0))
  }, ignoreNULL = FALSE)

  # OK — save selection and return to Preferences
  observeEvent(input$lang_picker_ok, {
    lang <- input$lang_picker_select
    if (!is.null(lang) && nzchar(lang)) {
      prefs$language <- lang
      config_dir <- tools::R_user_dir("classmate", "config")
      dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)
      tryCatch(saveRDS(lang, file.path(config_dir, "language.rds")),
               error = function(e) invisible(NULL))
    }
    show_preferences_modal()
  })

  # Cancel — return to Preferences without saving
  observeEvent(input$lang_picker_cancel, show_preferences_modal())

  observeEvent(input$pref_coding,          { prefs$coding          <- input$pref_coding })
  observeEvent(input$pref_plotting,        { prefs$plotting        <- input$pref_plotting })
  observeEvent(input$pref_mapping,         { prefs$mapping         <- input$pref_mapping })
  observeEvent(input$pref_img_format,      { prefs$img_format      <- input$pref_img_format })
  observeEvent(input$pref_img_quality,     { prefs$img_quality     <- input$pref_img_quality })
  observeEvent(input$pref_img_size,        { prefs$img_size        <- input$pref_img_size })
  observeEvent(input$pref_comment_density, {
    old_density <- prefs$comment_density
    new_density <- input$pref_comment_density
    prefs$comment_density <- new_density
    if (old_density != new_density) {
      current_code <- isolate(input$code_editor %||% "")
      if (nzchar(trimws(current_code)) && !isTRUE(ui_busy())) {
        comment_instruction <- switch(new_density,
          None    = paste(
            "Remove all comments from this R code.",
            "Delete every line that consists only of a # comment.",
            "Also remove any trailing inline comments (# ...) from code lines.",
            "Do not change any code logic, names, values, or structure."
          ),
          Minimal = paste(
            "Adjust the comments in this R code so that it has only brief",
            "section-marker comments dividing major logical steps",
            "(e.g. '# Load data', '# Fit model', '# Plot results').",
            "Remove any inline comments and any over-detailed explanatory comments.",
            "Do not change any code logic, names, values, or structure."
          ),
          Most    = paste(
            "Adjust the comments in this R code so that it has section-marker comments",
            "dividing major logical steps, plus brief inline notes (≤ 8 words each)",
            "on any lines that would not be immediately obvious to a typical student.",
            "Do not comment every line — only add inline notes to genuinely non-obvious choices.",
            "Do not change any code logic, names, values, or structure."
          )
        )
        removeModal()
        output$run_status <- renderUI(tags$span(
          "Adjusting comments...", style = "color: #888;"
        ))
        tryCatch({
          result <- call_claude(
            messages = list(list(
              role    = "user",
              content = paste0(
                comment_instruction,
                "\n\nReturn ONLY the modified R code with no explanation,",
                " introduction, or markdown code fences.\n\n",
                current_code
              )
            )),
            model         = MODEL_HAIKU,
            max_tokens    = 2600,
            system_prompt = paste(
              "You are a code comment editor for R code.",
              "Your only task is to adjust comment lines to the requested level.",
              "Never change any executable code. Return only the raw R code."
            ),
            api_key      = api_key_val(),
            cache_system = FALSE
          )
          new_code <- trimws(result$text)
          # Strip accidental fences if model wraps them anyway
          new_code <- gsub("^```(?:r|R)?\\s*\n?", "", new_code, perl = TRUE)
          new_code <- gsub("\n?```\\s*$",           "", new_code, perl = TRUE)
          if (nzchar(new_code)) {
            updateAceEditor(session, "code_editor", value = new_code)
            output$run_status <- renderUI(tags$span(
              "Comments updated.", style = "color: #888;"
            ))
          } else {
            output$run_status <- renderUI(NULL)
          }
        }, error = function(e) {
          output$run_status <- renderUI(tags$span(
            "Could not update comments.", style = "color: #b22222;"
          ))
        })
      }
    }
  })

  # Persist prefs to disk whenever any value changes
  observe({
    save_prefs(reactiveValuesToList(prefs))
  })

  if (!is.null(ps)) {
    if (length(ps$selected_files)   > 0) selected_files(ps$selected_files)
    if (length(ps$selected_objects) > 0) selected_objects(ps$selected_objects)
    session$onFlushed(function() {
      updateTextAreaInput(session, "prompt",      value = ps$prompt      %||% "")
      updateAceEditor(session, "code_editor", value =ps$code_editor %||% "")
    }, once = TRUE)
  }

  # --- Ask / Clear / New conversation ----------------------------------------
  observeEvent(input$clear_prompt, {
    if (!identical(ask_mode(), "answer")) hide_changes_tab()
    updateTextAreaInput(session, "prompt", value = "")
    if (protection_notice_active()) protection_notice_active(FALSE)
  })

  do_new_conversation_reset <- function() {
    save_current_conversation_if_nonempty()
    # Auto-save code log for this conversation before clearing
    if (length(all_log_entries()) > 0) do_save_log()
    conv_count_rv(conv_count_rv() + 1L)
    hide_changes_tab()
    conversation_history(list())
    run_history(list())
    unconf_code(list())
    answer_latest(NULL)
    answer_hist(list())
    interactions(list())
    active_code_id(NULL)
    placeholder_map(character(0))
    set_ask_mode("code")
    pending_log_entries(list())
    all_log_entries(list())
    loaded_packages(character(0))
    last_extracted_code("")
    last_prompt_for_code("")
    last_code_description("")
    loaded_script_name(NULL)
    last_logged_code("")
    last_run_failed(FALSE)
    last_run_code(NULL)
    code_before_fix(NULL)
    console_output_rv(character(0)); warnings_rv(character(0)); plot_files_rv(character(0)); diff_rv(NULL)
    # Note: selected_files and selected_objects are intentionally NOT cleared —
    # the user's context files carry over into the new conversation.
    prompt_history(character(0))
    updateTextAreaInput(session, "prompt",      value = "")
    updateAceEditor(session, "code_editor", value = "")
    output$run_status <- renderUI(NULL)
    disable("save_block")
  }

  output$conv_remaining_ui <- renderUI({
    max_c <- max_conversations_val()
    if (is.null(max_c)) return(NULL)
    remaining <- max(max_c - conv_count_rv(), 0L)
    colour <- if (remaining == 0) "#dc3545" else if (remaining == 1) "#fd7e14" else if (help_mode()) "#555" else "#888"
    tags$div(
      style = paste0("font-size: 0.75em; color: ", colour, "; margin-top: 2px; white-space: nowrap;"),
      paste0(remaining, " new conversation", if (remaining != 1) "s" else "", " remaining")
    )
  })

  observeEvent(input$new_conversation, {
    # Check conversation limit before proceeding
    max_c <- max_conversations_val()
    if (!is.null(max_c) && conv_count_rv() >= max_c) {
      showModal(modalDialog(
        title = "Conversation limit reached",
        tags$p(paste0(
          "You have used all ", max_c, " conversation",
          if (max_c != 1) "s" else "",
          " permitted in this session."
        )),
        tags$p("Please quit and restart Classmate, or ask your instructor for guidance."),
        footer = modalButton("OK"),
        easyClose = TRUE
      ))
      return()
    }
    showModal(modalDialog(
      title = "Start a new conversation?",
      tags$p("This will clear the Code editor, R output, Past code, and Past prompts. The current conversation will be saved to Past conversations from which previous code and prompts can be recalled. The code notebook for this conversation will be saved automatically."),
      footer = tagList(
        actionButton("confirm_new_conversation", "Yes, start new conversation", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  })

  observeEvent(input$confirm_new_conversation, {
    removeModal()
    do_new_conversation_reset()
  })

  # --- Quick Console ---------------------------------------------------------
  qc_history <- reactiveVal(list())  # list of list(input, output, is_error, plot_path)

  # Current working directory, shown in the Quick Console; changes via the
  # Change Working Directory button or setwd() in the console.
  wd_rv <- reactiveVal(project_root())
  note_wd_change <- function(path) {
    wd_rv(path)
    showNotification(paste("Working directory changed to", path),
                     type = "message", duration = 6)
  }
  observe({
    invalidateLater(3000, session)
    fb <- .cm$fallback
    if (!is.null(fb)) {
      .cm$fallback <- NULL
      wd_rv(fb$to)
      showNotification(paste0("The folder ", fb$from, " no longer exists, so Classmate has switched to ",
                              fb$to, ". Use Change Working Directory (in the Quick Console) to choose another."),
                       type = "warning", duration = 15)
    }
  })
  output$qc_wd_ui <- renderUI(
    tags$p(style = "font-size: 0.8em; color: #555; margin: 0 0 4px 0;",
           "Working directory: ", tags$code(wd_rv()))
  )

  show_qc_modal <- function() {
    showModal(modalDialog(
      title = "Quick Console",
      size  = "l",
      tags$div(
        style = paste0("height: 280px; overflow-y: auto; background: #fafafa;",
                       " border: 1px solid #ddd; border-radius: 4px;",
                       " padding: 8px; margin-bottom: 8px;"),
        uiOutput("qc_output_ui")
      ),
      shinyAce::aceEditor(
        "qc_input", value = "", mode = "r", theme = "chrome",
        height = "90px", fontSize = 13,
        showLineNumbers = FALSE, highlightActiveLine = FALSE,
        hotkeys = list(run_key = list(win = "Ctrl-Return", mac = "Command-Return"))
      ),
      uiOutput("qc_wd_ui"),
      tags$p(em(style = "font-size:0.8em; color:#888;",
        "Same workspace as the main app. ",
        "q() and quit() close the Quick Console, not your R session.")),
      footer = tagList(
        actionButton("qc_run",   "Run",         class = "btn-success"),
        actionButton("qc_clear", "Clear output", style = "margin-left: 4px;"),
        actionButton("qc_clear_workspace", "Clear Workspace", style = "margin-left: 4px;"),
        actionButton("qc_chdir", "Change Working Directory", style = "margin-left: 4px;"),
        tags$span(style = "flex: 1;"),
        modalButton("Close")
      ),
      easyClose = FALSE
    ))
    shinyjs::runjs('
      setTimeout(function() {
        var ed = ace.edit("qc_input");
        if (!ed) return;
        ed.commands.addCommand({
          name: "smartEnter",
          bindKey: {win: "Return", mac: "Return"},
          exec: function(ed) {
            var code = ed.getValue();
            if (isCompleteR(code.trim())) {
              Shiny.setInputValue("qc_run", Math.random(), {priority: "event"});
            } else {
              ed.insert("\\n");
            }
          }
        });
      }, 300);
    ')
  }

  run_in_qc <- function(code) {
    warnings_seen  <- character(0)
    console_output <- character(0)
    # Run in the user's project directory, not the Shiny app directory
    .enter <- cm_enter_root()
    on.exit(.enter$restore(), add = TRUE)
    if (!.enter$ok)
      return(list(output = paste("Classmate could not open your working directory.",
                                 "Use Change Working Directory to choose a folder."),
                  is_error = TRUE, plot_path = NULL))
    # Temporarily shadow q/quit/stopApp/ask in globalenv to intercept them
    .qc_classmate_fns <- c("talk", "whisper", "raisehand", "rh", "ssshh",
                           "reset_key", "classmate_reset", "classmate_speaks",
                           "classmate_make_key", "classmate_config_show")
    shadow <- c(
      list(
        q       = if (exists("q",       .GlobalEnv, inherits = FALSE)) get("q",       .GlobalEnv) else NULL,
        quit    = if (exists("quit",    .GlobalEnv, inherits = FALSE)) get("quit",    .GlobalEnv) else NULL,
        stopApp = if (exists("stopApp", .GlobalEnv, inherits = FALSE)) get("stopApp", .GlobalEnv) else NULL
      ),
      setNames(lapply(.qc_classmate_fns, function(nm)
        if (exists(nm, .GlobalEnv, inherits = FALSE)) get(nm, .GlobalEnv) else NULL
      ), .qc_classmate_fns)
    )
    assign("q",       function(...) stop("__QC_QUIT__"),    envir = .GlobalEnv)
    assign("quit",    function(...) stop("__QC_QUIT__"),    envir = .GlobalEnv)
    assign("stopApp", function(...) stop("__QC_STOPAPP__"), envir = .GlobalEnv)
    .qc_not_here_msg <- paste0(
      "This function is not available inside Classmate's Quick Console.\n",
      "Please leave Classmate (using Pause or Quit with Code Log) and run it from your main R console."
    )
    for (.qc_fn in .qc_classmate_fns)
      assign(.qc_fn, local({ msg <- .qc_not_here_msg; function(...) message(msg) }), envir = .GlobalEnv)
    rm(.qc_classmate_fns, .qc_fn, .qc_not_here_msg)
    on.exit({
      for (nm in names(shadow)) {
        if (is.null(shadow[[nm]])) {
          if (exists(nm, .GlobalEnv, inherits = FALSE)) rm(list = nm, envir = .GlobalEnv)
        } else {
          assign(nm, shadow[[nm]], envir = .GlobalEnv)
        }
      }
    })
    # Capture plot output to a temp PNG
    plot_file   <- tempfile(fileext = ".png")
    dev_before  <- dev.cur()
    png(plot_file, width = 900, height = 650, res = 96)
    dev_qc <- dev.cur()
    on.exit({ if (dev.cur() == dev_qc) dev.off() }, add = TRUE)
    # Package installs routinely take longer than the 30 s limit and cannot be interrupted
    # part-way, so they run without one.
    installing <- code_installs_packages(code)
    result <- tryCatch({
      if (!installing) setTimeLimit(elapsed = 30, transient = TRUE)
      console_output <- capture.output({
        withCallingHandlers(
          source(textConnection(code), local = FALSE, print.eval = TRUE),
          warning = function(w) {
            warnings_seen <<- c(warnings_seen, conditionMessage(w))
            invokeRestart("muffleWarning")
          }
        )
      })
      if (dev.cur() == dev_qc) dev.off()
      out <- strip_ansi(console_output)
      if (length(warnings_seen) > 0)
        out <- c(out, paste0("Warning: ", warnings_seen))
      plot_path <- if (file.exists(plot_file) && file.info(plot_file)$size > 0)
        plot_file else NULL
      list(output = paste(out, collapse = "\n"), is_error = FALSE, plot_path = plot_path)
    }, error = function(e) {
      if (dev.cur() == dev_qc) tryCatch(dev.off(), error = function(e) NULL)
      msg <- conditionMessage(e)
      if (grepl("__QC_QUIT__", msg))
        return(list(output = "(q() / quit() is not available here — close the Quick Console to return to Classmate)", is_error = FALSE, plot_path = NULL))
      if (grepl("__QC_STOPAPP__", msg))
        return(list(output = "(stopApp() is not available inside the Quick Console)", is_error = FALSE, plot_path = NULL))
      if (grepl("reached elapsed time limit", msg))
        return(list(output = "Timed out after 30 seconds. The operation was interrupted.", is_error = TRUE, plot_path = NULL, timed_out = TRUE))
      list(output = strip_ansi(msg), is_error = TRUE, plot_path = NULL)
    }, finally = {
      # Always clear the limit straight away, so a late expiry can never surface outside the handler
      tryCatch(setTimeLimit(elapsed = Inf, transient = FALSE), error = function(e) NULL)
    })
    # If the code changed the working directory (setwd()), make that the new root
    now_wd <- tryCatch(normalizePath(getwd(), winslash = "/"), error = function(e) NA_character_)
    if (!is.na(now_wd) && !identical(now_wd, project_root())) {
      r <- cm_set_root(now_wd)
      if (isTRUE(r$ok) && isTRUE(r$changed)) {
        result$wd_changed <- r$path
        result$output <- paste0(
          if (nzchar(trimws(result$output))) paste0(result$output, "\n") else "",
          "[Classmate] Working directory is now ", r$path)
      }
    }
    result
  }

  output$qc_output_ui <- renderUI({
    hist <- qc_history()
    if (length(hist) == 0)
      return(tags$div(
        style = "color: #888; font-size: 0.85em; font-style: italic;",
        "Type R code below and press Run (or Ctrl+Enter)."
      ))
    items <- lapply(hist, function(h) {
      tags$div(
        tags$div(
          style = "color: #2c5f8a; font-family: monospace; font-size: 0.85em; margin-top: 6px; white-space: pre-wrap;",
          paste0("> ", gsub("\n", "\n  ", trimws(h$input)))
        ),
        if (nzchar(trimws(h$output)))
          tags$div(
            style = paste0("font-family: monospace; font-size: 0.85em; white-space: pre-wrap; ",
                           if (h$is_error) "color: #b22222;" else "color: #333;"),
            trimws(h$output)
          )
      )
    })
    tags$div(items)
  })

  observeEvent(input$quick_console, {
    qc_history(list())
    show_qc_modal()
  })

  qc_pending_code <- reactiveVal("")

  do_qc_run <- function(code = isolate(input$qc_input %||% ""), allow_install = FALSE) {
    if (!nzchar(trimws(code))) return()
    if (!allow_install && code_installs_packages(code)) {
      qc_pending_code(code)
      showModal(modalDialog(
        title = "Installing packages? Better done in RStudio",
        tags$p("The Quick Console is designed for quick operations. Installing packages can take several minutes and cannot be interrupted, so Classmate may seem frozen while it runs."),
        tags$p("We recommend that you Pause Classmate (your session is saved), install the package in the main RStudio console, and then restart Classmate to carry on where you left off."),
        tags$p(em("You can still install it here if you prefer: Classmate will wait for it to finish.")),
        footer = tagList(
          actionButton("qc_install_pause", "Pause Classmate", class = "btn-primary"),
          actionButton("qc_install_anyway", "Install here anyway"),
          actionButton("qc_install_close", "Close")
        ),
        easyClose = FALSE
      ))
      return(invisible(NULL))
    }
    result <- run_in_qc(code)
    if (!is.null(result$wd_changed)) wd_rv(result$wd_changed)
    qc_history(c(qc_history(), list(list(
      input     = code,
      output    = result$output,
      is_error  = result$is_error,
      plot_path = result$plot_path
    ))))
    shinyAce::updateAceEditor(session, "qc_input", value = "")
    if (isTRUE(result$timed_out)) {
      showModal(modalDialog(
        title = "That took too long for the Quick Console",
        tags$p("The Quick Console is designed for quick operations, so it stops any command that runs for more than 30 seconds. Your command was interrupted."),
        tags$p("For a longer process, we recommend that you Pause Classmate (your session is saved), run the command in the main RStudio console, and then restart Classmate to carry on where you left off."),
        footer = tagList(
          actionButton("qc_timeout_pause", "Pause Classmate", class = "btn-primary"),
          actionButton("qc_timeout_close", "Close")
        ),
        easyClose = FALSE
      ))
    } else if (!is.null(result$plot_path)) {
      plot_src <- paste0("classmate_plots/", basename(result$plot_path))
      showModal(modalDialog(
        title = "Plot",
        size  = "l",
        tags$div(
          style = "text-align: center;",
          tags$img(src = plot_src, style = "max-width: 100%; height: auto;")
        ),
        footer = actionButton("qc_plot_close", "Close", class = "btn-primary"),
        easyClose = FALSE
      ))
    }
  }

  observeEvent(input$qc_run,          { do_qc_run() })
  observeEvent(input$qc_input_run_key, { do_qc_run() })

  observeEvent(input$qc_timeout_close, { removeModal(); show_qc_modal() })
    observeEvent(input$qc_install_close, {
    removeModal()
    show_qc_modal()
    shinyAce::updateAceEditor(session, "qc_input", value = qc_pending_code())   # keep what they typed
  })
    observeEvent(input$qc_install_anyway, {
    removeModal()
    show_qc_modal()
    do_qc_run(qc_pending_code(), allow_install = TRUE)
  })

  observeEvent(input$qc_plot_close, {
    removeModal()
    show_qc_modal()
  })

  observeEvent(input$qc_clear, {
    qc_history(list())
  })

  add_qc_entry <- function(input, output, is_error = FALSE)
    qc_history(c(qc_history(), list(list(
      input = input, output = output, is_error = is_error, plot_path = NULL))))

  # --- Clear Workspace (from the Quick Console) -------------------------------
  observeEvent(input$qc_clear_workspace, {
    showModal(modalDialog(
      title = "Clear workspace?",
      "Are you sure? This will remove all objects from the R workspace (rm(list = ls())).",
      footer = tagList(
        actionButton("qc_cw_confirm", "Yes, clear workspace",
          style = "background-color: #e67e22; border-color: #ca6f1e; color: white;"),
        actionButton("qc_cw_cancel", "Cancel")
      ),
      easyClose = FALSE
    ))
  })
  observeEvent(input$qc_cw_cancel, show_qc_modal())
  observeEvent(input$qc_cw_confirm, {
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    selected_objects(character(0))
    add_qc_entry("rm(list = ls())", "[Classmate] Workspace cleared.")
    show_qc_modal()
  })

  # --- Change Working Directory (from the Quick Console) ----------------------
  chdir_path <- reactiveVal(project_root())

  show_chdir_modal <- function(start = project_root()) {
    chdir_path(start)
    showModal(modalDialog(
      title = "Change working directory",
      tags$p("Current working directory: ", tags$code(wd_rv())),
      fluidRow(
        column(9, textInput("chdir_text", NULL, value = start, width = "100%")),
        column(3, actionButton("chdir_goto", "Go to", width = "100%"))
      ),
      div(style = "display: flex; gap: 6px; margin-bottom: 8px;",
        actionButton("chdir_up",   "Up one level", class = "btn-sm"),
        actionButton("chdir_home", "Home",         class = "btn-sm"),
        actionButton("chdir_cur",  "Current folder", class = "btn-sm")
      ),
      uiOutput("chdir_list_ui"),
      tags$p(style = "color: #888; font-size: 0.85em; margin-top: 6px;",
        "Click a folder to open it, then press Use this folder.",
        "Your R session (e.g. RStudio) will be switched to the same folder."),
      footer = tagList(
        actionButton("chdir_use", "Use this folder", class = "btn-primary"),
        actionButton("chdir_cancel", "Cancel")
      ),
      easyClose = FALSE
    ))
  }

  browse_to <- function(path) {
    p <- tryCatch(normalizePath(path.expand(path), winslash = "/", mustWork = TRUE),
                  error = function(e) NA_character_)
    if (is.na(p) || !dir.exists(p)) {
      showNotification("That folder could not be found.", type = "error", duration = 5)
      return(invisible(FALSE))
    }
    chdir_path(p)
    updateTextInput(session, "chdir_text", value = p)
    invisible(TRUE)
  }

  output$chdir_list_ui <- renderUI({
    p <- chdir_path()
    dirs <- tryCatch(list.dirs(p, full.names = FALSE, recursive = FALSE),
                     error = function(e) NULL, warning = function(w) NULL)
    if (is.null(dirs))
      return(tags$p(em("This folder cannot be read.")))
    dirs <- sort(dirs[!startsWith(dirs, ".")])
    div(
      style = paste0("height: 240px; overflow-y: auto; border: 1px solid #ddd;",
                     " border-radius: 4px; padding: 4px 8px; background: #fafafa;"),
      if (length(dirs) == 0) tags$p(em("No sub-folders here."), style = "margin: 4px 0;")
      else lapply(dirs, function(d)
        tags$div(tags$a(href = "#", class = "cm-chdir-item", `data-name` = d,
                        paste0("\U0001F4C1 ", d))))
    )
  })

  observeEvent(input$qc_chdir, show_chdir_modal())
  observeEvent(input$chdir_enter, browse_to(file.path(chdir_path(), input$chdir_enter$name)))
  observeEvent(input$chdir_up,   browse_to(dirname(chdir_path())))
  observeEvent(input$chdir_home, browse_to("~"))
  observeEvent(input$chdir_cur,  browse_to(project_root()))
  observeEvent(input$chdir_goto, browse_to(input$chdir_text %||% ""))
  observeEvent(input$chdir_cancel, show_qc_modal())
  observeEvent(input$chdir_use, {
    target <- chdir_path()
    r <- cm_set_root(target)
    if (!isTRUE(r$ok)) {
      showNotification(r$error %||% "Could not change the working directory.",
                       type = "error", duration = 6)
      return(invisible(NULL))
    }
    if (isTRUE(r$changed)) {
      wd_rv(r$path)
      add_qc_entry(paste0('setwd("', r$path, '")'),
                   paste0("[Classmate] Working directory is now ", r$path))
      if (!isTRUE(r$outputs_ok))
        showNotification(paste("Classmate could not create an outputs folder in this",
                               "directory; it may be read-only."),
                         type = "warning", duration = 8)
    } else {
      add_qc_entry("# working directory unchanged", paste("[Classmate] Already in", r$path))
    }
    show_qc_modal()
  })

  # --- Ask for Code ----------------------------------------------------------
  do_ask_code <- function(current_prompt) {
    if (isTRUE(quota_exceeded())) { show_quota_modal(); return(invisible(NULL)) }
    capture_active_code()    # save what the previous code interaction looked like before it is cleared
    autorun_text <- NULL
    pending_export(NULL)
    hide_changes_tab()
    console_output_rv(character(0)); warnings_rv(character(0)); plot_files_rv(character(0)); diff_rv(NULL)
    disable("ask_code"); disable("ask_plain")
    tryCatch({
      user_message <- build_user_message(
        file_paths          = context_files(),
        object_names        = context_objects(),
        current_code        = input$code_editor,
        last_known_code     = last_extracted_code(),
        run_history         = run_history(),
        user_prompt         = current_prompt,
        last_console_output = console_output_rv(),
        file_schemas        = file_schema_cache()
      )
      new_user_turn   <- list(role = "user", content = user_message)
      history_so_far  <- c(conversation_history(), list(new_user_turn))
      windowed_prompt <- c(take_last_n(conversation_history(), MAX_HISTORY_TURNS - 1), list(new_user_turn))

      api_result <- tryCatch(
        call_claude(
          messages      = windowed_prompt,
          model         = prefs$model,
          max_tokens    = 1100,
          system_prompt = build_system_prompt(prefs$coding, prefs$plotting, prefs$mapping,
                                              prefs$img_format, prefs$img_quality, prefs$img_size,
                                              prefs$max_lines, prefs$comment_density,
                                              loaded_packages(), prefs$language)
        ),
        error = function(e) list(text = "", stop_reason = "error")
      )
      raw_response <- api_result$text

      if (!nzchar(trimws(raw_response))) {
        output$run_status <- renderUI(tags$span(
          "No response from Claude. Check your API key and try again.",
          style = "color: #b22222;"
        ))
        return(invisible(NULL))
      }

      # Token limit reached — discard truncated response
      if (identical(api_result$stop_reason, "max_tokens")) {
        showModal(modalDialog(
          title     = "Prompt too complex",
          "The response was cut short. Try asking for a simpler or smaller piece of code, or split the task into steps.",
          easyClose = FALSE,
          footer    = modalButton("OK")
        ))
        return(invisible(NULL))
      }

      # Tier-2 scope check: AI hard stop
      if (grepl("^OUT_OF_SCOPE", trimws(raw_response), ignore.case = TRUE)) {
        showModal(modalDialog(
          title     = "Out of scope",
          "Sorry, this request is out of scope. Classmate is here to help with R programming, data analysis, and GIS tasks.",
          easyClose = FALSE,
          footer    = actionButton("oos_ok_code", "OK", class = "btn-primary")
        ))
        return(invisible(NULL))
      }

      # Clarity check: prompt too vague to write grounded code
      if (grepl("^NEEDS_CLARIFICATION", trimws(raw_response), ignore.case = TRUE)) {
        nc <- parse_needs_clarification(raw_response)
        clarify_restore_prompt(current_prompt)
        showModal(modalDialog(
          title     = "Please Clarify",
          tags$p(if (nzchar(nc$reason)) nc$reason else
            "The prompt is too general to write code using your actual data or objects."),
          if (nzchar(nc$suggestions)) tags$p(em(nc$suggestions)),
          easyClose = FALSE,
          footer    = actionButton("clarify_modify_prompt", "Modify Prompt", class = "btn-primary")
        ))
        return(invisible(NULL))
      }

      # Disclosure risk: would produce publishable output revealing personal data
      if (grepl("^DISCLOSURE_RISK", trimws(raw_response), ignore.case = TRUE)) {
        dr <- parse_disclosure_risk(raw_response)
        clarify_restore_prompt(current_prompt)
        showModal(modalDialog(
          title = "Disclosure Risk",
          tags$p(if (nzchar(dr$reason)) dr$reason else
            "This request would produce output that could disclose personal information about identifiable individuals."),
          tags$p(em("Please modify the request to avoid including personal information in any publishable or exportable output.")),
          easyClose = FALSE,
          footer    = actionButton("clarify_modify_prompt", "Modify Prompt", class = "btn-primary")
        ))
        return(invisible(NULL))
      }

      record_usage(api_result$cost_usd)
      conversation_history(c(history_so_far, list(list(role = "assistant", content = raw_response))))
      prompt_history(c(current_prompt, prompt_history()))
      parts <- split_response_into_text_and_code(raw_response)

      if (nzchar(trimws(parts$code)) && code_too_long(parts$code, prefs$max_lines)) {
        showModal(modalDialog(
          title = "Response too complex",
          tags$p(
            "The code Claude generated exceeds the current code-length limit (",
            prefs$max_lines, " lines). This usually means the prompt covered too much at once."
          ),
          tags$p("Try breaking it into smaller steps — for example, load the data first, ",
                 "then build the analysis, then visualise the results, one prompt at a time. ",
                 "The code has not been added to the editor."),
          footer    = modalButton("OK"),
          easyClose = FALSE
        ))
        enable("ask_code"); enable("ask_plain")
        return(invisible(NULL))
      }

      last_prompt_for_code(current_prompt)
      old_editor_code <- isolate(input$code_editor) %||% ""
      last_extracted_code(parts$code)
      last_code_description(parts$description %||% "")
      loaded_script_name(NULL)
      last_run_failed(FALSE)
      code_before_fix(NULL)
      updateAceEditor(session, "code_editor", value = parts$code)
      output$run_status <- renderUI(NULL)
      d <- if (nzchar(trimws(old_editor_code)))
        compute_diff(old_editor_code, parts$code) else NULL
      diff_rv(d)
      if (!is.null(d)) show_changes_tab() else {
        hide_changes_tab()
        goto_tab("Code")
      }
      sq <- add_interaction(list(kind = "code", prompt = current_prompt, code = parts$code,
                                 description = parts$description %||% "", editor_code = parts$code,
                                 console = character(0), warnings = character(0), plots = character(0),
                                 last_run_code = NULL, failed = FALSE, error = "", diff = d,
                                 tab = if (!is.null(d)) "Changes" else "Code"))
      active_code_id(sq)
      unconf_add(parts$code, current_prompt, parts$description, "ask", sq)
      if (isTRUE(input$autorun_code) && nzchar(trimws(parts$code))) autorun_text <- parts$code
    }, finally = { enable("ask_code"); enable("ask_plain") })
    # AutoRun Code: run what was just generated (passed directly, as the editor's own value has not
    # reached the server yet)
    if (!is.null(autorun_text)) do_run_code(autorun_text)
  }

  observeEvent(input$ask_code, {
    req(input$prompt)
    do_ask_code(input$prompt)
  })

  observeEvent(input$oos_ok_code, {
    removeModal()
    updateTextAreaInput(session, "prompt", value = "")
  })

  # --- Answer button -----------------------------------------------------------
  # Stage 1: Claude either answers directly or asks for a small read-only
  # calculation. Stage 2: the calculation is run in a throw-away environment,
  # its output is scrubbed of individual records, and Claude writes the answer.
  do_ask_answer <- function(current_prompt) {
    if (isTRUE(quota_exceeded())) { show_quota_modal(); return(invisible(NULL)) }
    ui_busy(TRUE)
    updateTextAreaInput(session, "prompt", value = "")
    disable("ask_code"); disable("ask_plain")

    finish <- function() { ui_busy(FALSE); enable("ask_code"); enable("ask_plain") }
    fail <- function(msg) {
      finish()
      updateTextAreaInput(session, "prompt", value = current_prompt)
      output$run_status <- renderUI(tags$span(msg, style = "color: #b22222;"))
      invisible(NULL)
    }
    call_ai <- function(messages, system_prompt, max_tokens)
      tryCatch(
        call_claude(messages = messages, model = prefs$model,
                    max_tokens = max_tokens, system_prompt = system_prompt),
        error = function(e) list(text = "", stop_reason = "error", cost_usd = 0)
      )

    user_message <- build_user_message(
      file_paths          = context_files(),
      object_names        = context_objects(),
      current_code        = input$code_editor,
      last_known_code     = last_extracted_code(),
      run_history         = run_history(),
      user_prompt         = current_prompt,
      last_console_output = console_output_rv(),
      file_schemas        = file_schema_cache()
    )
    new_user_turn  <- list(role = "user", content = user_message)
    history_so_far <- c(conversation_history(), list(new_user_turn))
    windowed       <- c(take_last_n(conversation_history(), MAX_HISTORY_TURNS - 1), list(new_user_turn))
    plan_prompt    <- build_answer_system_prompt(prefs$language)

    r1  <- withProgress(message = "Thinking...", value = 0.3,
                        call_ai(windowed, plan_prompt, 2000))
    raw <- r1$text
    if (!nzchar(trimws(raw)))
      return(fail("No response from Claude. Check your API key and try again."))
    if (identical(r1$stop_reason, "max_tokens")) {
      finish()
      showModal(modalDialog(
        title = "Answer too long",
        "The answer was cut short. Try asking a narrower question, or split it into steps.",
        easyClose = FALSE, footer = modalButton("OK")))
      return(invisible(NULL))
    }

    trimmed <- trimws(raw)
    if (grepl("^OUT_OF_SCOPE", trimmed, ignore.case = TRUE)) {
      finish()
      showModal(modalDialog(
        title = "Out of scope",
        "Sorry, this request is out of scope. Classmate is here to help with R programming, data analysis, and GIS tasks.",
        easyClose = FALSE,
        footer = actionButton("oos_ok_plain", "OK", class = "btn-primary")))
      return(invisible(NULL))
    }
    if (grepl("^NEEDS_CLARIFICATION", trimmed, ignore.case = TRUE)) {
      finish()
      nc <- parse_needs_clarification(raw)
      clarify_restore_prompt(current_prompt)
      showModal(modalDialog(
        title     = "Please Clarify",
        tags$p(if (nzchar(nc$reason)) nc$reason else
          "The question is too general to answer using your actual data or objects."),
        if (nzchar(nc$suggestions)) tags$p(em(nc$suggestions)),
        easyClose = FALSE,
        footer    = actionButton("clarify_modify_prompt", "Modify Prompt", class = "btn-primary")
      ))
      return(invisible(NULL))
    }
    if (grepl("^DISCLOSURE_RISK", trimmed, ignore.case = TRUE)) {
      finish()
      dr <- parse_disclosure_risk(raw)
      clarify_restore_prompt(current_prompt)
      showModal(modalDialog(
        title = "Disclosure Risk",
        tags$p(if (nzchar(dr$reason)) dr$reason else
          "This request would produce output that could disclose personal information about identifiable individuals."),
        tags$p(em("Please modify the request to avoid including personal information in any publishable or exportable output.")),
        easyClose = FALSE,
        footer    = actionButton("clarify_modify_prompt", "Modify Prompt", class = "btn-primary")
      ))
      return(invisible(NULL))
    }

    record_usage(r1$cost_usd %||% 0)
    plan <- parse_answer_plan(raw)

    finalize <- function(answer_tokens, code = "", results_text = "") {
      capture_active_code()    # keep the code side as it is, for Back to Code / Recall
      prev <- answer_latest()
      if (!is.null(prev)) answer_hist(c(list(prev), answer_hist()))
      sq  <- next_seq()
      ent <- list(seq = sq, question = current_prompt,
                  answer = restore_placeholders(answer_tokens, placeholder_map()),
                  code = code, results = results_text)
      answer_latest(ent)
      add_interaction(list(seq = sq, kind = "answer", prompt = current_prompt, entry = ent))
      # History (which is sent to Claude on later turns) keeps placeholders, not real values
      conversation_history(c(history_so_far,
                             list(list(role = "assistant", content = answer_tokens))))
      prompt_history(c(current_prompt, prompt_history()))
      output$run_status <- renderUI(NULL)
      set_ask_mode("answer")
      finish()
      invisible(NULL)
    }

    if (identical(plan$type, "answer")) return(finalize(plan$text))

    # --- Calculation needed -----------------------------------------------------
    unsafe_msg <- function(bad) paste0(
      "Classmate could not answer this safely without using: ", paste(bad, collapse = ", "),
      ". Try Ask for Code instead.")
    bad <- answer_code_violations(plan$code)
    if (length(bad) > 0) return(fail(unsafe_msg(bad)))
    if (code_too_long(plan$code, max(prefs$max_lines, 40)))
      return(fail(paste("This question needs a longer calculation than the Answer button handles.",
                        "Try splitting it into smaller questions, or use Ask for Code.")))

    code_cur <- plan$code
    raw_cur  <- raw

    # Data frames the calculation may touch (the selected objects, plus any other data
    # frame the code names), as copies in which identifying text is replaced by codes.
    prepare_run <- function(code) {
      syms <- tryCatch(all.names(parse(text = code, keep.source = FALSE)), error = function(e) character(0))
      cand <- union(selected_objects(), intersect(syms, ls(envir = .GlobalEnv)))
      cand <- cand[vapply(cand, function(n)
        exists(n, envir = .GlobalEnv, inherits = FALSE) &&
          is.data.frame(get(n, envir = .GlobalEnv, inherits = FALSE)), logical(1))]
      ps <- pseudonymise_objects(mget(cand, envir = .GlobalEnv), placeholder_map())
      list(ps = ps, masked = cand)
    }

    check_and_install_if_needed(code_cur, function() {
      res <- NULL; usable <- FALSE; red <- list(text = "", map = character(0)); tok_all <- character(0)
      for (attempt in 1:2) {
        prep <- prepare_run(code_cur)
        if (length(prep$ps$failed) > 0)
          return(fail(paste("Classmate could not safely prepare", paste(prep$ps$failed, collapse = ", "),
                            "for this question. Try Ask for Code instead.")))
        tok_all <- prep$ps$map
        bad_id  <- answer_identifier_violations(code_cur, prep$ps$id_cols)
        problem <- NULL
        if (length(bad_id) > 0) {
          res <- list(success = FALSE, output = character(0), warnings = character(0), env = new.env())
          problem <- paste0(
            "Your code used ", paste(bad_id, collapse = ", "), ". Columns of names, IDs or addresses ",
            "contain opaque codes, so text functions, sorting, comparing and number conversion on ",
            "them are not allowed and would give wrong answers. If the question needs part of an ",
            "identifier or such operations, reply with ANSWER: explaining briefly that this button ",
            "cannot do that and that Ask for Code can; otherwise rewrite using only counting, ",
            "grouping, which.max() style lookups and joins.")
        } else {
          to_run <- replace_known_literals(code_cur, setNames(names(tok_all), unname(tok_all)))
          res <- withProgress(message = "Working on your answer...", value = 0.6,
                              run_answer_code(to_run, objects = prep$ps$objs))
          created  <- setdiff(ls(res$env, all.names = TRUE), prep$masked)
          vocab    <- tryCatch(build_redaction_vocab(mget(created, envir = res$env)),
                               error = function(e) NULL)
          scrubbed <- scrub_results_for_answer(res$output, res$warnings)
          red      <- redact_values(scrubbed, vocab, tok_all)
          tok_all  <- red$map
          hidden   <- grepl("values not shown", scrubbed, fixed = TRUE)
          usable   <- isTRUE(res$success) && !hidden && nzchar(trimws(scrubbed))
          if (usable) break
          problem <- if (!isTRUE(res$success)) {
            paste0("The code failed with this error:\n", redact_values(res$message, vocab, tok_all)$text)
          } else if (hidden) {
            paste("Your output was hidden because it looked like individual records (printed",
                  "data-frame rows). Re-run, printing only aggregated results with cat(), a",
                  "named vector, table() or summary().")
          } else "The code printed no results. Re-run, printing the results you need."
        }
        if (attempt == 2) break
        r_fix <- withProgress(message = "Trying again...", value = 0.6, call_ai(
          c(windowed, list(list(role = "assistant", content = raw_cur),
                           list(role = "user", content = paste0(problem,
            "\nReply with COMPUTE: and one corrected ```r block following the same rules,",
            " or ANSWER: if no calculation is needed.")))),
          plan_prompt, 2000))
        if (!nzchar(trimws(r_fix$text))) break
        record_usage(r_fix$cost_usd %||% 0)
        plan2 <- parse_answer_plan(r_fix$text)
        if (identical(plan2$type, "answer")) return(finalize(plan2$text))
        bad2 <- answer_code_violations(plan2$code)
        if (length(bad2) > 0) return(fail(unsafe_msg(bad2)))
        code_cur <- plan2$code
        raw_cur  <- r_fix$text
      }
      if (!usable)
        return(fail(paste("Classmate could not work this out automatically. Try rephrasing the",
                          "question, or use Ask for Code to work through it step by step.")))

      results_msg <- paste0(
        "Here are the results of running that code (summary output only):\n```\n",
        red$text, "\n```\nNow write the answer to my original question in plain English. ",
        "Do not include code.")
      r2 <- withProgress(message = "Writing the answer...", value = 0.8, call_ai(
        c(windowed, list(list(role = "assistant", content = raw_cur),
                         list(role = "user", content = results_msg))),
        build_answer_write_system_prompt(prefs$language), 1600))
      if (!nzchar(trimws(r2$text)))
        return(fail("No response from Claude when writing the answer. Please try again."))
      record_usage(r2$cost_usd %||% 0)

      new_pkgs <- vapply(extract_pkg_info(code_cur), `[[`, character(1), "pkg")
      loaded_packages(unique(c(loaded_packages(), new_pkgs)))
      answer_tokens <- trimws(sub("^\\s*ANSWER:\\s*", "", r2$text))
      # Keep the codes that actually appeared (in results or answer) so later turns can restore them
      seen_text <- paste(red$text, answer_tokens)
      seen      <- intersect(unique(unlist(regmatches(seen_text, gregexpr("\\[\\[[A-Za-z0-9_]+\\]\\]", seen_text)))),
                             names(tok_all))
      placeholder_map(c(placeholder_map()[setdiff(names(placeholder_map()), seen)], tok_all[seen]))
      # What the student sees under "Show my working" has the real values put back
      shown <- strsplit(paste(res$output, collapse = "\n"), "\n", fixed = TRUE)[[1]]
      if (length(shown) > 200)
        shown <- c(shown[1:200], sprintf("[... %d further lines not shown]", length(shown) - 200))
      shown_txt <- restore_placeholders(paste(shown, collapse = "\n"), tok_all)
      finalize(answer_tokens, code = code_cur, results_text = shown_txt)
    })
  }

  observeEvent(input$ask_plain, {
    req(input$prompt)
    if (!isTRUE(getOption(".classmate_answer_notice_shown"))) {
      showModal(modalDialog(
        title = "About the Answer button",
        tags$p("The Answer button gives you a plain-English answer instead of code."),
        tags$p("To answer some questions, Classmate runs a short calculation on your data",
               "and sends the summary results (for example counts or averages, never",
               "individual records) to Claude, which writes the answer. Please do not use it",
               "with data that must not be summarised outside your computer."),
        tags$p(em("You will only see this message once in this R session.")),
        footer = tagList(
          actionButton("answer_notice_ok", "Continue", class = "btn-primary"),
          modalButton("Cancel")
        ),
        easyClose = FALSE
      ))
    } else {
      do_ask_answer(input$prompt)
    }
  })

  observeEvent(input$answer_notice_ok, {
    removeModal()
    options(.classmate_answer_notice_shown = TRUE)
    req(input$prompt)
    do_ask_answer(input$prompt)
  })

  # --- Code / Answer switch -----------------------------------------------------------
  # Appears (at the end of the tab bar) once there is both code-side work and an answer. The
  # slider follows the current mode, so it moves by itself when Ask for Code or Answer is used,
  # and clicking it switches between the code tabs and the Answer tabs.
  has_code_state <- reactive({
    nzchar(trimws(input$code_editor %||% "")) || length(run_history()) > 0 ||
      length(console_output_rv()) > 0 || length(plot_files_rv()) > 0 ||
      isTRUE(changes_tab_shown())
  })
  has_answer_state <- reactive({
    !is.null(answer_latest()) || length(answer_hist()) > 0
  })
  observe({
    shinyjs::toggle("mode_switch_li",
      condition = (isTRUE(has_code_state()) && isTRUE(has_answer_state())) || isTRUE(help_mode()))
  })
  observe({
    shinyjs::runjs(sprintf("if (window.cmSetModeSwitch) cmSetModeSwitch(%s);",
                           if (identical(ask_mode(), "answer")) "true" else "false"))
  })
  observeEvent(input$mode_switch_clicked, {
    # Show Past prompts with the prompt behind what is now on view highlighted (none for a loaded script)
    if (identical(input$mode_switch_clicked, "answer")) {
      set_ask_mode("answer")
      updateTabsetPanel(session, "main_tabs", selected = "Answer")
      hl <- answer_latest()$question %||% ""
    } else {
      set_ask_mode("code")
      updateTabsetPanel(session, "main_tabs", selected = "Code")
      hl <- last_prompt_for_code() %||% ""
    }
    updateTabsetPanel(session, "prompt_tabs", selected = "Past prompts")
    session$sendCustomMessage("cm_highlight_prompt", list(text = hl))
  })

  # --- Answer tab ------------------------------------------------------------------
  answer_card <- function(entry) {
    div(
      tags$p(style = "color: #888; font-size: 0.85em; margin: 4px 0 8px 0;",
             tags$em(paste0("You asked: ", entry$question))),
      HTML(answer_to_html(entry$answer)),
      if (nzchar(entry$code %||% ""))
        tags$details(
          style = "margin-top: 10px;",
          tags$summary(style = "cursor: pointer; color: #2c5f8a;", "Show my working"),
          tags$p(em("The code Classmate ran to work this out:"), style = "margin: 8px 0 4px 0;"),
          tags$pre(entry$code),
          if (nzchar(entry$results %||% ""))
            tagList(tags$p(em("What it produced:"), style = "margin: 8px 0 4px 0;"),
                    tags$pre(entry$results))
        )
    )
  }
  output$answer_ui <- renderUI({
    a <- answer_latest()
    if (is.null(a))
      return(tags$p(em("Your answer will appear here."), style = "color: #999; margin: 4px 0;"))
    answer_card(a)
  })
  output$past_answers_ui <- renderUI({
    h <- answer_hist()
    if (length(h) == 0)
      return(tags$p(em("No earlier answers yet."), style = "color: #999; margin: 4px 0;"))
    tagList(lapply(h, function(e)
      div(style = "display: flex; align-items: flex-start; border-bottom: 1px solid #eee; padding: 2px 0;",
        tags$p(e$question %||% "", style = paste0(
          "margin: 0; flex: 1; font-size: 0.85em; color: #444;",
          "white-space: pre-wrap; word-break: break-word;")),
        tags$button("Recall", id = paste0("recall_answer_", e$seq %||% 0L),
          style = paste0(
            "flex-shrink: 0; margin-left: 6px; padding: 1px 6px;",
            "font-size: 0.75em; background: white;",
            "border: 1px solid #bbb; border-radius: 3px;",
            "cursor: pointer; color: #555;"),
          onclick = paste0("Shiny.setInputValue('recall_answer_seq', ", as.integer(e$seq),
                           ", {priority: 'event'});")))))
  })

  # Recall on the Past answers tab: that answer becomes the current one (the one it replaces
  # moves into the list) and the Answer tab is shown.
  observeEvent(input$recall_answer_seq, {
    h  <- answer_hist()
    i  <- which(vapply(h, function(e) identical(as.integer(e$seq %||% 0L), as.integer(input$recall_answer_seq)), logical(1)))
    if (length(i) == 0) return()
    ent    <- h[[i[1]]]
    hist   <- h[-i[1]]
    latest <- answer_latest()
    if (!is.null(latest)) hist <- c(list(latest), hist)
    answer_hist(hist)
    answer_latest(ent)
    updateTabsetPanel(session, "main_tabs", selected = "Answer")
  })

  observeEvent(input$oos_ok_plain, {
    removeModal()
    updateTextAreaInput(session, "prompt", value = "")
  })

  observeEvent(input$clarify_modify_prompt, {
    removeModal()
    updateTextAreaInput(session, "prompt", value = clarify_restore_prompt())
    shinyjs::runjs('setTimeout(function(){ document.getElementById("prompt").focus(); }, 100);')
  })

  # --- Explain button --------------------------------------------------------
  # explain_level_changed tracks whether the dropdown has been changed since
  # the last time an explanation was generated, so Regenerate can be locked
  # until the user actually picks a different level.
  explain_level_at_last_gen <- reactiveVal(NULL)
  explain_result             <- reactiveVal(NULL)   # list(explanation, help_pages)
  last_explain_level         <- reactiveVal("Intermediate")  # persists across modal opens
  tell_more_result           <- reactiveVal(NULL)

  output$tell_more_content <- renderUI({
    r <- tell_more_result()
    if (is.null(r))
      return(tags$p("", style = "margin: 0;"))
    if (isTRUE(r$loading))
      return(tags$p(em("Thinking…"), style = "color: #888; margin: 0;"))
    div(style = "font-size: 0.9em; line-height: 1.55;",
        HTML(explanation_to_html(r$text)))
  })

  # Enable Tell me more only when explanation is loaded and text is selected in it
  observe({
    sel     <- trimws(input$explain_selection %||% "")
    loaded  <- !is.null(explain_result()) && !isTRUE(explain_result()$loading)
    if (nzchar(sel) && loaded) enable("tell_more") else disable("tell_more")
  })

  observeEvent(input$tell_more, {
    sel_text <- trimws(input$explain_selection %||% "")
    req(nzchar(sel_text))
    level    <- last_explain_level()
    full_exp <- explain_result()$explanation %||% ""
    tell_more_result(list(loading = TRUE))
    shinyjs::runjs(paste0(
      "document.getElementById('tell_more_content').innerHTML=",
      "'<p style=\"color:#888;font-style:italic;\">Thinking…</p>';",
      "document.getElementById('tell-more-overlay').style.display='flex';"
    ))
    shinyjs::delay(100, {
      api_result <- tryCatch(
        call_claude(
          messages = list(list(role = "user", content = paste0(
            "A student is reading this R explanation:\n\n", full_exp, "\n\n",
            "They have highlighted this passage:\n\n“", sel_text, "”\n\n",
            "Unpack that passage a little further — in approximately 200 words, at ",
            level, " level. Stay tightly focused on what was highlighted: clarify ",
            "the meaning, add a layer of detail, or give a concrete example that ",
            "illustrates the point. Do not introduce new topics or go beyond what ",
            "the highlighted text is actually saying. Do not repeat what the ",
            "explanation already covers."
          ))),
          model         = prefs$model,
          max_tokens    = 500,
          system_prompt = paste(
            "You are a helpful R programming tutor unpacking one specific passage",
            "from an explanation the student has already read. Your only job is to",
            "go a little deeper into that passage — clarify it, add one layer of",
            "detail, or give a brief concrete example. Do not introduce new topics,",
            "do not speculate beyond what the passage says, and do not repeat what",
            "was already explained. Target approximately 200 words.",
            "Do not begin with a heading or title of any kind — start directly",
            "with the elaboration. Use **bold** for emphasis where helpful but",
            "do not use ## or ### headers.",
            "",
            build_language_clause(prefs$language)
          )
        ),
        error = function(e) list(text = "", cost_usd = 0)
      )
      if (nzchar(trimws(api_result$text %||% ""))) {
        record_usage(api_result$cost_usd)
        tell_more_result(list(text = api_result$text))
      } else {
        tell_more_result(list(text = "Could not generate additional detail. Please try again."))
      }
    })
  })

  # Clear tell_more state when explanation modal closes
  observeEvent(input$explain_close, {
    tell_more_result(NULL)
    shinyjs::runjs("document.getElementById('tell-more-overlay').style.display='none';")
  })

  observeEvent(input$explain_code, {

    # --- Error-explanation mode: run failed, explain the output to the student ---
    if (last_run_failed()) {
      error_text <- paste(console_output_rv(), collapse = "\n")
      code_text  <- isolate(input$code_editor) %||% ""
      explain_result(list(loading = TRUE))
      showModal(modalDialog(
        title     = "What went wrong?",
        size      = "l",
        easyClose = FALSE,
        div(style = "min-height: 120px;", uiOutput("explain_text_ui")),
        footer = actionButton("explain_close", "Close",
          style = "background-color: white; border-color: #bbb; color: #333;")
      ))
      shinyjs::delay(150, {
        withProgress(message = "Interpreting the output...", value = 0.5, {
          api_result <- tryCatch(
            call_claude(
              messages = list(list(role = "user", content = paste0(
                "Here is the R code the student ran:\n\n```r\n", code_text, "\n```\n\n",
                "It produced the following output (errors and/or warnings):\n\n",
                error_text
              ))),
              model         = prefs$model,
              max_tokens    = 520,
              system_prompt = build_error_explain_system_prompt(prefs$language)
            ),
            error = function(e) list(text = "", stop_reason = "error", cost_usd = 0)
          )
        })
        if (!nzchar(trimws(api_result$text))) {
          explain_result(list(explanation = "Could not generate an explanation — please check your API key and try again."))
        } else {
          record_usage(api_result$cost_usd)
          raw <- api_result$text
          expl <- trimws(sub("(?s)^EXPLANATION:\\s*", "", raw, perl = TRUE))
          explain_result(list(explanation = expl))
        }
      })
      return(invisible(NULL))
    }

    # --- Output-explanation mode: R output tab is active with content -----------
    on_output_tab  <- isTRUE(input$main_tabs == "R output")
    has_run_output <- length(console_output_rv()) > 0 || length(plot_files_rv()) > 0 || length(warnings_rv()) > 0
    if (on_output_tab && has_run_output && !last_run_failed()) {
      level <- last_explain_level()
      explain_level_at_last_gen(level)
      disable("explain_regenerate")
      explain_result(list(loading = TRUE))
      showModal(modalDialog(
        title     = "Output explanation",
        size      = "l",
        easyClose = FALSE,
        div(style = "min-height: 180px;", uiOutput("explain_text_ui")),
        div(style = "margin-top: 14px;",
          disabled(actionButton("tell_more", "Tell me more",
            style = "background-color: white; border-color: #bbb; color: #333;"))
        ),
        hr(style = "margin-top: 18px;"),
        fluidRow(
          column(5,
            selectInput("explain_level", "Level of explanation:",
              choices  = c("Beginner", "Intermediate", "Advanced"),
              selected = level, width = "100%")
          ),
          column(3, style = "padding-top: 25px;",
            disabled(actionButton("explain_regenerate", "Regenerate",
              style = "background-color: white; border-color: #bbb; color: #333;"))
          )
        ),
        footer = actionButton("explain_close", "Close",
          style = "background-color: white; border-color: #bbb; color: #333;")
      ))
      updateSelectInput(session, "explain_level", selected = level)
      text_out   <- paste(console_output_rv(), collapse = "\n")
      plot_files <- plot_files_rv()
      code_ctx   <- isolate(input$code_editor %||% "")
      prompt_ctx <- isolate(last_prompt_for_code() %||% "")
      shinyjs::delay(150, {
        withProgress(message = "Interpreting output...", value = 0.5, {
          api_result <- tryCatch(
            call_claude(
              messages      = list(list(role = "user",
                content = build_output_explain_content(
                  text_out, plot_files, level,
                  code   = code_ctx,
                  prompt = prompt_ctx
                ))),
              model         = prefs$model,
              max_tokens    = 900,
              system_prompt = build_output_explain_system_prompt(prefs$language)
            ),
            error = function(e) list(text = "", stop_reason = "error", cost_usd = 0,
                                     error_msg = conditionMessage(e))
          )
        })
        if (!nzchar(trimws(api_result$text))) {
          detail <- if (nzchar(api_result$error_msg %||% ""))
            paste0(" (", api_result$error_msg, ")") else ""
          explain_result(list(explanation = paste0(
            "Could not generate an explanation", detail, ".")))
        } else {
          last_explain_level(level)
          record_usage(api_result$cost_usd)
          explain_result(parse_explain_response(api_result$text))
        }
      })
      return(invisible(NULL))
    }

    # --- Changes mode: explain what was changed and why ------------------------
    on_changes_tab <- isTRUE(input$main_tabs == "Changes")
    d <- diff_rv()
    if (on_changes_tab && !is.null(d)) {
      annotate_lines <- function(lines, changed_idx) {
        vapply(seq_along(lines), function(i) {
          mark <- if (i %in% changed_idx) "  ## << CHANGED" else ""
          paste0(lines[[i]], mark)
        }, character(1))
      }
      old_annotated <- paste(annotate_lines(d$old_lines, d$old_changed), collapse = "\n")
      new_annotated <- paste(annotate_lines(d$new_lines, d$changed),     collapse = "\n")
      original_request <- trimws(last_prompt_for_code() %||% "")
      user_msg <- paste0(
        if (nzchar(original_request))
          paste0("The student asked: \"", original_request, "\"\n\n") else "",
        "Here is the PREVIOUS version of the code",
        " (lines marked ## << CHANGED were altered or removed):\n\n",
        "```r\n", old_annotated, "\n```\n\n",
        "Here is the NEW version of the code",
        " (lines marked ## << CHANGED were added or modified):\n\n",
        "```r\n", new_annotated, "\n```\n\n",
        "Please explain what was changed and why."
      )
      explain_result(list(loading = TRUE))
      showModal(modalDialog(
        title     = "What changed?",
        size      = "l",
        easyClose = FALSE,
        div(style = "min-height: 120px;", uiOutput("explain_text_ui")),
        footer = actionButton("explain_close", "Close",
          style = "background-color: white; border-color: #bbb; color: #333;")
      ))
      shinyjs::delay(150, {
        withProgress(message = "Explaining the changes...", value = 0.5, {
          api_result <- tryCatch(
            call_claude(
              messages      = list(list(role = "user", content = user_msg)),
              model         = prefs$model,
              max_tokens    = 800,
              system_prompt = build_diff_explain_system_prompt(prefs$language)
            ),
            error = function(e) list(text = "", stop_reason = "error", cost_usd = 0)
          )
        })
        if (!nzchar(trimws(api_result$text))) {
          explain_result(list(explanation = "Could not generate an explanation — please check your API key and try again."))
        } else {
          record_usage(api_result$cost_usd)
          explain_result(parse_explain_response(api_result$text))
        }
      })
      return(invisible(NULL))
    }

    # --- Normal mode: explain the code in the editor ---------------------------
    req(nzchar(trimws(input$code_editor %||% "")))

    sel       <- trimws(input$code_selection %||% "")
    code_text <- if (nzchar(sel)) sel else isolate(input$code_editor)
    modal_title <- if (nzchar(sel)) "Code explanation (selection)" else "Code explanation"

    level <- last_explain_level()

    # Set loading state BEFORE showModal so the uiOutput renders the
    # "Generating explanation..." message as soon as the modal opens.
    explain_level_at_last_gen(level)
    disable("explain_regenerate")
    explain_result(list(loading = TRUE))

    showModal(modalDialog(
      title = modal_title,
      size  = "l",
      easyClose = FALSE,

      div(style = "min-height: 180px;",
        uiOutput("explain_text_ui")
      ),

      div(style = "margin-top: 14px;",
        disabled(actionButton("tell_more", "Tell me more",
          style = "background-color: white; border-color: #bbb; color: #333;"))
      ),

      hr(style = "margin-top: 18px;"),

      fluidRow(
        column(5,
          selectInput("explain_level", "Level of explanation:",
            choices  = c("Beginner", "Intermediate", "Advanced"),
            selected = level,
            width    = "100%"
          )
        ),
        column(3, style = "padding-top: 25px;",
          disabled(actionButton("explain_regenerate", "Regenerate",
            style = "background-color: white; border-color: #bbb; color: #333;"))
        )
      ),

      footer = tagList(
        actionButton("explain_close", "Close",
          style = "background-color: white; border-color: #bbb; color: #333;")
      )
    ))

    updateSelectInput(session, "explain_level", selected = level)

    shinyjs::delay(150, {
      withProgress(message = "Generating explanation...", value = 0.5, {
        api_result <- tryCatch(
          call_claude(
            messages      = list(list(role = "user",
                                      content = build_explain_prompt(code_text, level))),
            model         = prefs$model,
            max_tokens    = 1600,
            system_prompt = build_explain_system_prompt(prefs$language)
          ),
          error = function(e) list(text = "", stop_reason = "error")
        )
      })

      if (!nzchar(trimws(api_result$text))) {
        explain_result(list(
          explanation = "Could not generate an explanation — please check your API key and try again."
        ))
      } else if (identical(api_result$stop_reason, "max_tokens")) {
        explain_result(list(
          explanation = "The code is too long to explain in full. Try selecting just the part you want explained and clicking Explain again."
        ))
      } else {
        last_explain_level(level)
        record_usage(api_result$cost_usd)
        explain_result(parse_explain_response(api_result$text))
      }
    })
  })

  output$explain_text_ui <- renderUI({
    result <- explain_result()
    if (is.null(result) || isTRUE(result$loading))
      return(div(style = "color: #888; padding: 20px;", em("Generating explanation...")))
    div(
      style = "max-height: 380px; overflow-y: auto; padding: 4px 2px; line-height: 1.55;",
      HTML(explanation_to_html(result$explanation))
    )
  })
  outputOptions(output, "explain_text_ui", suspendWhenHidden = FALSE)

  # Lock / unlock Regenerate when the level dropdown changes.
  observeEvent(input$explain_level, {
    if (!is.null(explain_level_at_last_gen()) &&
        input$explain_level != explain_level_at_last_gen()) {
      enable("explain_regenerate")
    } else {
      disable("explain_regenerate")
    }
  })

  observeEvent(input$explain_regenerate, {
    req(!is.null(explain_result()))
    new_level <- input$explain_level
    explain_level_at_last_gen(new_level)
    disable("explain_regenerate")
    explain_result(list(loading = TRUE))   # triggers "Generating explanation..." render

    sel       <- trimws(input$code_selection %||% "")
    code_text <- if (nzchar(sel)) sel else isolate(input$code_editor)

    shinyjs::delay(150, {
      withProgress(message = "Regenerating explanation...", value = 0.5, {
        api_result <- tryCatch(
          call_claude(
            messages      = list(list(role = "user",
                                      content = build_explain_prompt(code_text, new_level))),
            model         = prefs$model,
            max_tokens    = 1600,
            system_prompt = build_explain_system_prompt(prefs$language)
          ),
          error = function(e) list(text = "", stop_reason = "error")
        )
      })

      if (!nzchar(trimws(api_result$text))) {
        explain_result(list(
          explanation = "Could not generate an explanation — please check your API key and try again."
        ))
      } else if (identical(api_result$stop_reason, "max_tokens")) {
        explain_result(list(
          explanation = "The code is too long to explain in full. Try selecting just the part you want explained and clicking Explain again."
        ))
      } else {
        last_explain_level(new_level)
        record_usage(api_result$cost_usd)
        explain_result(parse_explain_response(api_result$text))
      }
    })
  })

  observeEvent(input$explain_close, {
    explain_result(NULL)
    explain_level_at_last_gen(NULL)
    removeModal()
  })

  # --- Run the code ----------------------------------------------------------
  do_run_code <- function(code_text) {
    pending_export(NULL)
    ui_busy(TRUE)
    console_output_rv(character(0)); warnings_rv(character(0)); plot_files_rv(character(0))

    check_and_install_if_needed(code_text, function() {
      code_running(TRUE)
      tryCatch({
        run_result <- withProgress(message = "Running code...", value = 0.5, run_code(code_text))
        last_run_code(code_text)
        handle_run_result(run_result)
        if (run_result$success) {
          last_run_failed(FALSE)
          last_error_msg("")
          unconf_remove_code(code_text)    # now confirmed (listed under its confirmed entry)
          new_pkgs <- vapply(extract_pkg_info(code_text), `[[`, character(1), "pkg")
          loaded_packages(unique(c(loaded_packages(), new_pkgs)))
          if (!identical(trimws(code_text), trimws(last_logged_code()))) {
            script_name <- loaded_script_name()
            desc <- if (nzchar(trimws(last_code_description() %||% ""))) {
              last_code_description()
            } else if (!is.null(script_name)) {
              script_name
            } else {
              ""
            }
            entry <- list(prompt = last_prompt_for_code(), code = code_text,
                          description = desc, source = "ask_code", ts = Sys.time())
            run_history(c(run_history(), list(entry)))
            pending_log_entries(c(pending_log_entries(), list(entry)))
            all_log_entries(c(all_log_entries(), list(entry)))
            last_logged_code(code_text)
            loaded_script_name(NULL)
          }
        } else {
          last_run_failed(TRUE)
          last_error_msg(sub("^Error running code:\\s*\\n?\\s*", "", run_result$message))
        }
      }, finally = { code_running(FALSE); ui_busy(FALSE) })
    })
  }
  # Running a block of "save this plot" code appended by the expanded-plot window runs only that block:
  # everything before it has already been run and need not be repeated.
  do_run_export <- function(block, full, file) {
    ui_busy(TRUE); code_running(TRUE)
    on.exit({ code_running(FALSE); ui_busy(FALSE) }, add = TRUE)
    res <- withProgress(message = "Saving the plot...", value = 0.5, run_code(block))
    run_status_is_run(TRUE)
    if (isTRUE(res$success)) {
      pending_export(NULL)
      last_run_code(full); last_run_failed(FALSE); last_error_msg("")
      entry <- list(prompt = "Save the plot to a file", code = block, description = "", source = "export")
      pending_log_entries(c(pending_log_entries(), list(entry)))
      all_log_entries(c(all_log_entries(), list(entry)))
      last_logged_code(full)
      output$run_status <- renderUI(tags$span(
        paste0("Saved ", file, " in ", project_root()),
        style = "color: #2e7d32; font-weight: bold;"))
    } else {
      err <- sub("^Error running code:\\s*\\n?\\s*", "", res$message %||% "")
      output$run_status <- renderUI(tags$span(paste("Could not save the plot:", err),
                                              style = "color: #b22222; font-weight: bold;"))
    }
    invisible(NULL)
  }
  observeEvent(input$run_code, {
    pe <- pending_export()
    if (!is.null(pe) && identical(trimws(input$code_editor %||% ""), trimws(pe$full)))
      do_run_export(pe$snippet, pe$full, pe$file)
    else {
      pending_export(NULL)
      do_run_code(input$code_editor)
    }
  })

  # --- Fix button ------------------------------------------------------------
  code_before_fix <- reactiveVal(NULL)

  do_fix <- function() {
    if (isTRUE(quota_exceeded())) { show_quota_modal(); return(invisible(NULL)) }

    current_code  <- input$code_editor
    current_error <- last_error_msg()

    # Save original code on first fix attempt in this error sequence
    if (is.null(code_before_fix())) code_before_fix(current_code)

    # Build context from recent run_history steps (last 2, most recent first)
    history_context <- local({
      hist <- run_history()
      if (length(hist) == 0) return("")
      recent <- rev(take_last_n(hist, 2))
      blocks <- vapply(recent, function(e) {
        p <- trimws(e$prompt %||% "")
        c <- trimws(e$code   %||% "")
        entry <- character(0)
        if (nzchar(p)) entry <- c(entry, paste0("Prompt: ", p))
        if (nzchar(c)) entry <- c(entry, paste0("```r\n", c, "\n```"))
        paste(entry, collapse = "\n")
      }, character(1))
      blocks <- blocks[nzchar(blocks)]
      if (length(blocks) == 0) return("")
      paste0(
        "For context, here are the most recent steps the student has already completed:\n\n",
        paste(blocks, collapse = "\n\n"),
        "\n\n"
      )
    })

    withProgress(message = "Diagnosing and fixing...", value = 0.5, {
      api_result <- tryCatch(
        call_claude(
          messages = list(list(role = "user", content = paste0(
            history_context,
            "Here is the current code that is failing:\n\n```r\n", current_code, "\n```\n\n",
            "It produced this error:\n", current_error
          ))),
          model         = prefs$model,
          max_tokens    = 1100,
          system_prompt = build_fix_system_prompt(prefs$language),
          cache_system  = FALSE
        ),
        error = function(e) list(text = "", stop_reason = "error", cost_usd = 0)
      )
    })

    if (!nzchar(trimws(api_result$text))) {
      showModal(modalDialog(
        title  = "Fix failed",
        "No response from Claude. Please check your API key and try again.",
        footer = modalButton("OK")
      ))
      return(invisible(NULL))
    }

    record_usage(api_result$cost_usd)

    # Parse EXPLANATION and code block from response
    raw   <- api_result$text
    expl  <- trimws(sub("(?s)EXPLANATION:\\s*(.*?)\\s*```.*", "\\1", raw, perl = TRUE))
    code_match <- regmatches(raw, regexpr("(?s)```(?:r)?\\s*\\n(.+?)\\s*```", raw, perl = TRUE))
    fixed_code <- if (length(code_match) > 0)
      trimws(sub("(?s)```(?:r)?\\s*\\n(.+?)\\s*```", "\\1", code_match, perl = TRUE))
    else NULL

    if (is.null(fixed_code) || !nzchar(fixed_code)) {
      showModal(modalDialog(
        title  = "Fix failed",
        "Claude could not produce corrected code. Try running Fix again, or return to editing.",
        footer = tagList(
          actionButton("fix_retry",  "Continue fixing", class = "btn-primary"),
          actionButton("fix_cancel", "Cancel", class = "btn-danger")
        )
      ))
      return(invisible(NULL))
    }

    # Apply fixed code to editor, compute diff, then check packages and re-run
    original_code <- code_before_fix() %||% ""
    updateAceEditor(session, "code_editor", value = fixed_code)
    local_d <- if (nzchar(trimws(original_code)))
      compute_diff(original_code, fixed_code) else NULL
    diff_rv(local_d)
    captured_expl       <- expl
    captured_fixed_code <- fixed_code

    check_and_install_if_needed(fixed_code, function() {
      run_result <- withProgress(message = "Re-running fixed code...", value = 0.5,
                                 run_code(captured_fixed_code))
      last_run_code(captured_fixed_code)
      handle_run_result(run_result, stay_on_code = TRUE)
      if (!is.null(local_d) && run_result$success) show_changes_tab()

      if (run_result$success) {
        last_run_failed(FALSE)
        code_before_fix(NULL)
        entry <- list(prompt = last_prompt_for_code(), code = captured_fixed_code,
                      description = last_code_description(), source = "ask_code", ts = Sys.time())
        unconf_remove_code(captured_fixed_code)
        run_history(c(run_history(), list(entry)))
        pending_log_entries(c(pending_log_entries(), list(entry)))
        all_log_entries(c(all_log_entries(), list(entry)))
        last_logged_code(captured_fixed_code)
        new_pkgs <- vapply(extract_pkg_info(captured_fixed_code), `[[`, character(1), "pkg")
        loaded_packages(unique(c(loaded_packages(), new_pkgs)))
        showModal(modalDialog(
          title  = "Success!",
          tags$p(if (nzchar(captured_expl)) captured_expl
                 else "The code has been fixed and ran successfully."),
          footer = modalButton("OK"),
          easyClose = FALSE
        ))
      } else {
        last_run_failed(TRUE)
        last_error_msg(sub("^Error running code:\\s*\\n?\\s*", "", run_result$message))
        showModal(modalDialog(
          title  = "Fix unsuccessful",
          tags$p("Sorry, the fix was unsuccessful. You can try another round of diagnosis,",
                 "or cancel to restore the original code."),
          footer = tagList(
            actionButton("fix_retry",  "Continue fixing", class = "btn-primary"),
            actionButton("fix_cancel", "Cancel",          class = "btn-danger")
          ),
          easyClose = FALSE
        ))
      }
    })
  }

  observeEvent(input$fix_code, do_fix())

  observeEvent(input$fix_retry, {
    removeModal()
    do_fix()
  })

  observeEvent(input$fix_cancel, {
    removeModal()
    original <- code_before_fix()
    if (!is.null(original)) {
      updateAceEditor(session, "code_editor", value =original)
      output$run_status <- renderUI(tags$span("Fix cancelled — original code restored.",
                                            style = "color: #888;"))
    }
    code_before_fix(NULL)
    last_run_failed(TRUE)
  })


  run_status_ui <- function(success) {
    if (isTRUE(success))
      tags$span("Success!", style = "color: #1a6e1a; font-weight: bold;")
    else if (identical(success, FALSE))
      tags$span("Failed to run", style = "color: #b22222; font-weight: bold;")
    else
      NULL
  }

  # Helper: after a run_result, update console output + plots, switch tabs
  # The "Success!" / error line under Run describes the code as it was when run: once the code in the
  # editor changes (typing, Add to code...) it is removed until the code has been run again.
  run_status_is_run <- reactiveVal(FALSE)
  observeEvent(input$code_editor, {
    if (isTRUE(run_status_is_run()) &&
        !identical(trimws(input$code_editor %||% ""), trimws(last_run_code() %||% ""))) {
      output$run_status <- renderUI(NULL)
      run_status_is_run(FALSE)
    }
  }, ignoreInit = TRUE)

  handle_run_result <- function(run_result, stay_on_code = FALSE) {
    run_status_is_run(TRUE)
    if (!is.null(run_result$wd_changed)) note_wd_change(run_result$wd_changed)
    base_output <- run_result$output %||% character(0)
    if (!isTRUE(run_result$success)) {
      err_line <- sub("^Error running code:\\s*\\n?\\s*", "", run_result$message %||% "")
      if (nzchar(trimws(err_line)))
        base_output <- c(paste0("Error: ", err_line), if (length(base_output) > 0) base_output)
    }
    plots <- run_result$plot_files %||% character(0)
    console_output_rv(base_output)
    plot_files_rv(plots)
    warnings_rv(run_result$warnings %||% character(0))
    output$run_status <- renderUI(run_status_ui(run_result$success))
    # A successful run (Run, AutoRun or Fix) finishes with the prompt, so the prompt box is cleared
    if (isTRUE(run_result$success)) updateTextAreaInput(session, "prompt", value = "")
    has_output <- nzchar(trimws(paste(base_output, collapse = "\n"))) || length(plots) > 0
    if (!stay_on_code && has_output) {
      goto_tab("R output")
    } else {
      goto_tab("Code")
    }
  }

  # --- View Code Log -----------------------------------------------------------
  observeEvent(input$view_log, {
    entries <- all_log_entries()
    body <- if (length(entries) == 0)
      tags$p(em("No code has been run successfully in this conversation yet."),
             style = "color: #888; margin: 4px 0;")
    else
      div(style = "height: 380px; overflow-y: auto; padding-right: 6px;",
        lapply(seq_along(entries), function(i) {
          e <- entries[[i]]
          div(style = "margin-bottom: 14px;",
            tags$p(style = "margin: 0 0 4px 0; font-size: 0.85em; color: #666;",
              tags$strong(paste0("Chunk ", i, ": ")),
              if (is.null(e$prompt) || !nzchar(trimws(e$prompt)))
                em("no prompt recorded (code entered or loaded manually)")
              else em(trimws(e$prompt))),
            tags$pre(style = "margin: 0; font-size: 0.82em; white-space: pre-wrap; background: #f8f8f8;",
                     trimws(e$code)))
        }))
    showModal(modalDialog(
      title = "Code Log",
      size  = "l",
      tags$p(style = "font-size: 0.85em; color: #888;",
        "The code run successfully in this conversation. Press Save to save it as an R Notebook",
        "and open it in RStudio."),
      body,
      footer = tagList(
        if (length(entries) > 0) actionButton("view_log_save", "Save", class = "btn-primary"),
        modalButton("Close")),
      easyClose = TRUE
    ))
  })
  observeEvent(input$view_log_save, {
    removeModal()
    do_save_log()
    showNotification("Code log saved and opened. You can carry on; saving again later includes everything so far.",
                     type = "message", duration = 8)
  })

  # --- Save Code Log ---------------------------------------------------------
  do_save_log <- function() {
    entries <- all_log_entries()
    if (length(entries) == 0) return(invisible(NULL))
    p <- make_log_path()
    log_path(p)
    cat(format_log_entries(entries), "\n\n", file = p, append = FALSE, sep = "")
    pending_log_entries(list())   # clear "unsaved" marker; all_log_entries kept for next save
    open_log_file(p)
  }

  observeEvent(input$save_block, {
    code_text <- isolate(input$code_editor %||% "")
    req(nzchar(trimws(code_text)))
    desc <- trimws(last_code_description() %||% "")
    # Derive filename from description (sanitise) or fall back to timestamp
    stem <- if (nzchar(desc))
      gsub("[^A-Za-z0-9_]", "_", gsub("\\s+", "_", desc))
    else
      paste0("classmate_block_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    stem <- sub("_+$", "", substr(stem, 1, 60))
    p <- file.path(project_root(), paste0(stem, ".R"))
    writeLines(code_text, p)
    open_log_file(p)
    output$run_status <- renderUI(tags$span(
      paste0("Code block saved to ", basename(p), "."),
      style = "color: #888;"
    ))
  })

  # --- Pause ----------------------------------------------------------
  show_pause_modal <- function()
    showModal(modalDialog(
      title = "Pause",
      tags$p(tags$strong("Pause Classmate: your session is saved and Classmate closes.")),
      tags$p(paste(
        "The current session — prompt, code, context files and objects,",
        "conversation history, and preferences — will be saved and restored",
        "automatically when you next start the app."
      )),
      tags$p(em(
        "The API key is not saved; it will be picked up from the R environment automatically."
      )),
      footer = tagList(
        actionButton("confirm_pause_app", "Pause", class = "btn-primary"),
        modalButton("Cancel")
      )
    ))
  observeEvent(input$pause_app, show_pause_modal())

  # Pause is also offered directly by the Quick Console's install warning and timeout pop-ups
  do_pause_app <- function() {
    removeModal()
    capture_active_code()
    pause_state <- list(
      prompt               = isolate(input$prompt),
      code_editor          = isolate(input$code_editor),
      selected_files       = selected_files(),
      selected_objects     = selected_objects(),
      prefs                = list(
        coding          = prefs$coding,
        plotting        = prefs$plotting,
        mapping         = prefs$mapping,
        model           = prefs$model,
        img_format      = prefs$img_format,
        img_quality     = prefs$img_quality,
        img_size        = prefs$img_size,
        max_lines       = prefs$max_lines,
        comment_density = prefs$comment_density
      ),
      conversation_history = conversation_history(),
      saved_conversations  = saved_conversations(),
      run_history          = run_history(),
      unconf_code          = unconf_code(),
      pending_log_entries  = pending_log_entries(),
      all_log_entries      = all_log_entries(),
      last_extracted_code  = last_extracted_code(),
      last_prompt_for_code = last_prompt_for_code(),
      last_logged_code     = last_logged_code(),
      prompt_history       = prompt_history(),
      conv_count_rv        = conv_count_rv(),
      answer_latest        = answer_latest(),
      answer_hist          = answer_hist(),
      interactions         = interactions(),
      active_code_id       = active_code_id(),
      placeholder_map      = placeholder_map(),
      ask_mode             = ask_mode()
    )
    if (length(all_log_entries()) > 0) do_save_log()
    tryCatch(
      saveRDS(pause_state, pause_file()),
      error = function(e)
        showNotification(paste("Could not save pause state:", conditionMessage(e)),
                         type = "error", duration = 8)
    )
    stopApp()
  }
  # One observer per button: combining the inputs in one observeEvent(c(...)) loses the action-button
  # class, so the 0 an action button reports when it is first drawn would count as a click
  observeEvent(input$confirm_pause_app, do_pause_app())
  observeEvent(input$qc_timeout_pause,  do_pause_app())
  observeEvent(input$qc_install_pause,  do_pause_app())

  # --- Quit ------------------------------------------------------------------
  do_quit_with_autosave <- function() {
    if (length(all_log_entries()) > 0) do_save_log()
    do_quit()
  }

  show_quit_confirm_modal <- function() {
    save_note <- if (length(all_log_entries()) > 0)
      "Your code notebook will be saved automatically and opened in RStudio."
    else
      "There is nothing to save."
    showModal(modalDialog(
      title = "Quit with Code Log?",
      tags$p("Are you sure you want to quit? Your R workspace will not be cleared."),
      tags$p(save_note),
      footer = tagList(
        actionButton("confirm_quit", "Yes, quit", class = "btn-danger"),
        modalButton("Cancel")
      )
    ))
  }

  observeEvent(input$quit,         { show_quit_confirm_modal() })
  observeEvent(input$confirm_quit, { removeModal(); do_quit_with_autosave() })

  # --- Auto-save on unexpected close (browser tab closed, Escape in console) --
  session$onSessionEnded(function() {
    # pending_log_entries is cleared by do_save_log; non-empty means unsaved runs exist
    entries <- isolate(all_log_entries())
    pending <- isolate(pending_log_entries())
    if (length(entries) > 0 && length(pending) > 0) {
      tryCatch({
        p <- make_log_path()
        cat(format_log_entries(entries), "\n\n", file = p, append = FALSE, sep = "")
        open_log_file(p)
      }, error = function(e) NULL)
    }
  })
}

shinyApp(ui, server)
