library(shiny)
library(shinyjs)
library(shinyFiles)
library(shinyAce)
library(httr2)

# --- Project root and output paths -------------------------------------------
# talk() stores the user's working directory in this option before calling
# runApp(), which would otherwise change the wd to this inst/app/ folder.
# Fall back to getwd() when the app is run directly for development.
PROJECT_ROOT <- getOption(".classmate_project_root", getwd())
OUTPUTS_DIR  <- file.path(PROJECT_ROOT, "outputs")
PAUSE_FILE   <- file.path(PROJECT_ROOT, "claude_assistant_pause.rds")
dir.create(OUTPUTS_DIR, showWarnings = FALSE, recursive = TRUE)

# Serve temp plot files captured during code runs
addResourcePath("classmate_plots", tempdir())

# --- Constants ---------------------------------------------------------------
MAX_HISTORY_TURNS     <- 8
KEY_CHECK_INTERVAL_MS <- 10 * 60 * 1000

`%||%` <- function(x, y) if (is.null(x)) y else x

# Pinned to Sonnet.  Haiku hallucinated a non-existent tmap function in
# side-by-side testing so it is not offered here.
MODEL_SONNET <- "claude-sonnet-4-6"
MODEL_HAIKU  <- "claude-haiku-4-5-20251001"

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

filename_timestamp_instruction <- paste0(
  "HARD RULE — do NOT add any file-saving or export code unless the user's prompt ",
  "explicitly asks to save or export something (e.g. 'save the map', 'export to CSV', ",
  "'write the results to a file'). If the prompt only asks to create a plot, map, or ",
  "object, just create it — do not append saving code automatically. ",

  "When saving IS requested: always save into this exact folder, which already exists: ",
  OUTPUTS_DIR, ". ",
  "Use file.path(\"", OUTPUTS_DIR, "\", filename) as the save path. ",
  "Do NOT call dir.create() — the folder is guaranteed to exist. ",
  "If the user has NOT specified a filename, default to a timestamp-based name: ",
  "paste0(format(Sys.time(), \"%Y-%m-%d-%H-%M-%S\"), \".ext\") ",
  "replacing .ext with the appropriate extension. ",

  "HARD RULE for saving image output (png, pdf, tiff, jpeg, bmp) when saving IS requested: ",
  "NEVER use tmap_save(), ggsave(), or any package-level save function for images. ",
  "Always use the explicit device pattern: open the device, print/plot the object, ",
  "close the device. For example: ",
  "  png(file.path(\"", OUTPUTS_DIR, "\", fname), width=W, height=H, units=\"mm\", res=DPI) ",
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
    filename_timestamp_instruction,
    format_preferences_clause(coding, plotting, mapping, max_lines, comment_density),
    format_image_preferences_clause(img_format, img_quality, img_size),
    build_language_clause(language),
    code_description_rule,
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
           length(schema$cols %||% character(0)), " columns]")
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
    "> **Note:** Please Pause or Quit Classmate before running code in this notebook.",
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

validate_api_key <- function(key) {
  old_key <- Sys.getenv("ANTHROPIC_API_KEY")
  Sys.setenv(ANTHROPIC_API_KEY = key)
  ok <- tryCatch({
    resp <- call_claude(
      messages   = list(list(role = "user", content = "Hi")),
      model      = MODEL_SONNET,
      max_tokens = 1,
      api_key    = key,
      cache_system = FALSE
    )
    nzchar(trimws(resp$text))
  }, error = function(e) FALSE)
  if (!ok) Sys.setenv(ANTHROPIC_API_KEY = old_key)
  ok
}

build_volumes <- function() shinyFiles::getVolumes()()

compute_default_root_and_path <- function(volumes, wd = getwd()) {
  wd_norm  <- normalizePath(wd, winslash = "/", mustWork = FALSE)
  best     <- NULL
  best_len <- -1
  for (nm in names(volumes)) {
    vol_norm <- normalizePath(volumes[[nm]], winslash = "/", mustWork = FALSE)
    if (startsWith(wd_norm, vol_norm) && nchar(vol_norm) > best_len) {
      best     <- nm
      best_len <- nchar(vol_norm)
    }
  }
  if (is.null(best)) return(list(root = names(volumes)[1], path = ""))
  vol_norm <- normalizePath(volumes[[best]], winslash = "/", mustWork = FALSE)
  rel <- sub(paste0("^", vol_norm), "", wd_norm)
  rel <- sub("^/+", "", rel)
  list(root = best, path = rel)
}

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
# panels. Returns NULL when change is too substantial (> 25% or > 10 changed lines).
compute_diff <- function(old_code, new_code) {
  old_lines <- strsplit(old_code, "\n", fixed = TRUE)[[1]]
  new_lines <- strsplit(new_code, "\n", fixed = TRUE)[[1]]
  strip_trailing_op <- function(x) trimws(sub("[+,|&]\\s*$", "", trimws(x)))
  old_norm  <- strip_trailing_op(old_lines)
  new_norm  <- strip_trailing_op(new_lines)
  n_old <- length(old_lines)
  n_new <- length(new_lines)

  # Greedy forward scan: match each non-blank new line to the first available old line
  new_to_old <- rep(NA_integer_, n_new)
  old_ptr <- 1L
  for (i in seq_len(n_new)) {
    if (!nzchar(new_norm[i])) next
    for (j in seq(old_ptr, n_old)) {
      if (nzchar(old_norm[j]) && identical(new_norm[i], old_norm[j])) {
        new_to_old[i] <- j
        old_ptr <- j + 1L
        break
      }
    }
  }

  # Changed new lines = non-blank lines with no match in old
  changed_new <- which(is.na(new_to_old) & nzchar(new_norm))
  n_changed   <- length(changed_new)
  if (n_changed == 0) return(NULL)
  if (n_changed / max(n_new, 1L) > 0.25) return(NULL)
  if (n_changed > 10) return(NULL)

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
    for (k in seq(ci - 1L, 1L)) {
      if (!is.na(new_to_old[k])) { prev_old <- new_to_old[k]; break }
    }
    next_old <- NA_integer_
    if (ci < n_new) for (k in seq(ci + 1L, n_new)) {
      if (!is.na(new_to_old[k])) { next_old <- new_to_old[k]; break }
    }
    gap_start <- if (is.na(prev_old)) old_start else prev_old + 1L
    gap_end   <- if (is.na(next_old)) old_end   else next_old - 1L
    if (gap_start <= gap_end) {
      rel <- seq(gap_start, gap_end) - old_start + 1L
      old_changed <- c(old_changed, rel[rel >= 1L & rel <= n_old_sec])
    }
  }

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

run_code <- function(code_text) {
  warnings_seen  <- character(0)
  console_output <- character(0)
  plot_files     <- character(0)

  # Run in the user's project directory, not the Shiny app directory
  old_wd <- setwd(PROJECT_ROOT)
  on.exit(setwd(old_wd), add = TRUE)

  # Open a numbered PNG device to capture any plots generated
  tmp_base    <- tempfile("classmate_plot_")
  tmp_pattern <- paste0(tmp_base, "%03d.png")
  grDevices::png(tmp_pattern, width = 900, height = 675, res = 120, bg = "white")
  plot_dev <- grDevices::dev.cur()

  result <- tryCatch({
    console_output <- capture.output({
      withCallingHandlers({
        source(textConnection(code_text), local = FALSE, print.eval = TRUE)
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
  plot_files <- candidates[file.exists(candidates) & file.size(candidates) > 8192]

  c(result, list(plot_files = plot_files))
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
explanation_to_html <- function(text) {
  # Bold: **text** -> <strong>text</strong>
  text <- gsub("\\*\\*([^*]+)\\*\\*", "<strong>\\1</strong>", text)
  # Backtick code: `x` -> <code>x</code>
  text <- gsub("`([^`]+)`", "<code>\\1</code>", text)
  # Paragraphs: blank lines
  paras <- strsplit(text, "\n{2,}")[[1]]
  paras <- trimws(paras)
  paras <- paras[nzchar(paras)]
  # Within paragraphs, single newlines become <br>
  paras <- gsub("\n", "<br>", paras)
  paste0("<p>", paras, "</p>", collapse = "\n")
}

# --- Usage tracking helpers --------------------------------------------------

PRICE_INPUT       <- 3.00   # USD per MTok (non-cached input)
PRICE_CACHE_WRITE <- 3.75   # USD per MTok
PRICE_CACHE_READ  <- 0.30   # USD per MTok
PRICE_OUTPUT      <- 15.00  # USD per MTok

calc_cost <- function(usage) {
  cache_write <- usage$cache_creation_input_tokens %||% 0L
  cache_read  <- usage$cache_read_input_tokens     %||% 0L
  regular_in  <- (usage$input_tokens  %||% 0L) - cache_write - cache_read
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

usage_log_path   <- function() file.path(usage_data_dir(), "usage_log.rds")
active_cfg_path  <- function() {
  d <- tools::R_user_dir("classmate", "config")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  file.path(d, "active_config.rds")
}

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
  log <- read_usage_log()
  log <- c(log, list(list(timestamp = Sys.time(), cost_usd = cost_usd)))
  tryCatch(saveRDS(log, usage_log_path()), error = function(e) invisible(NULL))
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
                         api_key = Sys.getenv("ANTHROPIC_API_KEY"),
                         cache_system = TRUE) {
  system_block <- if (!is.null(system_prompt) && nzchar(system_prompt)) {
    cc <- if (cache_system) list(type = "ephemeral") else NULL
    list(Filter(Negate(is.null),
                list(type = "text", text = system_prompt, cache_control = cc)))
  } else NULL

  body <- list(model = model, max_tokens = max_tokens, messages = messages)
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
    text        = result$content[[1]]$text,
    stop_reason = result$stop_reason,
    cost_usd    = calc_cost(result$usage %||% list())
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
    body.help-mode #student-mode-label { display: none !important; }

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
            Shiny.setInputValue('help_button_clicked', {id: id, nonce: Math.random()}, {priority: 'event'});
            return;
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

  # Lightbox overlay for plot thumbnails
  tags$div(
    id = "classmate-lb-overlay",
    style = paste0(
      "display: none; position: fixed; top: 0; left: 0; width: 100%; height: 100%;",
      "background: rgba(0,0,0,0.75); z-index: 99998;",
      "align-items: center; justify-content: center;"
    ),
    onclick = "this.style.display='none';",
    tags$img(
      id    = "classmate-lb-img",
      src   = "",
      style = paste0(
        "max-width: 90%; max-height: 90vh;",
        "border-radius: 6px; box-shadow: 0 4px 24px rgba(0,0,0,0.5);"
      )
    )
  ),

  # Tell-me-more inner window (sits above the explanation modal, below the lightbox)
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
      ),
      tags$span(id = "student-mode-label",
        style = "color: #888; font-size: 0.75em; display: none;",
        "Student mode")
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
      fluidRow(
        column(5,
          tags$h5("Paste an API key"),
          tags$p(em("For personal or instructor use.")),
          passwordInput("api_key", NULL, width = "100%")
        ),
        column(2,
          div(style = "text-align: center; padding-top: 55px;",
            tags$strong("— or —"))
        ),
        column(5,
          tags$h5("Load a key file"),
          tags$p(em("For students: load the classmate.key file from your instructor.")),
          fileInput("key_file", NULL, accept = ".key", width = "100%",
                    buttonLabel = "Browse...", placeholder = "classmate.key")
        )
      ),
      hr(),
      div(style = "display: flex; justify-content: space-between; align-items: center;",
        actionButton("set_key", "Set key", class = "btn-primary"),
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

    fluidRow(
      column(8,
        shinyFilesButton("file_select", "Add files",
          title = "Select one or more files", multiple = TRUE),
        tags$span(style = "display: inline-block; width: 4px;"),
        actionButton("add_objects", "Add objects"),
        tags$span(style = "display: inline-block; width: 4px;"),
        shinyFilesButton("load_workspace", "Load Workspace",
          title = "Select a workspace file (.RData / .rda)", multiple = FALSE),
        tags$span(style = "display: inline-block; width: 4px;"),
        actionButton("clear_workspace", "Clear Workspace")
      ),
      column(4,
        disabled(actionButton("remove_context", "Remove checked")),
        tags$span(style = "display: inline-block; width: 4px;"),
        disabled(actionButton("remove_all_context", "Remove all"))
      )
    ),
    tags$p(tags$strong("Files and objects added as context:")),
    uiOutput("context_list_ui"),

    hr(),
    fluidRow(
      column(8,
        actionButton("ask_plain", "Ask"),
        actionButton("ask_code", "Ask for Code", class = "btn-primary"),
        actionButton("clear_prompt", "Clear"),
        actionButton("change_key", "Change API key"),
        actionButton("load_key_file_btn", "Load key file")
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
        tags$div(style = "margin-top: 8px; min-height: 220px;",
          uiOutput("console_output_ui")
        )
      ),
      tabPanel("Past code",
        tags$div(style = "margin-top: 8px; min-height: 220px;",
          uiOutput("past_code_ui")
        )
      ),
    ),

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
        disabled(actionButton("save_block", "Save Code Block", class = "btn-primary")),
        div(style = "min-width: 175px;", uiOutput("code_saved_ui"))
      )),
      column(6,
        div(style = "display: flex; justify-content: flex-end; align-items: flex-start; white-space: nowrap;",
          actionButton("about_classmate", "About Classmate",
            style = "margin-right: 8px;"),
          actionButton("help_toggle", "?",
            style = "margin-right: 8px;"),
          actionButton("quick_console", "Quick Console",
            style = "background-color: white; border-color: #bbb; color: #333; margin-right: 8px;"),
          actionButton("pause_app", "Save & Pause",
            style = "background-color: white; border-color: #bbb; color: #333; margin-right: 14px;"),
          div(style = "display: inline-flex; flex-direction: column; align-items: flex-start;",
            div(
              actionButton("new_conversation", "New conversation",
                style = "background-color: #e67e22; border-color: #ca6f1e; color: white;"),
              actionButton("quit", "Quit", class = "btn-danger",
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

server <- function(input, output, session) {

  # Show a clean "session ended" page, then stop after 400 ms so the JS has
  # time to render before the WebSocket drops.
  do_quit <- function() {
    runjs("document.getElementById('session-ended-overlay').classList.add('visible');")
    later::later(function() stopApp(), delay = 0.4)
  }

  # Clear the instructor API key from the R environment when the session ends
  # (covers Quit button, Pause, browser-tab close, and network drop).
  # Personal users keep their own key — only student mode is cleared.
  session$onEnded(function() {
    if (isolate(app_mode()) == "student") Sys.unsetenv("ANTHROPIC_API_KEY")
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
          "your actual data values are not."
        ),
        tags$li(
          "Do not include personal information about yourself or others in your",
          "prompts, and avoid working with datasets that contain identifiable",
          "personal data during this session."
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
      footer = div(style = "display: flex; justify-content: space-between; width: 100%;",
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
  .resuming_pause  <- file.exists(PAUSE_FILE)   # checked before pause state is consumed
  if (.resuming_pause) {
    session$onFlushed(function() {
      shinyjs::runjs("
        var s = document.getElementById('classmate-splash');
        if (s) s.classList.add('hidden');
      ")
    }, once = TRUE)
  }
  app_mode        <- reactiveVal(if (!is.null(.saved_cfg)) "student" else "personal")
  # Show student-mode label on startup if already in student mode
  if (!is.null(.saved_cfg)) {
    observe({ shinyjs::show("student-mode-label") }) |> bindEvent(session$clientData$url_hostname, once = TRUE)
  }
  cost_limit_val        <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$cost_limit        else NULL)
  reset_period_val      <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$reset_period      else "weekly")
  final_expiry_val      <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$final_expiry      else NULL)
  max_conversations_val <- reactiveVal(if (!is.null(.saved_cfg)) .saved_cfg$max_conversations else NULL)
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
    "to real individuals.\n\nPress Clear to continue."
  )
  if (!.resuming_pause) {
    session$onFlushed(function() {
      updateTextAreaInput(session, "prompt", value = .PROTECTION_NOTICE)
    }, once = TRUE)
  }

  current_spend <- reactive({
    if (app_mode() != "student") return(0)
    period_spend(usage_log_rv(), reset_period_val())
  })

  key_expired <- reactive({
    if (app_mode() != "student") return(FALSE)
    expiry <- final_expiry_val()
    if (is.null(expiry)) return(FALSE)
    Sys.Date() > as.Date(expiry)
  })

  quota_exceeded <- reactive({
    if (app_mode() != "student") return(FALSE)
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
        tags$p("Please contact your instructor for a new key."),
        easyClose = FALSE,
        footer    = modalButton("OK")
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
             "The Ask, Ask for Code, Explain, and Fix buttons are now disabled."),
      tags$p(reset_line),
      tags$p("Any code already in the editor can still be run and edited."),
      easyClose = FALSE,
      footer    = modalButton("OK")
    ))
  }

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
    Sys.setenv(ANTHROPIC_API_KEY = .saved_cfg$api_key)
  }
  key_is_set   <- reactiveVal(Sys.getenv("ANTHROPIC_API_KEY") != "")
  changing_key <- reactiveVal(FALSE)

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

  show_change_key_modal <- function() {
    showModal(modalDialog(
      title = "Change API key",
      passwordInput("new_api_key", "Paste new API key:", value = "", width = "100%"),
      tags$p(em("Treat the key like a password — never paste it into a script you save or share.")),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_change_key", "Set key", class = "btn-primary")
      )
    ))
  }

  show_load_key_file_modal <- function() {
    showModal(modalDialog(
      title = "Load key file",
      tags$p("Select the classmate.key file provided by your instructor.",
             "If it differs from the currently loaded key, your usage quota",
             "will be reset in line with the new key's settings."),
      fileInput("mid_session_key_file", NULL, accept = ".key", width = "100%",
                buttonLabel = "Browse...", placeholder = "classmate.key"),
      footer = modalButton("Cancel")
    ))
  }

  observeEvent(input$load_key_file_btn, show_load_key_file_modal())

  observeEvent(input$mid_session_key_file, {
    req(input$mid_session_key_file)
    load_key_file(input$mid_session_key_file$datapath, on_success = function() {
      removeModal()
    })
  })

  load_key_file <- function(path, on_success = function() invisible(NULL)) {
    payload <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.null(payload) || is.null(payload$api_key)) {
      showModal(modalDialog(
        title = "Invalid key file",
        "This file does not appear to be a valid classmate key file.",
        footer = modalButton("OK")
      ))
      return(invisible(NULL))
    }
    withProgress(message = "Checking key...", value = 0.5, {
      ok <- validate_api_key(payload$api_key)
    })
    if (!ok) { show_key_error(); return(invisible(NULL)) }

    # If the key_id differs from the active config, this is a newly issued key
    # from the instructor — reset the usage log so the student gets a fresh quota.
    cfg <- tryCatch(readRDS(active_cfg_path()), error = function(e) NULL)
    if (!is.null(payload$key_id) &&
        (is.null(cfg) || !identical(cfg$key_id, payload$key_id))) {
      tryCatch(unlink(usage_log_path()), error = function(e) invisible(NULL))
      usage_log_rv(list())
    }

    # Save active config for classmate_config_show()
    tryCatch(saveRDS(payload, active_cfg_path()), error = function(e) invisible(NULL))
    app_mode(             "student")
    shinyjs::show("student-mode-label")
    cost_limit_val(       payload$cost_limit)
    reset_period_val(     payload$reset_period)
    final_expiry_val(     payload$final_expiry)
    max_conversations_val(payload$max_conversations)
    on_success()
    if (!isTRUE(getOption(".classmate_disclaimer_shown"))) {
      show_student_disclaimer()
      options(.classmate_disclaimer_shown = TRUE)
    }
  }

  observeEvent(input$set_key, {
    req(input$api_key)
    key <- trimws(input$api_key)
    withProgress(message = "Checking key...", value = 0.5, { ok <- validate_api_key(key) })
    if (ok) {
      app_mode("personal")
      key_is_set(TRUE)
    } else show_key_error()
  })

  observeEvent(input$key_file, {
    req(input$key_file)
    load_key_file(input$key_file$datapath, on_success = function() key_is_set(TRUE))
  })

  observeEvent(input$new_key_file, {
    req(input$new_key_file)
    load_key_file(input$new_key_file$datapath, on_success = function() {
      changing_key(FALSE)
      removeModal()
    })
  })

  observeEvent(input$quit_setup, do_quit())

  observeEvent(input$change_key, { changing_key(TRUE); show_change_key_modal() })

  observeEvent(input$confirm_change_key, {
    new_key <- trimws(input$new_api_key)
    if (!nzchar(new_key)) { removeModal(); return(invisible(NULL)) }
    removeModal()
    withProgress(message = "Checking key...", value = 0.5, { ok <- validate_api_key(new_key) })
    if (ok) {
      app_mode("personal")
      cost_limit_val(NULL)
      changing_key(FALSE)
      key_is_set(TRUE)
    } else show_key_error()
  })

  observeEvent(input$key_error_ok, {
    removeModal()
    if (changing_key()) show_change_key_modal()
  })

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
      ok <- tryCatch({
        resp <- call_claude(
          messages     = list(list(role = "user", content = "Hi")),
          model        = MODEL_SONNET,
          max_tokens   = 1,
          cache_system = FALSE
        )
        nzchar(trimws(resp$text))
      }, error = function(e) FALSE)
      if (!ok) {
        changing_key(FALSE)
        key_status_msg(paste(
          "Your API key is no longer valid — it may have been suspended or deleted.",
          "Please enter a new key, or re-enter the same key once it has been reactivated."
        ))
        key_is_set(FALSE)
      }
    })
  })

  # --- Usage bar -------------------------------------------------------------
  output$usage_bar_ui <- renderUI({
    if (app_mode() != "student") return(NULL)
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
        target = "Past code", position = "after", select = FALSE)
      changes_tab_shown(TRUE)
    }
    updateTabsetPanel(session, "main_tabs", selected = "Changes")
  }

  hide_changes_tab <- function() {
    if (changes_tab_shown()) {
      if (isTRUE(isolate(input$main_tabs) == "Changes"))
        updateTabsetPanel(session, "main_tabs", selected = "Code")
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
        max_tokens   = 40,
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
    ph <- prompt_history()
    rh <- run_history()
    if (length(ph) == 0 && length(rh) == 0) return(invisible(NULL))
    summary <- make_conversation_summary(ph, api_key_val())
    entry <- list(
      id          = as.numeric(Sys.time()),
      summary     = summary,
      prompts     = ph,
      codes       = rh,
      log_entries = all_log_entries(),
      files       = unique(selected_files()),
      objects     = unique(selected_objects())
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
            onclick = paste0(
              "document.getElementById('classmate-lb-img').src='", src, "';",
              "document.getElementById('classmate-lb-overlay').style.display='flex';"
            ),
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
                    "load_script", "save_block", "help_toggle",
                    "quick_console", "pause_app", "clear_workspace", "new_conversation", "quit"))
        shinyjs::disable(btn)
      return()
    }
    has_code   <- nzchar(trimws(input$code_editor %||% ""))
    exceeded   <- isTRUE(quota_exceeded())
    on_code_tab     <- is.null(input$main_tabs) || isTRUE(input$main_tabs == "Code")
    on_changes_tab  <- isTRUE(input$main_tabs == "Changes")
    has_unrun_code  <- is.null(last_run_code()) ||
                       !identical(trimws(input$code_editor %||% ""), trimws(last_run_code()))
    if (has_code && has_unrun_code && !code_running() && !exceeded && (on_code_tab || on_changes_tab)) {
      enable("run_code")
    } else {
      disable("run_code")
    }
    # Explain: Code or Changes tab with code, R output tab with content, or after a failed run
    on_output_tab  <- isTRUE(input$main_tabs == "R output")
    has_run_output <- length(console_output_rv()) > 0 || length(plot_files_rv()) > 0 || length(warnings_rv()) > 0
    if (!code_running() && !exceeded &&
        ((has_code && (on_code_tab || on_changes_tab)) || last_run_failed() ||
         (on_output_tab && has_run_output))) {
      enable("explain_code")
    } else {
      disable("explain_code")
    }
    if (last_run_failed() && !code_running() && !exceeded)
      enable("fix_code") else disable("fix_code")
    if (on_code_tab && !exceeded && !is.null(last_run_code())) {
      enable("save_block")
    } else {
      disable("save_block")
    }
    if (on_code_tab && !exceeded) {
      enable("load_script")
    } else {
      disable("load_script")
    }
    if (protection_notice_active()) {
      for (btn in c("ask_plain", "ask_code", "explain_code", "fix_code", "run_code",
                    "load_script", "save_block", "help_toggle",
                    "quick_console", "pause_app", "clear_workspace", "new_conversation",
                    "add_objects", "remove_context", "remove_all_context",
                    "preferences", "change_key", "load_key_file_btn",
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
    for (btn in c("help_toggle", "quick_console", "pause_app",
                  "add_objects", "preferences", "change_key", "load_key_file_btn",
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
      tags$p("If you reach your limit, buttons will be disabled until the allowance resets or your instructor provides a new key."),
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

  help_text_for <- function(btn_id) {
    texts <- list(
      help_toggle      = "You are in Help mode. Click any active button to read what it does. Press ❓ again to return to normal.",
      ask_plain        = paste(
        "Ask — Send a prompt to the AI. If the response includes R code, it is run",
        "immediately and the result appears in the R output tab. The code is saved to",
        "your code log but is not placed in the editor. Use Ask for quick one-step",
        "tasks where you want to see the output straight away."
      ),
      ask_code         = paste(
        "Ask for Code — Send a prompt to the AI and have the generated code placed",
        "in the Code editor for you to inspect before running. Press Run when you are",
        "ready to execute it. Use Ask for Code when you want to review or edit the code",
        "first."
      ),
      clear_prompt     = "Clear — Erases all text in the prompt box.",
      run_code         = paste(
        "Run — Executes the code currently in the editor. Results and any plots",
        "appear in the R output tab. If the code runs successfully it is added to",
        "Past Code and your code log."
      ),
      explain_code     = paste(
        "Explain — Asks the AI to explain the current code or R output. On the",
        "Code tab it explains what the code does; on the R output tab it interprets",
        "what the results mean. If you have highlighted a passage in an explanation",
        "window, it unpacks that specific part in more detail."
      ),
      fix_code         = paste(
        "Fix — Available after a failed run. Asks the AI to diagnose the error and",
        "produce corrected code. The fix is applied to the editor and run automatically.",
        "If the fix does not work you can try again, or cancel to restore the original code."
      ),
      load_script      = paste(
        "Load Script — Opens a file browser so you can load an existing R script",
        "from your project folder into the Code editor. Only available on the Code tab.",
        "Scripts longer than the current code-length limit will be refused."
      ),
      code_saved_notebook = paste(
        "Code saved to Notebook — This message confirms that the code you just ran",
        "has been added to the Code Log for this conversation.",
        "The log is saved as an R Notebook (.Rmd file) in your project folder",
        "and opened in RStudio automatically whenever you start a New Conversation,",
        "Pause, or Quit the app.",
        "One notebook is created per conversation: starting a New Conversation",
        "saves and closes the current notebook and begins a fresh one for the next",
        "conversation. This means you always have a clean, timestamped record of",
        "every piece of code that was successfully run in each conversation."
      ),
      save_block       = paste(
        "Save Code Block — Saves the code currently in the editor to a named .R",
        "file in your project folder (using the code description as the filename) and",
        "opens it in RStudio."
      ),
      quick_console    = paste(
        "Quick Console — Opens a small R console inside the app where you can run",
        "R code directly, inspect objects, or create new variables — all within the",
        "same workspace as the main app. The rest of the app is frozen while the",
        "Quick Console is open. Close it when you are done to return to the app.",
        "Note: typing q() or quit() will just close the Quick Console, not end your R session."
      ),
      pause_app        = paste(
        "Save & Pause — Saves your entire session — conversation history, code,",
        "context, and preferences — and closes Classmate. Reopen the app with talk()",
        "to pick up exactly where you left off.",
        "Use this if you want to take a longer break and return to the same conversation later.",
        "For a quick step out to run some R code, use Quick Console instead."
      ),
      clear_workspace  = paste(
        "Clear Workspace — Removes all R objects from your global environment",
        "(.GlobalEnv). This is the equivalent of running rm(list = ls()) in the console.",
        "Use with care — this cannot be undone."
      ),
      new_conversation = paste(
        "New Conversation — Starts a fresh session, clearing the Code editor,",
        "R output, Past code, and Past prompts. The current conversation is saved",
        "to Past conversations and the code log is saved automatically."
      ),
      quit             = paste(
        "Quit — Closes Classmate. Your code notebook (.Rmd) is saved",
        "automatically and opened in your editor. Your R workspace is not affected."
      ),
      file_select      = paste(
        "Add Files — Opens a file browser so you can attach files from your project",
        "folder as background context. The AI can see the filenames and their paths and",
        "will use them to inform its code (for example, when reading data)."
      ),
      add_objects      = paste(
        "Add Objects — Lets you choose objects from your current R workspace to",
        "share as context with the AI. A structural summary of each object is included,",
        "so the AI can see column names, data types, dimensions, and so on."
      ),
      remove_context   = "Remove Checked — Removes the ticked files from the context list.",
      remove_all_context = "Remove All — Clears all attached files from the context.",
      load_workspace   = paste(
        "Load Workspace — Loads a saved R workspace file (.RData or .rda) into your",
        "current R session, making its objects available as context."
      ),
      change_key       = "Change API Key — Lets you enter or update the Anthropic API key used to call the AI.",
      preferences      = paste(
        "Preferences — Opens the preferences panel where you can set your preferred",
        "coding style (base R, tidyverse, data.table), plotting library, mapping package,",
        "maximum code length, image export format and quality, and comment density."
      ),
      tell_more        = paste(
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
      explain_close    = "Close — Closes the Explanation window.",
      fix_cancel       = "Cancel — Abandons the current fix attempt and restores the original code.",
      fix_retry        = "Continue Fixing — Asks the AI to try again after an unsuccessful fix attempt."
    )
    # Dynamically-generated Repeat buttons in Past Code
    if (grepl("^re_run_", btn_id))
      return(paste(
        "Repeat — Re-loads this previously generated code block into the Code",
        "editor so you can run it again or use it as a starting point."
      ))

    # Tab headers (prefixed with tab__ by the JS overlay)
    if (startsWith(btn_id, "tab__")) {
      tab_val <- sub("^tab__", "", btn_id)
      tab_help <- list(
        "Your prompt"  = paste(
          "Your Prompt tab — This is where you type your question or instruction for the AI.",
          "Once submitted with Ask or Ask for Code, the prompt box clears ready for the next step."
        ),
        "Past prompts" = paste(
          "Past Prompts tab — Shows a history of all the prompts you have sent to the AI",
          "in this conversation. Click any entry to copy it back into the prompt box."
        ),
        "Code"         = paste(
          "Code tab — Contains the R code editor. Code generated by Ask for Code appears here",
          "for you to review. You can also type or paste code directly. Press Run to execute it."
        ),
        "R output"     = paste(
          "R Output tab — Displays the results of running your code: printed values, tables,",
          "statistical summaries, and plots. When more than one type of output is produced,",
          "separate Results, Plots, and Warnings sub-tabs appear."
        ),
        "Past code"    = paste(
          "Past Code tab — A record of every code block that has been run successfully in",
          "this conversation. Use the Repeat button next to any entry to reload that code",
          "into the editor."
        ),
        "Changes"      = paste(
          "Changes tab — Appears when the AI makes targeted edits to existing code.",
          "Shows the new version alongside the previous version, with changed lines",
          "highlighted in yellow, so you can see exactly what was altered."
        ),
        "Results"      = paste(
          "Results sub-tab — The printed text output from running the code: values,",
          "data summaries, model output, and any messages printed to the console."
        ),
        "Plots"        = paste(
          "Plots sub-tab — Image output from your code, shown as thumbnails.",
          "Click any thumbnail to expand it to full size."
        ),
        "Warnings"     = paste(
          "Warnings sub-tab — Any warning messages produced when the code ran.",
          "Warnings do not stop the code from running but may indicate something",
          "worth checking, such as missing values or a coordinate reference system mismatch."
        )
      )
      return(tab_help[[tab_val]] %||% paste0("Tab: ", tab_val, "."))
    }

    texts[[btn_id]] %||% paste0(
      "No help available for this button (id: ", btn_id, ")."
    )
  }

  observeEvent(input$help_button_clicked, {
    req(help_mode())
    btn_id <- input$help_button_clicked$id
    req(nzchar(btn_id %||% ""))
    showModal(modalDialog(
      title     = "Help",
      tags$p(help_text_for(btn_id), style = "line-height: 1.6;"),
      footer    = modalButton("OK"),
      easyClose = TRUE
    ))
  })

  output$prompt_history_ui <- renderUI({
    history <- prompt_history()
    if (length(history) == 0)
      return(tags$p(em("No prompts yet."), style = "color: #999; margin: 0; font-size: 0.9em;"))
    tagList(lapply(history, function(p) {
      # Escape single quotes in the prompt text so it survives the onclick string.
      p_escaped <- gsub("'", "\\\\'", p, fixed = TRUE)
      div(style = "display: flex; align-items: flex-start; border-bottom: 1px solid #eee; padding: 2px 0;",
        tags$p(p, style = paste0(
          "margin: 0; flex: 1;",
          "font-size: 0.85em; color: #444;",
          "white-space: pre-wrap; word-break: break-word;"
        )),
        tags$button("Repeat",
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
    if (length(keep_idx) == 0)
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

    # Newest at top — iterate in reverse order, using original indices
    items <- lapply(rev(keep_idx), function(i) {
      entry  <- full_history[[i]]
      label  <- make_label(entry)
      btn_id <- paste0("repeat_code_", i)
      div(style = "display: flex; align-items: flex-start; border-bottom: 1px solid #eee; padding: 3px 0;",
        tags$p(label, style = paste0(
          "margin: 0; flex: 1; font-size: 0.85em; color: #444;",
          "white-space: pre-wrap; word-break: break-word;"
        )),
        actionButton(btn_id, "Repeat",
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

  # Observe any repeat_code_N button — move entry to top (no duplicate), load into editor
  observe({
    history <- run_history()
    lapply(seq_along(history), function(i) {
      btn_id <- paste0("repeat_code_", i)
      observeEvent(input[[btn_id]], {
        h <- run_history()
        entry <- h[[i]]
        run_history(c(list(entry), h[-i]))
        updateAceEditor(session, "code_editor", value = entry$code)
        last_run_code(NULL)
        updateTabsetPanel(session, "main_tabs", selected = "Code")
      }, ignoreInit = TRUE)
    })
  })

  # Past prompt repeat: move to top (no duplicate)
  observeEvent(input$copy_past_prompt, {
    p   <- input$copy_past_prompt
    ph  <- prompt_history()
    idx <- match(p, ph)
    if (!is.na(idx)) ph <- c(p, ph[-idx]) else ph <- c(p, ph)
    prompt_history(ph)
    updateTextAreaInput(session, "prompt", value = p)
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
            modalButton("Cancel"),
            actionButton("confirm_recall_conv", "Yes, recall this conversation", class = "btn-primary")
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
    updateTabsetPanel(session, "main_tabs", selected = "Code")
    if (current_is_blank) conv_count_rv(max(conv_count_rv() - 1L, 1L))
  })

  # --- File browser and workspace object picker -----------------------------
  volumes     <- build_volumes()
  default_loc <- compute_default_root_and_path(volumes, wd = PROJECT_ROOT)

  shinyFileChoose(input, "file_select", roots = volumes, session = session,
                   defaultRoot = default_loc$root, defaultPath = default_loc$path)

  observeEvent(input$file_select, {
    file_info <- parseFilePaths(volumes, input$file_select)
    if (nrow(file_info) > 0)
      selected_files(union(selected_files(), file_info$datapath))
  })

  shinyFileChoose(input, "load_workspace", roots = volumes, session = session,
                   defaultRoot = default_loc$root, defaultPath = default_loc$path,
                   filetypes = c("RData", "rda", "rds"))

  observeEvent(input$load_workspace, {
    file_info <- parseFilePaths(volumes, input$load_workspace)
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

  shinyFileChoose(input, "load_script", roots = volumes, session = session,
                   defaultRoot = default_loc$root, defaultPath = default_loc$path,
                   filetypes = c("R", "r"))

  observeEvent(input$load_script, {
    file_info <- parseFilePaths(volumes, input$load_script)
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
        if (app_mode() != "student") tags$p(em(
          "You can increase or remove the code-length limit in Preferences."
        )),
        footer    = modalButton("OK"),
        easyClose = FALSE
      ))
      return()
    }
    loaded_script_name(fname)
    updateAceEditor(session, "code_editor", value = script_text)
    updateTabsetPanel(session, "main_tabs", selected = "Code")
  })

  output$obj_picker_list <- renderUI({
    filter_text <- tolower(trimws(if (is.null(input$obj_filter)) "" else input$obj_filter))
    all_objs    <- ls(envir = .GlobalEnv)
    if (length(all_objs) == 0) return(tags$p(em("No objects in workspace yet.")))
    matched <- if (nzchar(filter_text))
      all_objs[grepl(filter_text, tolower(all_objs), fixed = TRUE)]
    else
      all_objs
    if (length(matched) == 0) return(tags$p(em("No objects match the filter.")))
    div(style = "max-height: 300px; overflow-y: auto;",
      checkboxGroupInput("obj_picker_checked", label = NULL,
                          choices = matched, selected = character(0))
    )
  })

  observeEvent(input$add_objects, {
    showModal(modalDialog(
      title = "Add workspace objects as context",
      textInput("obj_filter", "Filter by name:", value = "", placeholder = "Type to filter..."),
      uiOutput("obj_picker_list"),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_add_objects", "Add selected", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_add_objects, {
    to_add <- input$obj_picker_checked
    if (!is.null(to_add) && length(to_add) > 0)
      selected_objects(union(selected_objects(), to_add))
    removeModal()
  })

  output$context_list_ui <- renderUI({
    files <- selected_files();  files <- files[nzchar(files)]
    objs  <- selected_objects(); objs  <- objs[nzchar(objs)]
    if (length(files) == 0 && length(objs) == 0)
      return(tags$p(em("None added yet.")))
    choices <- character(0)
    if (length(files) > 0)
      choices <- c(choices, stats::setNames(paste0("file::", files), basename(files)))
    if (length(objs) > 0)
      choices <- c(choices, stats::setNames(paste0("obj::", objs), paste0(objs, " [obj]")))
    div(
      style = paste0(
        "max-height: 100px; overflow-y: auto;",
        "border: 1px solid #e3e3e3; border-radius: 4px;",
        "padding: 2px 8px; background: #fafafa;"
      ),
      checkboxGroupInput("context_list_checked", label = NULL,
                          choices = choices, selected = character(0))
    )
  })

  observeEvent(input$remove_context, {
    to_remove <- input$context_list_checked
    if (!is.null(to_remove) && length(to_remove) > 0) {
      file_hits <- to_remove[startsWith(to_remove, "file::")]
      obj_hits  <- to_remove[startsWith(to_remove, "obj::")]
      if (length(file_hits) > 0)
        selected_files(setdiff(selected_files(), sub("^file::", "", file_hits)))
      if (length(obj_hits)  > 0)
        selected_objects(setdiff(selected_objects(), sub("^obj::", "", obj_hits)))
    }
  })

  observeEvent(input$remove_all_context, {
    if (length(selected_files()) == 0 && length(selected_objects()) == 0)
      return(invisible(NULL))
    showModal(modalDialog(
      title = "Remove all context?",
      "Are you sure? This will remove all files and objects from the context list.",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_remove_all_context", "Yes, remove all", class = "btn-warning")
      )
    ))
  })

  observeEvent(input$confirm_remove_all_context, {
    selected_files(character(0))
    selected_objects(character(0))
    removeModal()
  })

  observeEvent(input$clear_workspace, {
    showModal(modalDialog(
      title = "Clear workspace?",
      "Are you sure? This will remove all objects from the R workspace (rm(list = ls())).",
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_clear_workspace", "Yes, clear workspace",
          style = "background-color: #e67e22; border-color: #ca6f1e; color: white;")
      )
    ))
  })

  observeEvent(input$confirm_clear_workspace, {
    rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
    selected_objects(character(0))
    removeModal()
  })

  # --- Restore paused state (if any) ----------------------------------------
  ps <- if (file.exists(PAUSE_FILE)) {
    saved <- tryCatch(readRDS(PAUSE_FILE), error = function(e) NULL)
    unlink(PAUSE_FILE)
    saved
  } else NULL

  # --- Conversation + run-history state --------------------------------------
  conversation_history  <- reactiveVal(if (!is.null(ps)) ps$conversation_history  else list())
  saved_conversations   <- reactiveVal(if (!is.null(ps)) ps$saved_conversations   else list())
  last_extracted_code   <- reactiveVal(if (!is.null(ps)) ps$last_extracted_code   else "")
  last_prompt_for_code <- reactiveVal(if (!is.null(ps)) ps$last_prompt_for_code else "")
  last_code_description <- reactiveVal("")
  loaded_script_name    <- reactiveVal(NULL)
  run_history          <- reactiveVal(if (!is.null(ps)) ps$run_history          else list())
  pending_log_entries  <- reactiveVal(if (!is.null(ps)) ps$pending_log_entries  else list())
  all_log_entries      <- reactiveVal(if (!is.null(ps)) ps$all_log_entries      else list())
  last_logged_code     <- reactiveVal(if (!is.null(ps)) ps$last_logged_code     else "")
  prompt_history       <- reactiveVal(if (!is.null(ps)) ps$prompt_history       else character(0))
  conv_count_rv        <- reactiveVal(if (!is.null(ps)) ps$conv_count_rv        else 1L)

  # log_path is generated fresh each time Save Code Log is pressed (date-time stamped)
  make_log_path <- function()
    file.path(PROJECT_ROOT,
      paste0("classmate_log_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".Rmd"))
  log_path <- reactiveVal(file.path(PROJECT_ROOT, "classmate_log.Rmd"))  # placeholder

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
      updateTabsetPanel(session, "main_tabs", selected = "R output")
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
    model           = pref_val("model",            MODEL_SONNET),
    img_format      = pref_val("img_format",       "png"),
    img_quality     = pref_val("img_quality",      "Medium"),
    img_size        = pref_val("img_size",         "Journal double col. (180x120 mm)"),
    max_lines       = pref_val("max_lines",        50),
    comment_density = pref_val("comment_density",  "Minimal"),
    language        = {
      .lp <- file.path(tools::R_user_dir("classmate", "config"), "language.rds")
      if (file.exists(.lp)) tryCatch(readRDS(.lp), error = function(e) "English") else "English"
    }
  )

  model_choices <- c("Claude Sonnet" = MODEL_SONNET)

  show_preferences_modal <- function() {
    lang         <- prefs$language
    lang_choices <- if (tolower(lang) == "english") "English" else c(lang, "English")
    showModal(modalDialog(
      title = "Preferences",
      selectInput("pref_model", "Model:", choices = model_choices, selected = prefs$model),
      hr(),
      fluidRow(
        column(8, selectInput("pref_language", "Response language:",
          choices = lang_choices, selected = lang)),
        column(4, div(style = "margin-top: 25px;",
          actionButton("pref_lang_change", "Change", class = "btn-default btn-sm")))
      ),
      tags$p(em(
        "The language classmate uses for explanations. R code is always written in English."
      )),
      hr(),
      tags$h5("Preferred approach for..."),
      selectInput("pref_coding", "Coding:",
                  choices = c("tidyverse", "Base R"), selected = prefs$coding),
      selectInput("pref_plotting", "Plotting:",
                  choices = c("ggplot2", "Base R"), selected = prefs$plotting),
      selectInput("pref_mapping", "Mapping:",
                  choices = c("tmap", "ggplot2", "leaflet", "mapsf", "Base R"),
                  selected = prefs$mapping),
      tags$p(em(
        "These are defaults Claude will lean towards, not hard rules — ",
        "you can always ask for something different in your prompt."
      )),
      hr(),
      tags$h5("Image export defaults"),
      fluidRow(
        column(4, selectInput("pref_img_format", "Format:",
          choices = c("png", "jpg", "pdf", "tif", "bmp"), selected = prefs$img_format)),
        column(4, selectInput("pref_img_quality", "Quality / resolution:",
          choices = c("Low", "Medium", "High"), selected = prefs$img_quality)),
        column(4, selectInput("pref_img_size", "Target size:",
          choices = c(
            "Journal single col. (90x90 mm)",
            "Journal double col. (180x120 mm)",
            "A4 (210x297 mm)", "A3 (297x420 mm)", "Letter (8.5x11 in)",
            "4x6 in / 10x15 cm", "5x7 in / 13x18 cm", "8x10 in / 20x25 cm",
            "Square 6x6 in"
          ), selected = prefs$img_size))
      ),
      tags$p(em(
        "These guide Claude when writing image-saving code (ggsave, png, pdf, tiff, etc.).",
        "Low ≈ web/screen, Medium ≈ standard print, High ≈ high-quality print.",
        "As with other preferences, these are defaults — explicit prompts or a better",
        "fit for the task always take priority."
      )),
      hr(),
      tags$h5("Code style"),
      fluidRow(
        column(6,
          selectInput("pref_max_lines", "Maximum code length (lines):",
            choices = if (isolate(app_mode()) == "student")
              c("50 lines" = 50, "100 lines" = 100)
            else
              c("50 lines" = 50, "100 lines" = 100, "Unlimited" = Inf),
            selected = prefs$max_lines)
        ),
        column(6,
          selectInput("pref_comment_density", "Comments in generated code:",
            choices  = c("None", "Minimal", "Most"),
            selected = prefs$comment_density)
        )
      ),
      tags$p(em(
        "Maximum length is a soft target: Claude will aim to stay within it but may",
        "exceed it when the task genuinely requires more. Scripts you load directly",
        "will also generate a warning if they exceed this limit.",
        "Comment levels — None: no comments at all; Minimal: brief section markers only",
        "(e.g. '# Load data'); Most: section markers plus short notes on non-obvious lines.",
        "Even at Most, line-by-line commenting is avoided."
      )),
      footer = modalButton("Close")
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
      selected = if (current %in% langs) current else if (length(langs) > 0) langs[[1]] else character(0))
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
  observeEvent(input$pref_model,           { prefs$model           <- input$pref_model })
  observeEvent(input$pref_img_format,      { prefs$img_format      <- input$pref_img_format })
  observeEvent(input$pref_img_quality,     { prefs$img_quality     <- input$pref_img_quality })
  observeEvent(input$pref_img_size,        { prefs$img_size        <- input$pref_img_size })
  observeEvent(input$pref_max_lines, {
    v <- input$pref_max_lines
    prefs$max_lines <- if (v == "Inf") Inf else as.integer(v)
  })
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
            max_tokens    = 2000,
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
    hide_changes_tab()
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
    if (app_mode() != "student") return(NULL)
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
        modalButton("Cancel"),
        actionButton("confirm_new_conversation", "Yes, start new conversation", class = "btn-danger")
      )
    ))
  })

  observeEvent(input$confirm_new_conversation, {
    removeModal()
    do_new_conversation_reset()
  })

  # --- Quick Console ---------------------------------------------------------
  qc_history <- reactiveVal(list())  # list of list(input, output, is_error, plot_path)

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
      tags$p(em(style = "font-size:0.8em; color:#888;",
        "Same workspace as the main app. ",
        "q() and quit() close the Quick Console, not your R session.")),
      footer = tagList(
        actionButton("qc_run",   "Run",         class = "btn-success"),
        actionButton("qc_clear", "Clear output", style = "margin-left: 4px;"),
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
    old_wd <- setwd(PROJECT_ROOT)
    on.exit(setwd(old_wd), add = TRUE)
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
      "Please close or pause Classmate and run it from your main R console."
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
    result <- tryCatch({
      setTimeLimit(elapsed = 30, transient = TRUE)
      on.exit({ setTimeLimit(elapsed = Inf, transient = FALSE) }, add = TRUE)
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
        return(list(output = "Timed out after 30 seconds. The operation was interrupted.", is_error = TRUE, plot_path = NULL))
      list(output = strip_ansi(msg), is_error = TRUE, plot_path = NULL)
    })
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

  do_qc_run <- function() {
    code <- isolate(input$qc_input %||% "")
    if (!nzchar(trimws(code))) return()
    result <- run_in_qc(code)
    qc_history(c(qc_history(), list(list(
      input     = code,
      output    = result$output,
      is_error  = result$is_error,
      plot_path = result$plot_path
    ))))
    shinyAce::updateAceEditor(session, "qc_input", value = "")
    if (!is.null(result$plot_path)) {
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

  observeEvent(input$qc_plot_close, {
    removeModal()
    show_qc_modal()
  })

  observeEvent(input$qc_clear, {
    qc_history(list())
  })

  # --- Ask for Code ----------------------------------------------------------
  do_ask_code <- function(current_prompt) {
    if (isTRUE(quota_exceeded())) { show_quota_modal(); return(invisible(NULL)) }
    hide_changes_tab()
    console_output_rv(character(0)); warnings_rv(character(0)); plot_files_rv(character(0)); diff_rv(NULL)
    disable("ask_code"); disable("ask_plain")
    tryCatch({
      user_message <- build_user_message(
        file_paths          = selected_files(),
        object_names        = selected_objects(),
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
          max_tokens    = 800,
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
          if (app_mode() != "student") tags$p(em(
            "You can increase or remove the code-length limit in Preferences."
          )),
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
        updateTabsetPanel(session, "main_tabs", selected = "Code")
      }
    }, finally = { enable("ask_code"); enable("ask_plain") })
  }

  observeEvent(input$ask_code, {
    req(input$prompt)
    do_ask_code(input$prompt)
  })

  observeEvent(input$oos_ok_code, {
    removeModal()
    updateTextAreaInput(session, "prompt", value = "")
  })

  # --- Ask (auto-run) --------------------------------------------------------
  do_ask_plain <- function(current_prompt) {
    if (isTRUE(quota_exceeded())) { show_quota_modal(); return(invisible(NULL)) }
    ui_busy(TRUE)
    hide_changes_tab()
    updateTextAreaInput(session, "prompt", value = "")
    console_output_rv(character(0)); warnings_rv(character(0)); plot_files_rv(character(0)); diff_rv(NULL)
    disable("ask_code"); disable("ask_plain")

    # --- Synchronous part: call Claude -----------------------------------------
    user_message <- build_user_message(
      file_paths          = selected_files(),
      object_names        = selected_objects(),
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
        max_tokens    = 800,
        system_prompt = build_system_prompt(prefs$coding, prefs$plotting, prefs$mapping,
                                            prefs$img_format, prefs$img_quality, prefs$img_size,
                                            prefs$max_lines, prefs$comment_density,
                                            loaded_packages(), prefs$language)
      ),
      error = function(e) list(text = "", stop_reason = "error")
    )
    raw_response <- api_result$text

    if (!nzchar(trimws(raw_response))) {
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
      output$run_status <- renderUI(tags$span(
        "No response from Claude. Check your API key and try again.",
        style = "color: #b22222;"))
      return(invisible(NULL))
    }
    if (identical(api_result$stop_reason, "max_tokens")) {
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
      showModal(modalDialog(
        title = "Prompt too complex",
        "The response was cut short. Try asking for a simpler or smaller piece of code, or split the task into steps.",
        easyClose = FALSE, footer = modalButton("OK")))
      return(invisible(NULL))
    }
    if (grepl("^OUT_OF_SCOPE", trimws(raw_response), ignore.case = TRUE)) {
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
      showModal(modalDialog(
        title = "Out of scope",
        "Sorry, this request is out of scope. Classmate is here to help with R programming, data analysis, and GIS tasks.",
        easyClose = FALSE,
        footer = actionButton("oos_ok_plain", "OK", class = "btn-primary")))
      return(invisible(NULL))
    }

    # Clarity check: prompt too vague to write grounded code
    if (grepl("^NEEDS_CLARIFICATION", trimws(raw_response), ignore.case = TRUE)) {
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
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
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
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
    last_prompt_for_code(current_prompt)
    last_extracted_code(parts$code)

    if (!nzchar(trimws(parts$code))) {
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
      output$run_status <- renderUI(NULL)
      return(invisible(NULL))
    }

    if (code_too_long(parts$code, prefs$max_lines)) {
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
      showModal(modalDialog(
        title = "Response too complex",
        tags$p(
          "The code Claude generated exceeds the current code-length limit (",
          prefs$max_lines, " lines). This usually means the prompt covered too much at once."
        ),
        tags$p("Try breaking it into smaller steps — for example, load the data first, ",
               "then build the analysis, then visualise the results, one prompt at a time."),
        if (app_mode() != "student") tags$p(em(
          "You can increase or remove the code-length limit in Preferences."
        )),
        footer    = modalButton("OK"),
        easyClose = FALSE
      ))
      return(invisible(NULL))
    }

    # --- Possibly async part: check packages then run --------------------------
    captured_parts <- parts
    check_and_install_if_needed(parts$code, function() {
      run_result <- withProgress(message = "Running code...", value = 0.5,
                                 run_code(captured_parts$code))
      last_run_code(captured_parts$code)
      handle_run_result(run_result)
      if (run_result$success) {
        last_run_failed(FALSE); last_error_msg("")
        new_pkgs <- vapply(extract_pkg_info(captured_parts$code), `[[`, character(1), "pkg")
        loaded_packages(unique(c(loaded_packages(), new_pkgs)))
        entry <- list(prompt = current_prompt, code = captured_parts$code,
                      description = captured_parts$description %||% "", source = "ask_plain")
        pending_log_entries(c(pending_log_entries(), list(entry)))
        all_log_entries(c(all_log_entries(), list(entry)))
      } else {
        last_run_failed(TRUE)
        last_error_msg(sub("^Error running code:\\s*\\n?\\s*", "", run_result$message))
      }
      ui_busy(FALSE); enable("ask_code"); enable("ask_plain")
    })
  }

  observeEvent(input$ask_plain, {
    req(input$prompt)
    do_ask_plain(input$prompt)
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
          max_tokens    = 380,
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
              max_tokens    = 400,
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
              max_tokens    = 700,
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
              max_tokens    = 600,
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
            max_tokens    = 1200,
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
            max_tokens    = 1200,
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
  observeEvent(input$run_code, {
    ui_busy(TRUE)
    code_text <- input$code_editor
    updateTextAreaInput(session, "prompt", value = "")
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
                          description = desc, source = "ask_code")
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
          max_tokens    = 800,
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
          actionButton("fix_cancel", "Cancel", class = "btn-danger"),
          actionButton("fix_retry",  "Continue fixing", class = "btn-primary")
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
                      description = last_code_description(), source = "ask_code")
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
          footer = div(style = "display: flex; justify-content: space-between; width: 100%;",
            actionButton("fix_cancel", "Cancel",          class = "btn-danger"),
            actionButton("fix_retry",  "Continue fixing", class = "btn-primary")
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
  handle_run_result <- function(run_result, stay_on_code = FALSE) {
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
    has_output <- nzchar(trimws(paste(base_output, collapse = "\n"))) || length(plots) > 0
    if (!stay_on_code && has_output) {
      updateTabsetPanel(session, "main_tabs", selected = "R output")
    } else {
      updateTabsetPanel(session, "main_tabs", selected = "Code")
    }
  }

  # --- Code saved indicator --------------------------------------------------
  output$code_saved_ui <- renderUI({
    req(!is.null(last_run_code()))
    req(!last_run_failed())
    req(identical(trimws(input$code_editor %||% ""), trimws(last_run_code())))
    if (isTRUE(help_mode())) {
      actionButton("code_saved_notebook", "Code saved to Notebook",
        style = "background-color: #111; color: #fff; border-color: #111;")
    } else {
      tags$span("Code saved to Notebook",
        style = "font-size: 0.9em; color: #555; line-height: 31px;")
    }
  })

  observeEvent(input$code_saved_notebook, {
    showModal(modalDialog(
      title = "Code saved to Notebook",
      tags$p(
        "Each time code runs successfully, it is added to the Code Log for the",
        "current conversation."
      ),
      tags$p(
        "The log is saved automatically as a timestamped R Notebook (.Rmd file)",
        "in your project folder, and opened in RStudio, whenever you:"
      ),
      tags$ul(
        tags$li("start a ", tags$strong("New Conversation")),
        tags$li(tags$strong("Pause"), " the app"),
        tags$li(tags$strong("Quit"), " the app")
      ),
      tags$p(
        tags$strong("One notebook is created per conversation."),
        " Starting a New Conversation saves and closes the current notebook,",
        " and a fresh one begins for the next conversation.",
        " This gives you a clean, timestamped record of every piece of code",
        " that was successfully run in each conversation."
      ),
      tags$p(
        "In the notebook you can review and re-run each code chunk individually.",
        tags$strong(" Remember to Pause or Quit Classmate before running code",
                    " in the notebook to avoid conflicts.")
      ),
      footer = modalButton("Close"),
      easyClose = TRUE
    ))
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
    p <- file.path(PROJECT_ROOT, paste0(stem, ".R"))
    writeLines(code_text, p)
    open_log_file(p)
    output$run_status <- renderUI(tags$span(
      paste0("Code block saved to ", basename(p), "."),
      style = "color: #888;"
    ))
  })

  # --- Save & Pause ----------------------------------------------------------
  observeEvent(input$pause_app, {
    showModal(modalDialog(
      title = "Save & Pause",
      tags$p(tags$strong("Save your session and pause Classmate.")),
      tags$p(paste(
        "The current session — prompt, code, context files and objects,",
        "conversation history, and preferences — will be saved and restored",
        "automatically when you next start the app."
      )),
      tags$p(em(
        "The API key is not saved; it will be picked up from the R environment automatically."
      )),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_pause_app", "Save & Pause", class = "btn-primary")
      )
    ))
  })

  observeEvent(input$confirm_pause_app, {
    removeModal()
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
      pending_log_entries  = pending_log_entries(),
      all_log_entries      = all_log_entries(),
      last_extracted_code  = last_extracted_code(),
      last_prompt_for_code = last_prompt_for_code(),
      last_logged_code     = last_logged_code(),
      prompt_history       = prompt_history(),
      conv_count_rv        = conv_count_rv()
    )
    if (length(all_log_entries()) > 0) do_save_log()
    tryCatch(
      saveRDS(pause_state, PAUSE_FILE),
      error = function(e)
        showNotification(paste("Could not save pause state:", conditionMessage(e)),
                         type = "error", duration = 8)
    )
    stopApp()
  })

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
      title = "Quit Classmate?",
      tags$p("Are you sure you want to quit? Your R workspace will not be cleared."),
      tags$p(save_note),
      footer = tagList(
        modalButton("Cancel"),
        actionButton("confirm_quit", "Yes, quit", class = "btn-danger")
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
