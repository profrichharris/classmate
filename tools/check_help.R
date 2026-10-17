# Checks that Help mode covers every button and tab in the app.
#
# Run before every release, and whenever a button, tab or label is added,
# renamed or removed:
#
#   Rscript tools/check_help.R          # from the package root
#
# It parses inst/app/app.R (without launching the app), collects every button
# and tab the code defines, and compares them with HELP_TEXTS, HELP_TAB_TEXTS,
# HELP_EXEMPT and HELP_ID_PATTERNS. Buttons that exist only inside pop-up
# windows or on the key-loading startup screen are ignored, because the Help
# overlay is switched off there. Exits with status 1 if anything is missing.

app_file <- Sys.getenv("HELP_APP_FILE", "inst/app/app.R")
if (!file.exists(app_file)) stop("Run this from the package root (inst/app/app.R not found).")
exprs <- parse(app_file, keep.source = FALSE)

# --- Load the help tables without running the rest of the app ---------------
help_env <- new.env()
wanted <- c("HELP_TEXTS", "HELP_TAB_TEXTS", "HELP_EXEMPT", "HELP_ID_PATTERNS")
for (e in exprs) {
  if (is.call(e) && identical(e[[1]], as.name("<-")) && is.name(e[[2]]) &&
      as.character(e[[2]]) %in% wanted)
    eval(e, help_env)
}
missing_tables <- setdiff(wanted, ls(help_env))
if (length(missing_tables) > 0)
  stop("Help tables not found in app.R: ", paste(missing_tables, collapse = ", "))

# --- Walk the code collecting buttons and tabs ------------------------------
button_fns <- c("actionButton", "shinyFilesButton", "shinySaveButton",
                "shinyDirButton", "downloadButton", "actionLink", "checkboxInput")
found <- list()   # each: kind, id, hidden (TRUE = help overlay not active there)

fn_name <- function(call) {
  f <- call[[1]]
  if (is.name(f)) return(as.character(f))
  if (is.call(f) && length(f) == 3 && as.character(f[[1]]) %in% c("$", "::"))
    return(paste(as.character(f[[2]]), as.character(f[[3]]), sep = as.character(f[[1]])))
  ""
}

walk <- function(x, hidden = FALSE) {
  if (!is.call(x)) {
    if (is.pairlist(x) || is.expression(x) || is.list(x))
      for (i in seq_along(x)) {
        if (identical(x[[i]], quote(expr = ))) next   # empty default, e.g. function(x, y)
        walk(x[[i]], hidden)
      }
    return(invisible())
  }
  nm <- fn_name(x)
  here_hidden <- hidden
  if (nm == "modalDialog") here_hidden <- TRUE
  if (nm == "conditionalPanel") {
    cond <- tryCatch(x$condition, error = function(e) NULL)
    if (is.character(cond) && grepl("has_key == false", cond, fixed = TRUE)) here_hidden <- TRUE
  }
  if (nm %in% button_fns && length(x) >= 2) {
    id <- if (!is.null(names(x)) && "inputId" %in% names(x)) x[["inputId"]] else x[[2]]
    found[[length(found) + 1]] <<- list(
      kind = "button", id = if (is.character(id)) id else NA_character_,
      expr = paste(deparse(id), collapse = ""), hidden = here_hidden)
  }
  if (nm == "tags$button" && !is.null(names(x)) && "id" %in% names(x)) {
    id <- x[["id"]]
    found[[length(found) + 1]] <<- list(
      kind = "button", id = if (is.character(id)) id else NA_character_,
      expr = paste(deparse(id), collapse = ""), hidden = here_hidden)
  }
  if (!is.null(names(x)) && "data-help-id" %in% names(x) && is.character(x[["data-help-id"]])) {
    found[[length(found) + 1]] <<- list(
      kind = "button", id = x[["data-help-id"]],
      expr = paste(deparse(x[["data-help-id"]]), collapse = ""), hidden = here_hidden)
  }
  if (nm == "tabPanel" && length(x) >= 2) {
    ttl <- x[[2]]
    if (!is.null(names(x)) && "title" %in% names(x)) ttl <- x[["title"]]
    found[[length(found) + 1]] <<- list(
      kind = "tab", id = if (is.character(ttl)) ttl else NA_character_,
      expr = paste(deparse(ttl), collapse = ""), hidden = here_hidden)
  }
  for (i in seq_along(x)) {
    if (identical(x[[i]], quote(expr = ))) next   # empty argument, e.g. x[, 1]
    if (!is.null(x[[i]])) walk(x[[i]], here_hidden)
  }
  invisible()
}
for (e in exprs) walk(e)

df <- do.call(rbind, lapply(found, function(f)
  data.frame(kind = f$kind, id = f$id, expr = f$expr, hidden = f$hidden,
             stringsAsFactors = FALSE)))

literal <- df[!is.na(df$id), ]
dynamic <- df[is.na(df$id), ]

# A button/tab counts as "main screen" if it appears there at least once
# outside a pop-up window / startup screen.
main <- unique(literal[!literal$hidden, c("kind", "id")])
all_ids <- unique(literal[, c("kind", "id")])

covered_button <- function(id)
  id %in% names(help_env$HELP_TEXTS) || id %in% help_env$HELP_EXEMPT ||
    any(vapply(names(help_env$HELP_ID_PATTERNS), function(p) grepl(p, id), logical(1)))
covered_tab <- function(id) id %in% names(help_env$HELP_TAB_TEXTS)

problems <- 0L
say <- function(...) cat(..., "\n", sep = "")

say("Help mode check")
say("---------------")

miss_b <- main$id[main$kind == "button" & !vapply(main$id, covered_button, logical(1))]
if (length(miss_b)) {
  problems <- problems + length(miss_b)
  say("MISSING help text for buttons on the main screen:")
  for (i in miss_b) say("  - ", i)
} else say("OK: every main-screen button has help text.")

miss_t <- main$id[main$kind == "tab" & !vapply(main$id, covered_tab, logical(1))]
if (length(miss_t)) {
  problems <- problems + length(miss_t)
  say("MISSING help text for tabs:")
  for (i in miss_t) say("  - ", i)
} else say("OK: every main-screen tab has help text.")

# Stale entries: help written for an id/tab that no longer exists anywhere
stale_b <- setdiff(names(help_env$HELP_TEXTS), all_ids$id[all_ids$kind == "button"])
stale_b <- stale_b[!stale_b %in% c(dynamic$expr)]
if (length(stale_b)) {
  problems <- problems + length(stale_b)
  say("STALE help entries (no such button in the code):")
  for (i in stale_b) say("  - ", i)
}
stale_t <- setdiff(names(help_env$HELP_TAB_TEXTS), all_ids$id[all_ids$kind == "tab"])
# Sub-tabs and dynamically inserted tabs are built from computed titles
stale_t <- stale_t[!stale_t %in% c("Plots", "Results", "Warnings", "Changes", "Past conversations",
                                    "Past code")]
if (length(stale_t)) {
  problems <- problems + length(stale_t)
  say("STALE tab help entries (no such tab in the code):")
  for (i in stale_t) say("  - ", i)
}

# Computed ids already reviewed: web_key_ui()'s button, used only on the startup
# screen and inside the Load key file pop-up.
dynamic <- dynamic[!dynamic$expr %in% c("btn_id"), ]
# A computed id such as paste0("recall_prompt_", i) is covered if its literal prefix matches
# one of the HELP_ID_PATTERNS.
covered_by_pattern <- vapply(dynamic$expr, function(e) {
  m <- regmatches(e, regexec('^paste0\\("([^"]+)"', e))[[1]]
  length(m) == 2 && any(vapply(names(help_env$HELP_ID_PATTERNS), function(p) grepl(p, m[2]), logical(1)))
}, logical(1))
dynamic <- dynamic[!covered_by_pattern, ]
if (nrow(dynamic) > 0) {
  say("\nNote: ", nrow(dynamic), " button/tab(s) have computed ids/titles and cannot be",
      " checked automatically. Confirm they are covered by HELP_TEXTS or HELP_ID_PATTERNS:")
  for (e in unique(dynamic$expr)) say("  ? ", e)
}

if (problems > 0) {
  say("\n", problems, " problem(s) found — update HELP_TEXTS / HELP_TAB_TEXTS in inst/app/app.R.")
  quit(status = 1)
}
say("\nAll good.")
