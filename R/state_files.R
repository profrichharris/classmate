# ---------------------------------------------------------------------------
# Where Classmate keeps the saved key settings and the usage log
#
# The unlocked key (including the API key) and the running usage total are saved in the user's R
# configuration / data folders under neutral names starting with a dot, which most file
# explorers do not list (macOS and Linux treat a leading dot as hidden; on Windows the file is also
# given the "hidden" attribute). This only makes the files less visible, it does not protect them.
# ---------------------------------------------------------------------------

CLASSMATE_CFG_FILE   <- ".cm_state"
CLASSMATE_USAGE_FILE <- ".cm_usage"

.cm_win_hidden <- function(path, hide) {
  if (.Platform$OS.type == "windows" && file.exists(path))
    try(system2("attrib", c(if (hide) "+h" else "-h", shQuote(normalizePath(path), type = "cmd")),
                stdout = FALSE, stderr = FALSE), silent = TRUE)
  invisible(NULL)
}

# Path of the saved key settings ("config") or of the usage log ("usage"). Files left by earlier
# versions under their old, visible names are moved to the new name.
classmate_state_path <- function(which = c("config", "usage")) {
  which <- match.arg(which)
  d <- tools::R_user_dir("classmate", if (which == "config") "config" else "data")
  dir.create(d, showWarnings = FALSE, recursive = TRUE)
  new <- file.path(d, if (which == "config") CLASSMATE_CFG_FILE else CLASSMATE_USAGE_FILE)
  old <- file.path(d, if (which == "config") "active_config.rds" else "usage_log.rds")
  if (file.exists(old)) {
    if (!file.exists(new)) {
      if (file.rename(old, new)) .cm_win_hidden(new, TRUE)
    } else unlink(old)
  }
  new
}

# saveRDS() that copes with the Windows hidden attribute (a hidden file cannot be overwritten)
classmate_save_state <- function(object, path) {
  .cm_win_hidden(path, FALSE)
  saveRDS(object, path)
  .cm_win_hidden(path, TRUE)
  invisible(path)
}

# ---------------------------------------------------------------------------
# The student's API key while Classmate is running
#
# The key from a student key file is held in a private object inside the package and passed to each
# request directly; it is NOT put in the ANTHROPIC_API_KEY environment variable, so
# Sys.getenv("ANTHROPIC_API_KEY") shows nothing, including in the Quick Console and after Classmate
# stops. (A user's own ANTHROPIC_API_KEY is still used when there is no student key.) The copy is
# dropped when Classmate closes, however it closes. This only removes the obvious way to read the
# key: code in the same R session can still reach package internals or the saved file.
# ---------------------------------------------------------------------------

.cm_secret <- new.env(parent = emptyenv())
cm_set_key   <- function(key) { assign("key", key, envir = .cm_secret); invisible(NULL) }
cm_clear_key <- function() {
  if (exists("key", envir = .cm_secret, inherits = FALSE)) rm("key", envir = .cm_secret)
  invisible(NULL)
}
cm_get_key <- function() {
  if (exists("key", envir = .cm_secret, inherits = FALSE)) get("key", envir = .cm_secret)
  else Sys.getenv("ANTHROPIC_API_KEY")
}
