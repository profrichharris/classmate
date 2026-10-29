# Checks the datasets offered under "Klausmate (online datasets)" in Load Package Data.
#
#   Rscript tools/check_datasets.R            # check (run before every release that touches datasets/)
#   Rscript tools/check_datasets.R --update   # refresh the sizes and checksums in datasets/datasets.json first
#
# Fails (exit status 1) if a dataset lacks its source, retrieval date, licence, licence link or attribution,
# if its licence is not on the approved list, if its file is missing or its size / checksum is out of date,
# or if a file in datasets/ is not listed. This checks the paperwork only: whether a licence really allows
# a dataset to be redistributed is a decision for a person, recorded in the "reviewed" field.

ALLOWED_LICENCES <- c(
  "public-domain",   # works of the US federal government, CC0 and similar
  "cc0",
  "cc-by",           # attribution required
  "odbl",            # attribution and share-alike required: put the wording in "attribution" and "notice"
  "ogl",             # UK Open Government Licence v3.0: attribution required
  "free-with-credit" # published free to download with no formal licence: credit the source in "attribution" and say so in "notice"
)
REQUIRED <- c("id", "title", "description", "file", "format", "object", "source", "retrieved", "licence",
              "licence_id", "licence_url", "attribution", "reviewed")

dir  <- "datasets"
path <- file.path(dir, "datasets.json")
if (!file.exists(path)) stop("Run this from the package root (datasets/datasets.json not found).")
m <- jsonlite::fromJSON(path, simplifyVector = FALSE)
update <- "--update" %in% commandArgs(trailingOnly = TRUE)
sha <- function(f) unclass(as.character(openssl::sha256(file(f))))
problems <- character(0)
for (i in seq_along(m$datasets)) {
  d <- m$datasets[[i]]; id <- d$id %||% paste0("#", i)
  `%||%` <- function(a, b) if (is.null(a) || identical(a, "")) b else a
  for (f in REQUIRED)
    if (is.null(d[[f]]) || !nzchar(trimws(as.character(d[[f]]))))
      problems <- c(problems, sprintf("%s: '%s' is missing", id, f))
  if (!is.null(d$licence_id) && !(d$licence_id %in% ALLOWED_LICENCES))
    problems <- c(problems, sprintf("%s: licence_id '%s' is not on the approved list (%s)", id, d$licence_id,
                                    paste(ALLOWED_LICENCES, collapse = ", ")))
  if (identical(d$licence_id, "odbl") && !nzchar(d$notice %||% ""))
    problems <- c(problems, sprintf("%s: an ODbL dataset needs its share-alike wording in 'notice'", id))
  if (identical(d$licence_id, "free-with-credit") && !nzchar(d$notice %||% ""))
    problems <- c(problems, sprintf("%s: a free-with-credit dataset needs a 'notice' saying no formal licence is stated", id))
  f <- file.path(dir, d$file %||% "")
  if (!file.exists(f)) { problems <- c(problems, sprintf("%s: file '%s' not found", id, d$file)); next }
  if (update) { m$datasets[[i]]$bytes <- file.size(f); m$datasets[[i]]$sha256 <- sha(f) }
  else {
    if (!identical(as.numeric(d$bytes), as.numeric(file.size(f)))) problems <- c(problems, sprintf("%s: size is out of date", id))
    if (!identical(d$sha256, sha(f))) problems <- c(problems, sprintf("%s: checksum is out of date", id))
  }
}
listed <- vapply(m$datasets, function(d) d$file %||% "", "")
unlisted <- setdiff(list.files(dir, pattern = "\\.(gpkg|geojson|csv|zip|shp|dbf|shx|prj)$"), listed)
if (length(unlisted) > 0) problems <- c(problems, paste0("file not listed in datasets.json: ", unlisted))
if (update) {
  writeLines(jsonlite::toJSON(m, pretty = TRUE, auto_unbox = TRUE), path)
  cat("Updated sizes and checksums in", path, "\n")
}
if (length(problems) > 0) { cat("FAIL:\n", paste0("  - ", problems, collapse = "\n"), "\n", sep = ""); quit(status = 1) }
cat("OK:", length(m$datasets), "dataset(s) have complete licence and attribution details and current checksums.\n")
