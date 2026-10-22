# Builds datasets/childhood_lottery.gpkg (+ childhood_lottery_metrics.csv) from the Children's Commissioner
# "Childhood Lottery" workbook and the ONS Counties and Unitary Authorities (December 2025, BGC) boundaries.
#   Rscript tools/prepare_childhood_lottery.R <lottery.xlsx> <utla.geojson>
args <- commandArgs(trailingOnly = TRUE)
library(readxl); library(sf)
d <- suppressMessages(read_excel(args[1], sheet = "Full dataset", .name_repair = "minimal"))
names(d) <- c("la", "code", "region", "rural_urban", "pillar", "metric", "description", "source", "scored",
              "value_type", "value", "score", "rank", "regional_range", "england_average",
              "n_missing", "pct_missing", "notes")
d$value_num <- suppressWarnings(as.numeric(d$value))
d$value_num[d$value %in% "Yes"] <- 1; d$value_num[d$value %in% "No"] <- 0
d$score_num <- suppressWarnings(as.numeric(d$score))
prefix <- c("Better world" = "world", "Care" = "care", "Community" = "comm", "Education, jobs & skills" = "edu",
            "Family" = "family", "Health" = "health", "Overall total score" = "overall")
slug <- function(x) {
  x <- tolower(gsub("[^A-Za-z0-9]+", "_", gsub("['’]", "", x)))
  x <- gsub("^_|_$", "", x)
  x <- gsub("\\b(the|a|an|of|to|in|for|and|their|with|that)\\b_?", "", x)
  x <- gsub("_+", "_", gsub("^_|_$", "", x))
  vapply(x, function(s) {
    if (nchar(s) > 44) s <- sub("_[^_]*$", "", substr(s, 1, 45))
    gsub("(_(or|in|of|by|at|on|is|are|from|per))+$", "", s)
  }, "", USE.NAMES = FALSE)
}
m <- unique(d[, c("pillar", "metric", "description", "source", "scored", "value_type", "england_average")])
m$stem <- ifelse(m$pillar == "Overall total score", "overall", paste0(prefix[m$pillar], "_", slug(m$metric)))
m$stem <- make.unique(m$stem, sep = "_")
notes <- tapply(d$notes, d$metric, function(x) paste(unique(x[!is.na(x)]), collapse = " | "))
m$notes <- unname(notes[m$metric]); m$notes[m$notes %in% c("NA", "")] <- NA
m$england_average <- vapply(m$england_average, function(v) { n <- suppressWarnings(as.numeric(v)); if (is.na(n)) as.character(v) else format(signif(n, 6)) }, "")
d$stem <- m$stem[match(d$metric, m$metric)]
la <- unique(d[, c("code", "la", "region", "rural_urban", "n_missing", "pct_missing")])
wide <- la
for (st in m$stem) {
  r <- d[d$stem == st, ]; i <- match(wide$code, r$code)
  wide[[paste0(st, "_value")]] <- r$value_num[i]
  if (any(!is.na(r$score_num))) wide[[paste0(st, "_score")]] <- r$score_num[i]
}
names(wide)[1:6] <- c("code", "name", "region", "rural_urban", "n_missing_metrics", "pct_missing_metrics")
b <- st_read(args[2], quiet = TRUE)
names(b)[names(b) == "CTYUA25CD"] <- "code"
stopifnot(all(wide$code %in% b$code), nrow(b) == nrow(wide))
out <- merge(b[, "code"], wide, by = "code")
out <- st_transform(st_simplify(st_transform(out, 27700), dTolerance = 100, preserveTopology = TRUE), 27700)
out <- st_make_valid(out)
dir.create("datasets", showWarnings = FALSE)
unlink("datasets/childhood_lottery.gpkg")
st_write(out, "datasets/childhood_lottery.gpkg", layer = "childhood_lottery", quiet = TRUE)
m$value_col <- paste0(m$stem, "_value")
m$score_col <- ifelse(paste0(m$stem, "_score") %in% names(wide), paste0(m$stem, "_score"), NA)
write.csv(m[, c("pillar", "metric", "value_col", "score_col", "description", "value_type", "scored", "source",
                "england_average", "notes")], "datasets/childhood_lottery_metrics.csv", row.names = FALSE, na = "")
cat("rows:", nrow(out), " columns:", ncol(out), "\n")
