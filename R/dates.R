# Date handling for key start / expiry: detect the user's preferred order
# (day/month/year vs month/day/year vs year/month/day), parse input in any
# common form, and format dates in the preferred order.

# Returns "dmy", "mdy" or "ymd".
classmate_date_order <- function() {
  opt <- getOption("classmate.date_order")
  if (is.character(opt) && opt %in% c("dmy", "mdy", "ymd")) return(opt)
  s <- tryCatch(format(as.Date("2031-12-25"), "%x"), error = function(e) "")
  m <- gregexpr("[0-9]+", s)
  nums <- regmatches(s, m)[[1]]
  if (length(nums) != 3L) return("dmy")
  key <- vapply(nums, function(n) {
    if (n == "25") "d" else if (n == "12") "m" else if (n %in% c("31", "2031")) "y" else "?"
  }, "")
  ord <- paste(key, collapse = "")
  if (ord %in% c("dmy", "mdy", "ymd")) ord else "dmy"
}

classmate_date_pattern <- function(order = classmate_date_order()) {
  switch(order, mdy = "MM/DD/YYYY", ymd = "YYYY/MM/DD", "DD/MM/YYYY")
}

# Format a Date or POSIXct. with_time = TRUE appends HH:MM.
classmate_format_date <- function(x, order = classmate_date_order(),
                                  with_time = inherits(x, "POSIXt")) {
  if (is.null(x) || length(x) == 0L || is.na(x)) return("")
  fmt <- switch(order, mdy = "%m/%d/%Y", ymd = "%Y/%m/%d", "%d/%m/%Y")
  if (with_time) fmt <- paste(fmt, "%H:%M")
  format(x, fmt)
}

# Parse a user-typed date (optionally followed by a time such as 17:30).
# Accepts 2026-12-31, 31/12/2026, 12/31/26, 31.12.26, 31 Dec 2026, Dec 31, 2026.
# Ambiguous numeric dates (both parts <= 12) follow `order`.
# Returns a POSIXct (local time) with attr "has_time", or NA.
classmate_parse_date <- function(x, order = classmate_date_order()) {
  na <- structure(as.POSIXct(NA), has_time = FALSE)
  x <- trimws(as.character(x))
  if (length(x) != 1L || is.na(x) || !nzchar(x)) return(na)
  tm <- NULL
  tmr <- regexec("(?<![0-9])([0-9]{1,2}):([0-9]{2})(?::[0-9]{2})?\\s*([AaPp][Mm])?\\s*$", x, perl = TRUE)
  tmm <- regmatches(x, tmr)[[1]]
  if (length(tmm) > 0L) {
    h <- as.integer(tmm[2]); mi <- as.integer(tmm[3]); ap <- tolower(tmm[4])
    if (nzchar(ap)) {
      if (h < 1L || h > 12L) return(na)
      h <- (h %% 12L) + if (ap == "pm") 12L else 0L
    }
    if (h > 23L || mi > 59L) return(na)
    tm <- c(h, mi)
    x <- trimws(sub("\\s*(?<![0-9])[0-9]{1,2}:[0-9]{2}(?::[0-9]{2})?\\s*([AaPp][Mm])?\\s*$", "", x, perl = TRUE))
  }
  mons <- tolower(substr(month.name, 1, 3))
  y <- mth <- d <- NA_integer_
  if (grepl("^[0-9]{4}[-/. ][0-9]{1,2}[-/. ][0-9]{1,2}$", x)) {
    p <- as.integer(strsplit(x, "[-/. ]")[[1]]); y <- p[1]; mth <- p[2]; d <- p[3]
  } else if (grepl("^[0-9]{1,2}[-/. ][0-9]{1,2}[-/. ][0-9]{2}([0-9]{2})?$", x)) {
    p <- strsplit(x, "[-/. ]")[[1]]
    yy <- as.integer(p[3]); if (nchar(p[3]) == 2L) yy <- 2000L + yy
    a <- as.integer(p[1]); b <- as.integer(p[2]); y <- yy
    if (a > 12L && b <= 12L) { d <- a; mth <- b }
    else if (b > 12L && a <= 12L) { mth <- a; d <- b }
    else if (order == "mdy") { mth <- a; d <- b }
    else { d <- a; mth <- b }
  } else {
    w <- strsplit(gsub("[,.]", " ", tolower(x)), "[ -]+")[[1]]
    w <- w[nzchar(w)]
    mi <- which(substr(w, 1, 3) %in% mons & grepl("^[a-z]+$", w))
    if (length(mi) != 1L || length(w) != 3L) return(na)
    mth <- match(substr(w[mi], 1, 3), mons)
    nums <- suppressWarnings(as.integer(w[-mi]))
    if (anyNA(nums)) return(na)
    if (nchar(w[-mi][1]) >= 3L || nums[1] > 31L) { y <- nums[1]; d <- nums[2] }
    else { d <- nums[1]; y <- nums[2] }
    if (y < 100L) y <- 2000L + y
  }
  if (anyNA(c(y, mth, d))) return(na)
  dt <- tryCatch(as.Date(sprintf("%04d-%02d-%02d", y, mth, d)), error = function(e) as.Date(NA))
  if (is.na(dt) || as.integer(format(dt, "%d")) != d) return(na)
  if (is.null(tm)) return(structure(as.POSIXct(format(dt), tz = ""), has_time = FALSE))
  structure(as.POSIXct(sprintf("%s %02d:%02d:00", format(dt), tm[1], tm[2]), tz = ""),
            has_time = TRUE)
}

# Expiry may be a Date (valid through the end of that day) or a POSIXct.
classmate_is_expired <- function(expiry) {
  if (is.null(expiry) || length(expiry) == 0L || is.na(expiry)) return(FALSE)
  if (inherits(expiry, "POSIXt")) Sys.time() > expiry
  else Sys.Date() > as.Date(expiry)
}

classmate_not_yet_valid <- function(valid_from) {
  if (is.null(valid_from) || length(valid_from) == 0L || is.na(valid_from)) return(FALSE)
  Sys.time() < as.POSIXct(valid_from)
}
