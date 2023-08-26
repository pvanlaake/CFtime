# Create environment for global CFtime variables
CFt <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("calendars", c("standard", "gregorian", "proleptic_gregorian", "julian", "360_day", "365_day", "366_day", "noleap", "all_leap"), envir = CFt)
  assign("cal_ids", c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 4L, 5L), envir = CFt)
  assign("units", data.frame(unit    = c("days", "day", "d", "hours", "hour", "hr", "h", "minutes", "minute", "min", "seconds", "second", "sec", "s"),
                                    unit_id = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L)), envir = CFt)
  assign("unit_string", c("days", "hours", "minutes", "seconds"), envir = CFt)
  assign("unit_seconds", c(86400, 3600, 60, 1), envir = CFt)
}
