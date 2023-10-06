# Create environment for global CFtime variables
CFt <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("calendars", data.frame(name = c("standard", "gregorian", "proleptic_gregorian", "julian", "360_day", "365_day", "366_day", "noleap", "all_leap"),
                                 id   = c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 4L, 5L)), envir = CFt)
  assign("CFunits", data.frame(unit = c("days", "day", "d", "hours", "hour", "hr", "h", "minutes", "minute", "min", "seconds", "second", "sec", "s"),
                               id   = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L)), envir = CFt)
  assign("units", data.frame(name     = c("days", "hours", "minutes", "seconds"),
                             seconds  = c(86400, 3600, 60, 1),
                             per_day  = c(1, 24, 1440, 86400)), envir = CFt)
  assign("factor_periods", c("year", "season", "month", "dekad", "day"), envir = CFt)
}
