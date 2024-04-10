#nocov start
# Create environment for global CFtime variables
CFt <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("calendars", data.frame(name = c("standard", "gregorian", "proleptic_gregorian", "julian", "360_day", "365_day", "366_day", "noleap", "all_leap"),
                                 id   = c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 4L, 5L)), envir = CFt)
  assign("CFunits", data.frame(unit = c("years", "year", "yr", "months", "month", "mon", "days", "day", "d", "hours", "hour", "hr", "h", "minutes", "minute", "min", "seconds", "second", "sec", "s"),
                               id   = c(6L, 6L, 6L, 5L, 5L, 5L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 1L)), envir = CFt)
  assign("units", data.frame(name     = c("seconds", "minutes", "hours", "days", "months", "years"),
                             seconds  = c(1, 60, 3600, 86400, 86400 * 30, 86400 * 365),
                             per_day  = c(86400, 1440, 24, 1, 1/30, 1/365)), envir = CFt)
  assign("factor_periods", c("year", "season", "quarter", "month", "dekad", "day"), envir = CFt)
}
#nocov end
