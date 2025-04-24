#nocov start
# Create environment for global CFtime variables
CFt <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  assign("CFunits", data.frame(unit = c("years", "year", "yr", "months", "month", "mon", "days", "day", "d", "hours", "hour", "hr", "h", "minutes", "minute", "min", "seconds", "second", "sec", "s"),
                               id   = c(6L, 6L, 6L, 5L, 5L, 5L, 4L, 4L, 4L, 3L, 3L, 3L, 3L, 2L, 2L, 2L, 1L, 1L, 1L, 1L)), envir = CFt)
  assign("units", data.frame(name     = c("seconds", "minutes", "hours", "days", "months", "years"),
                             seconds  = c(1L, 60L, 3600L, 86400L, 86400L * 30L, 86400L * 365L),
                             per_day  = c(86400, 1440, 24, 1, 1/30, 1/365)), envir = CFt)
  assign("factor_periods", c("year", "season", "quarter", "month", "dekad", "day"), envir = CFt)
  assign("eps", .Machine$double.eps^0.5, envir = CFt)
}
#nocov end
