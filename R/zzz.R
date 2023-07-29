.onLoad <- function(libname, pkgname) {
  assign("CFt_calendars", c("standard", "gregorian", "proleptic_gregorian", "julian", "360_day", "365_day", "366_day", "no_leap", "all_leap"), envir = topenv())
  assign("CFt_cal_ids", c(1L, 1L, 1L, 2L, 3L, 4L, 5L, 4L, 5L), envir = topenv())
  assign("CFt_units", data.frame(unit    = c("days", "day", "d", "hours", "hour", "hr", "h", "minutes", "minute", "min", "seconds", "second", "sec", "s"),
                                 unit_id = c(1L, 1L, 1L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 4L, 4L, 4L, 4L),
                                 seconds = c(86400L, 86400L, 86400L, 3600L, 3600L, 3600L, 3600L, 60L, 60L, 60L, 1L, 1L, 1L, 1L)),
         envir = topenv())
  assign("CFt_unit_string", c("days", "hours", "minutes", "seconds"), envir = topenv())
}
