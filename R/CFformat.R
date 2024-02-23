#' Create a vector that represents CF timestamps
#'
#' This function generates a vector of character strings or `POSIXct`s that
#' represent the date and time in a selectable combination for each offset.
#'
#' The character strings use the format `YYYY-MM-DDThh:mm:ss±hh:mm`, depending
#' on the `format` specifier. The date in the string is not necessarily
#' compatible with `POSIXt` - in the `360_day` calendar `2017-02-30` is valid
#' and `2017-03-31` is not.
#'
#' For the "standard", "gregorian" and "proleptic_gregorian" calendars the
#' output can also be generated as a vector of `POSIXct` values by specifying
#' `asPOSIX = TRUE`.
#'
#' @param cf CFtime. The `CFtime` instance that contains the offsets to use.
#' @param format character. An atomic string with either of the values "date" or
#'   "timestamp". If the argument is not specified, the format used is
#'   "timestamp" if there is time information, "date" otherwise.
#' @param asPOSIX logical. If `TRUE`, for "standard", "gregorian" and
#'   "proleptic_gregorian" calendars the output is a vector of `POSIXct` - for
#'   other calendars the result is `NULL`. Default value is `FALSE`.
#'
#' @returns A character vector where each element represents a moment in time
#'   according to the `format` specifier. Time zone information is not
#'   represented.
#' @export
#'
#' @examples
#' cf <- CFtime("hours since 2020-01-01", "standard", seq(0, 24, by = 0.25))
#' CFtimestamp(cf, "timestamp")
#'
#' cf2 <- CFtime("days since 2002-01-21", "standard", 0:20)
#' tail(CFtimestamp(cf2, asPOSIX = TRUE))
#'
#' tail(CFtimestamp(cf2))
#'
#' tail(CFtimestamp(cf2 + 1.5))
CFtimestamp <- function(cf, format = NULL, asPOSIX = FALSE) {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to CFtimestamp must be an instance of the `CFtime` class")
  time <- .offsets2time(cf@offsets, cf@datum)
  if (nrow(time) == 0L) return()

  if (is.null(format)) format <- ifelse(unit(cf@datum) < 4L || .has_time(time), "timestamp", "date")
  else if (!(format %in% c("date", "time", "timestamp"))) stop("Format specifier not recognized")

  if (asPOSIX) {
    if (calendar_id(cf@datum) != 1L) stop("Cannot make a POSIX timestamp on a non-standard calendar")
    if (format == "date") ISOdate(time$year, time$month, time$day, 0L)
    else ISOdatetime(time$year, time$month, time$day, time$hour, time$minute, time$second, "UTC")
  } else {
    if (format == "date") sprintf("%04d-%02d-%02d", time$year, time$month, time$day)
    else sprintf("%04d-%02d-%02dT%s", time$year, time$month, time$day, .format_time(time))
  }
}

#' Formatting of time strings from time elements
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param t data.frame. A data.frame representing timestamps.
#'
#' @returns A vector of character strings with a properly formatted time. If any
#' timestamp has a fractional second part, then all time strings will report
#' seconds at milli-second precision.
#' @noRd
.format_time <- function(t) {
  fsec <- t$second %% 1L
  if (any(fsec > 0L)) {
    paste0(sprintf("%02d:%02d:", t$hour, t$minute), ifelse(t$second < 10, "0", ""), sprintf("%.3f", t$second))
  } else {
    sprintf("%02d:%02d:%02d", t$hour, t$minute, t$second)
  }
}

#' Do the time elements have time-of-day information?
#'
#' If any time information > 0, then `TRUE` otherwise `FALSE`
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param t data.frame. A data.frame representing timestamps.
#'
#' @returns `TRUE` if any timestamp has time-of-day information, `FALSE` otherwise.
#' @noRd
.has_time <- function(t) {
  any(t$hour > 0) || any(t$minute > 0) || any(t$second > 0)
}
