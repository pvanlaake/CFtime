#' Create a vector that represents CF timestamps
#'
#' This function generates a vector of character strings or `POSIXct`s that
#' represent the date and time in a selectable combination for each offset.
#'
#' The character strings use the format `YYYY-MM-DDThh:mm:ss±hhmm`, depending on
#' the `format` specifier. The date in the string is not necessarily compatible
#' with `POSIXt` - in the `360_day` calendar `2017-02-30` is valid and
#' `2017-03-31` is not.
#'
#' For the "standard", "gregorian" and "proleptic_gregorian" calendars the
#' output can also be generated as a vector of `POSIXct` values by specifying
#' `asPOSIX = TRUE`.
#'
#' @param cf CFtime. The `CFtime` instance that contains the offsets to use.
#' @param format character. A character string with either of the values "date" or
#'   "timestamp". If the argument is not specified, the format used is
#'   "timestamp" if there is time information, "date" otherwise.
#' @param asPOSIX logical. If `TRUE`, for "standard", "gregorian" and
#'   "proleptic_gregorian" calendars the output is a vector of `POSIXct` - for
#'   other calendars the result is `NULL`. Default value is `FALSE`.
#'
#' @seealso The [CFtime::format()] function gives greater flexibility through
#'   the use of strptime-like format specifiers.
#' @returns A character vector where each element represents a moment in time
#'   according to the `format` specifier.
#' @export
#'
#' @examples
#' cf <- CFtime("hours since 2020-01-01", "standard", seq(0, 24, by = 0.25))
#' as_timestamp(cf, "timestamp")
#'
#' cf2 <- CFtime("days since 2002-01-21", "standard", 0:20)
#' tail(as_timestamp(cf2, asPOSIX = TRUE))
#'
#' tail(as_timestamp(cf2))
#'
#' tail(as_timestamp(cf2 + 1.5))
as_timestamp <- function(cf, format = NULL, asPOSIX = FALSE) {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to `as.timestamp()` must be an instance of the `CFtime` class")
  if (asPOSIX && cf@datum@cal_id != 1L) stop("Cannot make a POSIX timestamp on a non-standard calendar")

  time <- .offsets2time(cf@offsets, cf@datum)
  if (nrow(time) == 0L) return()

  if (is.null(format)) format <- ifelse(cf@datum@unit < 4L || .has_time(time), "timestamp", "date")
  else if (!(format %in% c("date", "timestamp"))) stop("Format specifier not recognized")

  if (asPOSIX) {
    if (format == "date") ISOdate(time$year, time$month, time$day, 0L)
    else ISOdatetime(time$year, time$month, time$day, time$hour, time$minute, time$second, "UTC")
  } else .format_format(time, timezone(cf), format)
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

#' Do formatting of timestamps with format specifiers
#'
#' Internal function
#'
#' @param ts data.frame of decomposed offsets.
#' @param tz character. Time zone character string.
#' @param format character. A character string with the format specifiers, or
#' "date" or "timestamp".
#' @returns Character vector of formatted timestamps.
#' @noRd
.format_format <- function(ts, tz, format) {
  if (format == "") format <- "timestamp"
  if (format == "timestamp" && sum(ts$hour, ts$minute, ts$second) == 0)
    format <- "date"

  if (format == "date") return(sprintf("%04d-%02d-%02d", ts$year, ts$month, ts$day))
  else if (format == "timestamp") return(sprintf("%04d-%02d-%02d %s", ts$year, ts$month, ts$day, .format_time(ts)))

  # Expand any composite specifiers
  format <- stringr::str_replace_all(format, c("%F" = "%Y-%m-%d", "%R" = "%H:%M", "%T" = "%H:%M:%S"))

  # Splice in timestamp values for specifiers
  # nocov start
  if (grepl("%b|%h", format[1])) {
    mon <- strftime(ISOdatetime(2024, 1:12, 1, 0, 0, 0), "%b")
    format <- stringr::str_replace_all(format, "%b|%h", mon[ts$month])
  }
  if (grepl("%B", format[1])) {
    mon <- strftime(ISOdatetime(2024, 1:12, 1, 0, 0, 0), "%B")
    format <- stringr::str_replace_all(format, "%B", mon[ts$month])
  }
  # nocov end
  format <- stringr::str_replace_all(format, "%[O]?d", sprintf("%02d", ts$day))
  format <- stringr::str_replace_all(format, "%e", sprintf("%2d", ts$day))
  format <- stringr::str_replace_all(format, "%[O]?H", sprintf("%02d", ts$hour))
  format <- stringr::str_replace_all(format, "%[O]?I", sprintf("%02d", ts$hour %% 12))
  # "%j" = ???
  format <- stringr::str_replace_all(format, "%[O]?m", sprintf("%02d", ts$month))
  format <- stringr::str_replace_all(format, "%[O]?M", sprintf("%02d", ts$minute))
  format <- stringr::str_replace_all(format, "%p", ifelse(ts$hour < 12, "AM", "PM"))
  format <- stringr::str_replace_all(format, "%S", sprintf("%02d", as.integer(ts$second)))
  format <- stringr::str_replace_all(format, "%[E]?Y", sprintf("%04d", ts$year))
  format <- stringr::str_replace_all(format, "%z", tz)
  format <- stringr::str_replace_all(format, "%%", "%")
  format
}

