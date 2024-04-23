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
#' @param format character. An atomic string with either of the values "date" or
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
  if (asPOSIX && calendar_id(cf@datum) != 1L) stop("Cannot make a POSIX timestamp on a non-standard calendar")

  time <- .offsets2time(cf@offsets, cf@datum)
  if (nrow(time) == 0L) return()

  if (is.null(format)) format <- ifelse(unit(cf@datum) < 4L || .has_time(time), "timestamp", "date")
  else if (!(format %in% c("date", "time", "timestamp"))) stop("Format specifier not recognized")

  if (asPOSIX) {
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

#' Do formatting of timestamps with format specifiers
#'
#' Internal function
#'
#' @param ts data.frame of decomposed offsets
#' @param tz character. Atomic time zone string
#' @param format character. Atomic character string with the format specifiers
#' @returns Character vector of formatted timestamps
#' @noRd
.format_format <- function(ts, tz, format) {
  # Expand any composite specifiers
  F <- stringi::stri_locate_all(format, fixed = "%F")
  if (!is.na(F[[1]][1,1])) {
    format <- stringi::stri_sub_replace_all(format, from = F, value = "%Y-%m-%d")
  }
  R <- stringi::stri_locate_all(format, fixed = "%R")
  if (!is.na(R[[1]][1,1])) {
    format <- stringi::stri_sub_replace_all(format, from = R, value = "%H:%M")
  }
  T <- stringi::stri_locate_all(format, fixed = "%T")
  if (!is.na(T[[1]][1,1])) {
    format <- stringi::stri_sub_replace_all(format, from = T, value = "%H:%M:%S")
  }

  # Splice in timestamp values for specifiers
  b <- stringi::stri_locate_all(format, regex = "%b|%h")
  if (!is.na(b[[1]][1,1])) {
    mon <- strftime(ISOdatetime(2024, 1:12, 1, 0, 0, 0), "%b")
    format <- stringi::stri_sub_replace_all(format, from = b, value = as.list(mon[ts$month]))
  }
  B <- stringi::stri_locate_all(format, fixed = "%B")
  if (!is.na(B[[1]][1,1])) {
    mon <- strftime(ISOdatetime(2024, 1:12, 1, 0, 0, 0), "%B")
    format <- stringi::stri_sub_replace_all(format, from = B, value = as.list(mon[ts$month]))
  }
  d <- stringi::stri_locate_all(format, regex = "%[O]?d")
  if (!is.na(d[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = d, value = as.list(sprintf("%02d", ts$day)))
  e <- stringi::stri_locate_all(format, fixed = "%e")
  if (!is.na(e[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = e, value = as.list(sprintf("%2d", ts$day)))
  H <- stringi::stri_locate_all(format, regex = "%[O]?H")
  if (!is.na(H[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = H, value = as.list(sprintf("%02d", ts$hour)))
  I <- stringi::stri_locate_all(format, regex = "%[O]?I")
  if (!is.na(I[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = I, value = as.list(sprintf("%02d", ts$hour %% 12)))
  # j <- stringi::stri_locate_all(format, fixed = "%j")
  # if (!is.na(j[[1]][1,1]))
  #   format <- stringi::stri_sub_replace_all(format, from = j, value = as.list(ts$year))
  m <- stringi::stri_locate_all(format, regex = "%[O]?m")
  if (!is.na(m[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = m, value = as.list(sprintf("%02d", ts$month)))
  M <- stringi::stri_locate_all(format, regex = "%[O]?M")
  if (!is.na(M[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = M, value = as.list(sprintf("%02d", ts$minute)))
  p <- stringi::stri_locate_all(format, fixed = "%p")
  if (!is.na(p[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = p, value = as.list(ifelse(ts$hour < 12, "AM", "PM")))
  S <- stringi::stri_locate_all(format, fixed = "%S")
  if (!is.na(S[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = S, value = as.list(sprintf("%02d", ts$second)))
  Y <- stringi::stri_locate_all(format, regex = "%[E]?Y")
  if (!is.na(Y[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = Y, value = as.list(ts$year))
  z <- stringi::stri_locate_all(format, fixed = "%z")
  if (!is.na(z[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = z, value = tz)
  oo <- stringi::stri_locate_all(format, fixed = "%%")
  if (!is.na(oo[[1]][1,1]))
    format <- stringi::stri_sub_replace_all(format, from = oo, value = "%")
  format
}

