#' Create a vector of character strings that represent CF timestamps
#'
#' This function generates a vector of character strings that represent the date
#' and/or time in a selectable combination for each offset.
#'
#' The character strings use the format `YYYY-MM-DDThh:mm:ss±hh:mm`, depending
#' on the `format` specifier. The date in the string is not necessarily
#' compatible with `POSIXt` - in the `360_day` calendar `2017-02-30` is valid
#' and `2017-03-31` is not.
#'
#' @param cf CFtime. The `CFtime` instance that contains the offsets to use.
#' @param format character. An atomic string with either of the values "date",
#' "time", "timestamp". If the argument is not specified, the format used is
#' "timestamp" if there is time information, "date" otherwise.
#'
#' @returns A character vector where each element represents a moment in time
#' according to the `format` specifier. Time zone information is not represented.
#' @export
#'
#' @examples
#' cf <- CFtime("hours since 2020-01-01", "standard", seq(0, 24, by = 0.25))
#' CFtimestamp(cf, "timestamp")
#'
#' cf2 <- CFtime("days since 2002-01-21", "standard", 0:20)
#' tail(CFtimestamp(cf2))
#'
#' tail(CFtimestamp(cf2 + 1.5))
CFtimestamp <- function(cf, format = NULL) {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to CFtimestamp must be an instance of the `CFtime` class")
  if (is.null(format)) format <- ifelse(.has_time(cf@time), "timestamp", "date")
  else if (!(format %in% c("date", "time", "timestamp"))) stop("Format specifier not recognized")

  out <- switch(format,
    "date" = sprintf("%04d-%02d-%02d", cf@time$year, cf@time$month, cf@time$day),
    "time" = .format_time(cf@time),
    "timestamp" = sprintf("%04d-%02d-%02dT%s", cf@time$year, cf@time$month, cf@time$day, .format_time(cf@time))
  )
  return(out)
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
  fsec <- t$second %% 1
  if (any(fsec > 0)) {
    paste0(sprintf("%02d:%02d:", t$hour, t$minute), ifelse(t$second < 10, "0", ""), sprintf("%.3f", t$second))
  } else {
    sprintf("%02d:%02d:%02d", t$hour, t$minute, t$second)
  }
}

#' Do the time elements have time-of-day information
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
