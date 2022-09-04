#' Create a vector of character strings that represent CF timestamps
#'
#' The CFts instance typically contains a vector of offsets from an origin. This
#' function generates a vector of character strings that represent the date
#' and/or time in a selectable combination for each offset.
#'
#' The character strings use the format `YYYY-MM-DDThh:mm:ss±hh:mm`, depending
#' on the `format` specifier. The date in the string is not necessarily
#' compatible with `POSIXt` - in the `360_day` calendar `2017-02-30` is valid
#' and `2017-03-31` is not.
#'
#' @param cfts CFts. The CFts instance that contains the offsets to use.
#' @param format character. An atomic string with either of the values "date",
#' "time", "timestamp".
#'
#' @return A character vector where each element represents a moment in time
#' according to the `format` specifier.
#' @export
#'
#' @examples
#' datum <- CFdatum("hours since 2020-01-01")
#' ts <- CFts(datum, seq(0, 24, by = 0.25))
#' x <- as_timestamp(ts, "timestamp")
as_timestamp <- function(cfts, format = "date") {
  stopifnot(methods::is(cfts, "CFts"), format %in% c("date", "time", "timestamp"))
  x <- cfts@ymds
  out <- switch(format,
    "date" = sprintf("%04d-%02d-%02d", x[, 1], x[, 2], x[, 3]),
    "time" = .format_time(x[, 4]),
    "timestamp" = sprintf("%04d-%02d-%02dT%s%s", x[, 1], x[, 2], x[, 3], .format_time(x[, 4]), cfts@origin@tz)
  )
  return(out)
}

# Formatting of time strings from time elements
#
# This is an internal function that should not generally be used outside of
# the CFtime package.
#
# @param t numeric. A vector representing the second element of a timestamp.
#
# @return A character string with a properly formatted time string.
.format_time <- function(t) {
  fsec <- t %% 1
  has_fsec <- which(fsec > 0)
  out <- sprintf("%02d:%02d:%02d", t %/% 3600, (t %% 3600) %/% 60, t %% 60 %/% 1)
  out[has_fsec] <- paste0(out[has_fsec], substr(as.character(fsec[has_fsec]), 2, 8))
  return(out)
}

#' Create a factor from the offsets in an CFts instance
#'
#' With this function a factor can be generated for the time series contained in
#' the `CFts` instance. This is specifically interesting for creating factors
#' from the date part of the time series that aggregate the time series into
#' longer time periods (such as month) that can then be used to process daily CF
#' data sets using, for instance, `tapply()`. The factor will respect the
#' calendar of the datum that the time series is built on. For `period`s larger
#' than a day this will result in a factor where the calendar is no longer
#' relevant (because calendars impacts days, not weeks or months).
#'
#' The factor will be generated in the order of the offsets of the `CFts`
#' instance. While typical CF-compliant data sources use ordered time series
#' there is, however, no guarantee that the factor is ordered as
#' multiple `CFts` objects may have been merged out of order.
#'
#' The following periods are supported by this function:
#'
#' \itemize{
#'   \item `year`, the year of each offset is returned as "YYYY".
#'   \item `month`, the year and month of each offset are returned as "YYYY-MM".
#'   \item `dekad`, ten-day periods for each year and month are returned as
#'   "YYYYDxx", where xx runs from "01" to "36". Each month is subdivided in
#'   dekads as follows: 1- days 01 - 10; 2- days 11 - 20; 3- remainder of the
#'   month.
#' }
#'
#' Creating factors for other periods is not supported by this function. Factors
#' based on the timestamp information and not dependent on the calendar can
#' trivially be constructed from the output of the `as_timestamp()` function.
#'
#' @param cfts CFts. An atomic instance of the `CFts` class whose offsets will
#' be used to construct the factor.
#' @param period character. An atomic character string with one of the values
#' "year", "month" or "dekad".
#'
#' @return A factor with a length equal to the number of offsets in `cfts`.
#' @export
#'
#' @examples
#' datum <- CFdatum("days since 1949-12-01", "360_day")
#' cfts <- CFts(datum, 31:390)
#' f <- as_factor(cfts, "month")
as_factor <- function(cfts, period = "month") {
  stopifnot(methods::is(cfts, "CFts"), length(period) == 1, period %in% c("year", "month", "dekad"))
  x <- cfts@ymds
  out <- switch(period,
    "year"  = as.factor(sprintf("%04d", x[, 1])),
    "month" = as.factor(sprintf("%04d-%02d", x[, 1], x[, 2])),
    "dekad" = as.factor(sprintf("%04dD%02d", x[, 1], (x[, 2] - 1) * 3 + pmin((x[, 3] - 1) %/% 10 + 1, 3)))
  )

}
