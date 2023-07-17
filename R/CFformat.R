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
#' x <- CFtimestamp(ts, "timestamp")
CFtimestamp <- function(cfts, format = "date") {
  if (!(methods::is(cfts, "CFts"))) stop("First argument to CFtimestamp must be an instance of the `CFts` class")
  if (!(format %in% c("date", "time", "timestamp"))) stop("Format specifier not recognized")

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
#'   \item `season`, the meteorological season of each offset is returned as
#'   "YYYY-DJF", "YYYY-MAM", "YYYY-JJA" or "YYYY-SON". Note that December dates
#'   are labeled as belonging to the subsequent year, so the date "2020-12-01"
#'   yields "2021-DJF". This implies that for standard CMIP files having one or
#'   more full years of data the first season will have data for the first two
#'   months (January and February), while the final season will have only a
#'   single month of data (December).
#'   \item `month`, the year and month of each offset are returned as "YYYY-MM".
#'   \item `dekad`, ten-day periods for each year and month are returned as
#'   "YYYYDxx", where xx runs from "01" to "36". Each month is subdivided in
#'   dekads as follows: 1- days 01 - 10; 2- days 11 - 20; 3- remainder of the
#'   month.
#' }
#'
#' It is not possible to create a factor for a period that is shorter than the
#' temporal resolution of the source dataset from which the CFts instance
#' derives. As an example, if the source dataset has monthly data, a dekad
#' factor cannot be created.
#'
#' Creating factors for other periods is not supported by this function. Factors
#' based on the timestamp information and not dependent on the calendar can
#' trivially be constructed from the output of the `CFtimestamp()` function.
#'
#' @param cfts CFts. An atomic instance of the `CFts` class whose offsets will
#' be used to construct the factor.
#' @param period character. An atomic character string with one of the values
#' "year", "season", "month" or "dekad".
#'
#' @return A factor with a length equal to the number of offsets in `cfts`.
#' @export
#'
#' @examples
#' datum <- CFdatum("days since 1949-12-01", "360_day")
#' cfts <- CFts(datum, 31:390)
#' f <- CFfactor(cfts, "month")
CFfactor <- function(cfts, period = "month") {
  period <- tolower(period)
  if (!(methods::is(cfts, "CFts"))) stop("First argument to CFfactor must be an instance of the `CFts` class")
  if (!((length(period) == 1) && (period %in% c("year", "season", "month", "dekad")))) stop("Period specifier must be an atomic value of a supported period")

  # No fine-grained period factors for coarse source data
  timestep <- CFt_units$seconds[cfts@origin@unit] * cfts@resolution;
  if ((period == "year") && (timestep > 86400 * 366) ||
      (period == "season") && (timestep > 86400 * 90) || # Somewhat arbirary
      (period == "month") && (timestep > 86400 * 31) ||
      (period == "dekad") && (timestep > 86400))         # Must be constructed from daily or finer data
    stop("Cannot produce a short period factor from source data with long time interval")

  x <- cfts@ymds
  out <- switch(period,
    "year"   = as.factor(sprintf("%04d", x[, 1])),
    "season" = mapply (function (y, m)
                 switch(m,
                   as.factor(sprintf("%04d-DJF", y)), as.factor(sprintf("%04d-DJF", y)),
                   as.factor(sprintf("%04d-MAM", y)), as.factor(sprintf("%04d-MAM", y)), as.factor(sprintf("%04d-MAM", y)),
                   as.factor(sprintf("%04d-JJA", y)), as.factor(sprintf("%04d-JJA", y)), as.factor(sprintf("%04d-JJA", y)),
                   as.factor(sprintf("%04d-SON", y)), as.factor(sprintf("%04d-SON", y)), as.factor(sprintf("%04d-SON", y)),
                   as.factor(sprintf("%04d-DJF", y + 1))
                 ), x[, 1], x[, 2]),
    "month"  = as.factor(sprintf("%04d-%02d", x[, 1], x[, 2])),
    "dekad"  = as.factor(sprintf("%04dD%02d", x[, 1], (x[, 2] - 1) * 3 + pmin((x[, 3] - 1) %/% 10 + 1, 3)))
  )

}
