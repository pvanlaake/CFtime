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
#' @returns A character vector where each element represents a moment in time
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

#' Formatting of time strings from time elements
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param t numeric. A vector representing the second element of a timestamp.
#'
#' @returns A character string with a properly formatted time string.
#' @noRd
.format_time <- function(t) {
  fsec <- t %% 1
  has_fsec <- which(fsec > 0)
  out <- sprintf("%02d:%02d:%02d", t %/% 3600, (t %% 3600) %/% 60, t %% 60 %/% 1)
  out[has_fsec] <- paste0(out[has_fsec], substr(as.character(fsec[has_fsec]), 2, 8))
  return(out)
}

#' Create a factor from the offsets in an CFts instance
#'
#' With this function a factor can be generated for the time series, or a part
#' thereof, contained in the `CFts` instance. This is specifically interesting
#' for creating factors from the date part of the time series that aggregate the
#' time series into longer time periods (such as month) that can then be used to
#' process daily CF data sets using, for instance, `tapply()`.
#'
#' The factor will respect the calendar of the datum that the time series is
#' built on. For `period`s longer than a day this will result in a factor where
#' the calendar is no longer relevant (because calendars impacts days, not
#' dekads, months or seasons).
#'
#' The factor will be generated in the order of the offsets of the `CFts`
#' instance. While typical CF-compliant data sources use ordered time series
#' there is, however, no guarantee that the factor is ordered as multiple `CFts`
#' objects may have been merged out of order.
#'
#' If the `epoch` parameter is specified, either as a vector of years to include
#' in the factor, or as a list of such vectors, the factor will only consider
#' those values in the time series that fall within the list of years, inclusive
#' of boundary values. Other values in the factor will be set to `NA`. The years
#' need not be contiguous, within a single vector or among the list items, or in
#' order.
#'
#' The following periods are supported by this function:
#'
#' \itemize{
#'   \item `year`, the year of each offset is returned as "YYYY".
#'   \item `season`, the meteorological season of each offset is returned as
#'   "DJF", "MAM", "JJA" or "SON", preceeded by "YYYY-" if no `epoch` is
#'   specified. Note that December dates are labeled as belonging to the
#'   subsequent year, so the date "2020-12-01" yields "2021-DJF". This implies
#'   that for standard CMIP files having one or more full years of data the
#'   first season will have data for the first two months (January and
#'   February), while the final season will have only a single month of data
#'   (December).
#'   \item `month`, the month of each offset is returned as "01" to
#'   "12", preceeded by "YYYY-" if no `epoch` is specified. This is the default
#'   period.
#'   \item `dekad`, ten-day periods are returned as
#'   "Dxx", where xx runs from "01" to "36", preceeded by "YYYY" if no `epoch`
#'   is specified. Each month is subdivided in dekads as follows: 1- days 01 -
#'   10; 2- days 11 - 20; 3- remainder of the month.
#'   \item `day`, the month and day of each offset are returned as "MM-DD",
#'   preceeded by "YYYY-" if no `epoch` is specified.
#' }
#'
#' It is not possible to create a factor for a period that is shorter than the
#' temporal resolution of the source data set from which the `cfts` argument
#' derives. As an example, if the source data set has monthly data, a dekad or
#' day factor cannot be created.
#'
#' Creating factors for other periods is not supported by this function. Factors
#' based on the timestamp information and not dependent on the calendar can
#' trivially be constructed from the output of the `CFtimestamp()` function.
#'
#' @param cfts CFts. An atomic instance of the `CFts` class whose offsets will
#'   be used to construct the factor.
#' @param period character. An atomic character string with one of the values
#'   "year", "season", "month" (the default), "dekad" or "day".
#' @param epoch vector or list, optional. Vector of years for which to construct
#'   the factor, or a list whose elements are each a vector of years. If `epoch`
#'   is not specified, the factor will use the entire time series for the
#'   factor.
#'
#' @returns If `epoch` is a single vector or not specified, a factor with a
#'   length equal to the number of offsets in `cfts`. If `epoch` is a list, a
#'   list with the same number of elements and names as `epoch`, each containing
#'   a factor. Elements in the factor will be set to `NA` for time series values
#'   outside of the range of specified years.
#' @export
#'
#' @examples
#' datum <- CFdatum("days since 1949-12-01", "360_day")
#' cfts <- CFts(datum, 19830:54029)
#'
#' # Create a dekad factor for the whole time series
#' f <- CFfactor(cfts, "dekad")
#'
#' # Create four monthly factors for a baseline epoch and early, mid and late 21st century
#' ep <- CFfactor(cfts, epoch = list(baseline = 1991:2020, early = 2021:2040,
#'                                   mid = 2041:2060, late = 2061:2080))
CFfactor <- function(cfts, period = "month", epoch = NULL) {
  if (!(methods::is(cfts, "CFts"))) stop("First argument to CFfactor must be an instance of the `CFts` class")

  period <- tolower(period)
  if (!((length(period) == 1) && (period %in% c("year", "season", "month", "dekad", "day")))) stop("Period specifier must be an atomic value of a supported period")

  # No fine-grained period factors for coarse source data
  timestep <- CFt_units$seconds[cfts@origin@unit] * cfts@resolution;
  if ((period == "year") && (timestep > 86400 * 366) ||
      (period == "season") && (timestep > 86400 * 90) || # Somewhat arbitrary
      (period == "month") && (timestep > 86400 * 31) ||
      (period == "dekad") && (timestep > 86400) ||       # Must be constructed from daily or finer data
      (period == "day") && (timestep > 86400))           # Must be no longer than a day
    stop("Cannot produce a short period factor from source data with long time interval")

  yr <- cfts@ymds[, 1]
  mon <- cfts@ymds[, 2]
  if (period %in% c("dekad", "day")) day <- cfts@ymds[, 3]
  seasons <- c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  if (is.null(epoch)) {
    f <- switch(period,
      "year"   = sprintf("%04d", yr),
      "season" = sprintf("%04d-%s", yr, seasons[mon]),
      "month"  = sprintf("%04d-%s", yr, months[mon]),
      "dekad"  = sprintf("%04dD%02d", yr, (mon - 1) * 3 + pmin.int((day - 1) %/% 10 + 1, 3)),
      "day"    = sprintf("%04d-%02d-%02d", yr, mon, day)
    )
    out <- factor(f)
  } else {
    if (is.numeric(epoch)) ep <- list(epoch)
    else if ((is.list(epoch) && all(unlist(lapply(epoch, is.numeric))))) ep <- epoch
    else stop("when specified, the `epoch` parameter must be a numeric vector or a list thereof")

    out <- lapply(ep, function(years) {
      f <- switch(period,
             "year"   = ifelse(yr %in% years, sprintf("%04d", yr), NA_character_),
             "season" = ifelse(yr %in% years, seasons[mon], NA_character_),
             "month"  = ifelse(yr %in% years, months[mon], NA_character_),
             "dekad"  = ifelse(yr %in% years, sprintf("D%02d", (mon - 1) * 3 + pmin.int((day - 1) %/% 10 + 1, 3)), NA_character_),
             "day"    = ifelse(yr %in% years, sprintf("%s-%02d", months[mon], day), NA_character_)
           )
      f <- as.factor(f)
    })
    if (is.numeric(epoch)) out <- unlist(out)
    else names(out) <- names(epoch)
  }
  return(out)
}
