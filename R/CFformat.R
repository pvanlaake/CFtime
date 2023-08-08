#' Create a vector of character strings that represent CF timestamps
#'
#' The `CFtime` instance contains a vector of offsets from an origin. This
#' function generates a vector of character strings that represent the date
#' and/or time in a selectable combination for each offset.
#'
#' The character strings use the format `YYYY-MM-DDThh:mm:ss±hh:mm`, depending
#' on the `format` specifier. The date in the string is not necessarily
#' compatible with `POSIXt` - in the `360_day` calendar `2017-02-30` is valid
#' and `2017-03-31` is not.
#'
#' @param cf CFtime. The `CFtime` instance that contains the offsets to use.
#' @param format character. An atomic string with either of the values "date",
#' "time", "timestamp".
#'
#' @returns A character vector where each element represents a moment in time
#' according to the `format` specifier. Time zone information is not represented.
#' @export
#'
#' @examples
#' cf <- CFtime("hours since 2020-01-01", "standard", seq(0, 24, by = 0.25))
#' CFtimestamp(cf, "timestamp")
CFtimestamp <- function(cf, format = "date") {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to CFtimestamp must be an instance of the `CFtime` class")
  if (!(format %in% c("date", "time", "timestamp"))) stop("Format specifier not recognized")

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

#' Create a factor from the offsets in an CFtime instance
#'
#' With this function a factor can be generated for the time series, or a part
#' thereof, contained in the `CFtime` instance. This is specifically interesting
#' for creating factors from the date part of the time series that aggregate the
#' time series into longer time periods (such as month) that can then be used to
#' process daily CF data sets using, for instance, `tapply()`.
#'
#' The factor will respect the calendar of the datum that the time series is
#' built on. For `period`s longer than a day this will result in a factor where
#' the calendar is no longer relevant (because calendars impacts days, not
#' dekads, months or seasons).
#'
#' The factor will be generated in the order of the offsets of the `CFtime`
#' instance. While typical CF-compliant data sources use ordered time series
#' there is, however, no guarantee that the factor is ordered as multiple `CFtime`
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
#' temporal resolution of the source data set from which the `cf` argument
#' derives. As an example, if the source data set has monthly data, a dekad or
#' day factor cannot be created.
#'
#' Creating factors for other periods is not supported by this function. Factors
#' based on the timestamp information and not dependent on the calendar can
#' trivially be constructed from the output of the `CFtimestamp()` function.
#'
#' @param cf CFtime. An atomic instance of the `CFtime` class whose offsets will
#'   be used to construct the factor.
#' @param period character. An atomic character string with one of the values
#'   "year", "season", "month" (the default), "dekad" or "day".
#' @param epoch vector or list, optional. Vector of years for which to construct
#'   the factor, or a list whose elements are each a vector of years. If `epoch`
#'   is not specified, the factor will use the entire time series for the
#'   factor.
#'
#' @returns If `epoch` is a single vector or not specified, a factor with a
#'   length equal to the number of offsets in `cf`. If `epoch` is a list, a
#'   list with the same number of elements and names as `epoch`, each containing
#'   a factor. Elements in the factor will be set to `NA` for time series values
#'   outside of the range of specified years.
#' @export
#'
#' @examples
#' cf <- CFtime("days since 1949-12-01", "360_day", 19830:54029)
#'
#' # Create a dekad factor for the whole time series
#' f <- CFfactor(cf, "dekad")
#'
#' # Create four monthly factors for a baseline epoch and early, mid and late 21st century epochs
#' ep <- CFfactor(cf, epoch = list(baseline = 1991:2020, early = 2021:2040,
#'                                 mid = 2041:2060, late = 2061:2080))
CFfactor <- function(cf, period = "month", epoch = NULL) {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to CFfactor must be an instance of the `CFtime` class")
  if (nrow(cf@time) < 10) stop("Cannot create factor for very short time series.")

  period <- tolower(period)
  if (!((length(period) == 1) && (period %in% c("year", "season", "month", "dekad", "day"))))
    stop("Period specifier must be an atomic value of a supported period")

  # No fine-grained period factors for coarse source data
  timestep <- CFtime_unit_seconds[cf@datum@unit] * cf@resolution;
  if ((period == "year") && (timestep > 86400 * 366) ||
      (period == "season") && (timestep > 86400 * 90) || # Somewhat arbitrary
      (period == "month") && (timestep > 86400 * 31) ||
      (period == "dekad") && (timestep > 86400) ||       # Must be constructed from daily or finer data
      (period == "day") && (timestep > 86400))           # Must be no longer than a day
    stop("Cannot produce a short period factor from source data with long time interval")

  seasons <- c("DJF", "DJF", "MAM", "MAM", "MAM", "JJA", "JJA", "JJA", "SON", "SON", "SON", "DJF")
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  if (is.null(epoch)) {
    f <- switch(period,
      "year"   = sprintf("%04d", cf@time$year),
      "season" = ifelse(cf@time$month == 12, sprintf("%04d-DJF", cf@time$year + 1), sprintf("%04d-%s", cf@time$year, seasons[cf@time$month])),
      "month"  = sprintf("%04d-%s", cf@time$year, months[cf@time$month]),
      "dekad"  = sprintf("%04dD%02d", cf@time$year, (cf@time$month - 1) * 3 + pmin.int((cf@time$day - 1) %/% 10 + 1, 3)),
      "day"    = sprintf("%04d-%02d-%02d", cf@time$year, cf@time$month, cf@time$day)
    )
    out <- factor(f)
  } else {
    if (is.numeric(epoch)) ep <- list(epoch)
    else if ((is.list(epoch) && all(unlist(lapply(epoch, is.numeric))))) ep <- epoch
    else stop("When specified, the `epoch` parameter must be a numeric vector or a list thereof")

    out <- lapply(ep, function(years) {
      f <- switch(period,
        "year"   = ifelse(cf@time$year %in% years, sprintf("%04d", cf@time$year), NA_character_),
        "season" = ifelse((cf@time$month == 12) & ((cf@time$year + 1) %in% years), "DJF",
                          ifelse((cf@time$month < 12) & (cf@time$year %in% years), seasons[cf@time$month], NA_character_)),
        "month"  = ifelse(cf@time$year %in% years, months[cf@time$month], NA_character_),
        "dekad"  = ifelse(cf@time$year %in% years, sprintf("D%02d", (cf@time$month - 1) * 3 + pmin.int((cf@time$day - 1) %/% 10 + 1, 3)), NA_character_),
        "day"    = ifelse(cf@time$year %in% years, sprintf("%s-%02d", months[cf@time$month], cf@time$day), NA_character_)
      )
      f <- as.factor(f)
    })
    if (is.numeric(epoch)) out <- unlist(out)
    else names(out) <- names(epoch)
  }
  return(out)
}
