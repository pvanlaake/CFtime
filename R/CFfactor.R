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
#' trivially be constructed from the output of the [CFtimestamp()] function.
#'
#' @param cf CFtime. An atomic instance of the `CFtime` class whose offsets will
#'   be used to construct the factor.
#' @param period character. An atomic character string with one of the values
#'   "year", "season", "month" (the default), "dekad" or "day".
#' @param epoch numeric or list, optional. Vector of years for which to construct
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
#' # Create three monthly factors for early, mid and late 21st century epochs
#' ep <- CFfactor(cf, epoch = list(early = 2021:2040, mid = 2041:2060, late = 2061:2080))
CFfactor <- function(cf, period = "month", epoch = NULL) {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to CFfactor() must be an instance of the `CFtime` class")
  if (nrow(cf@time) < 10) stop("Cannot create a factor for very short time series")

  period <- tolower(period)
  if (!((length(period) == 1) && (period %in% CFt$factor_periods)))
    stop("Period specifier must be an atomic value of a supported period")

  # No fine-grained period factors for coarse source data
  timestep <- CFt$units$seconds[cf@datum@unit] * cf@resolution;
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
    out <- as.factor(f)
    attr(out, "epoch") <- -1L
    attr(out, "period") <- period
    return(out)
  }

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
    attr(f, "epoch") <- length(years)
    attr(f, "period") <- period
    f
  })
  if (is.numeric(epoch)) out <- out[[1]]
  else names(out) <- names(epoch)
  return(out)
}

#' Number of base time units in each factor level
#'
#' Given a factor as returned by [CFfactor()] and the `CFtime` instance from
#' which the factor was derived, this function will return a numeric vector with
#' the number of time units in each level of the factor.
#'
#' The result of this function is useful to convert between absolute and
#' relative values. Climate change anomalies, for instance, are usually computed
#' by differencing average values between a future period and a baseline period.
#' Going from average values back to absolute values for an aggregate period
#' (which is typical for temperature and precipitation, among other variables)
#' is easily done with the result of this function, without having to consider
#' the specifics of the calendar of the data set.
#'
#' If the factor `f` is for an epoch (e.g. spanning multiple years and the
#' levels do not indicate the specific year), then the result will indicate the
#' number of time units of the period in a regular single year. In other words,
#' for an epoch of 2041-2060 and a monthly factor on a standard calendar with a
#' `days` unit, the result will be `c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)`.
#' Leap days are thus only considered for the `366_day` and `all_leap` calendars.
#'
#' Note that this function gives the number of time units in each level of the
#' factor - the actual number of data points in the `cf` instance per factor
#' level may be different. Use [CFfactor_coverage()] to determine the actual
#' number of data points or the coverage of data points relative to the factor
#' level.
#'
#' @param cf CFtime. An instance of CFtime.
#' @param f factor or list. A factor or a list of factors derived from the
#'   parameter `cf`. The factor or list thereof should generally be generated by
#'   the function [CFfactor()].
#'
#' @returns If `f` is a factor, a numeric vector with a length equal to the
#'   number of levels in the factor, indicating the number of time units in each
#'   level of the factor. If `f` is a list of factors, a list with each element
#'   a numeric vector as above.
#' @export
#'
#' @examples
#' cf <- CFtime("days since 2001-01-01", "365_day", 0:364)
#' f <- CFfactor(cf, "dekad")
#' CFfactor_units(cf, f)
CFfactor_units <- function(cf, f) {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to `CFfactor_units()` must be an instance of the `CFtime` class")

  if (is.list(f)) factors <- f else factors <- list(f)
  if (!(all(unlist(lapply(factors, function(x) is.factor(x) && is.numeric(attr(x, "epoch")) &&
                          attr(x, "period") %in% CFt$factor_periods)))))
    stop("Argument `f` must be a factor generated by the function `CFfactor()`")

  cal <- cf@datum@cal_id
  upd <- CFt$units$per_day[cf@datum@unit]
  out <- lapply(factors, function(fac) .factor_units(fac, cal, upd))
  if (is.factor(f)) out <- out[[1]]
  return(out)
}

#' Calculate time units in factors
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param f factor. Factor as generated by `CFfactor()`.
#' @param cal numeric. Calendar id of the `CFtime()` instance.
#' @param upd numeric. Number of units per day, from the `CFt` environment.
#'
#' @returns A vector as long as the number of levels in the factor.
#' @noRd
.factor_units <- function(f, cal, upd) {
  period <- attr(f, "period")
  if (cal == 3) {
    res <- rep(c(360, 90, 30, 10, 1)[which(CFt$factor_periods == period)], nlevels(f))
  } else {
    if (attr(f, "epoch") > 0) {
      if (cal %in% c(1, 2, 4)) {
        res <- switch(period,
                      "year"   = rep(365, nlevels(f)),
                      "season" = ifelse(levels(f) %in% c("MAM", "JJA"), 92, ifelse(levels(f) == "SON", 91, 90)),
                      "month"  = c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[as.integer(levels(f))],
                      "dekad"  = {
                        dk <- as.integer(substr(levels(f), 2, 3))
                        ifelse(dk %% 3 > 0 | dk %in% c(12, 18, 27, 33), 10, ifelse(dk %in% c(3, 9, 15, 21, 24, 30, 36), 11, 8))
                      },
                      "day"    = rep(1, nlevels(f))
        )
      } else if (cal == 5) {
        res <- switch(period,
                      "year"   = rep(366, nlevels(f)),
                      "season" = ifelse(levels(f) %in% c("MAM", "JJA"), 92, 91),
                      "month"  = c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[as.integer(levels(f))],
                      "dekad"  = {
                        dk <- as.integer(substr(levels(f), 2, 3))
                        ifelse(dk %% 3 > 0 | dk %in% c(12, 18, 27, 33), 10, ifelse(dk %in% c(3, 9, 15, 21, 24, 30, 36), 11, 9))
                      },
                      "day"    = rep(1, nlevels(f))
        )
      }
    } else {  # not an epoch factor
      res <- switch(period,
                    "year"   = ifelse(.is_leap_year(as.integer(levels(f)), cal), 366, 365),
                    "season" = {
                      year <- substr(levels(f), 1, 4)
                      season <- substr(levels(f), 6, 8)
                      ifelse(season %in% c("MAM", "JJA"), 92,
                             ifelse(season == "SON", 91,
                                    ifelse(.is_leap_year(year, cal), 91, 90)))
                    },
                    "month"  = {
                      year  <- as.integer(substr(levels(f), 1, 4))
                      month <- as.integer(substr(levels(f), 6, 7))
                      ifelse(.is_leap_year(year, cal), c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month],
                             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[month])
                    },
                    "dekad"  = {
                      year  <- as.integer(substr(levels(f), 1, 4))
                      dk <- as.integer(substr(levels(f), 6, 7))
                      ifelse(dk %% 3 > 0 | dk %in% c(12, 18, 27, 33), 10, ifelse(dk %in% c(3, 9, 15, 21, 24, 30, 36), 11,
                                                                                 ifelse(.is_leap_year(year, cal), 9, 8)))
                    },
                    "day"    = rep(1, nlevels(f))
      )
    }
  }
  return(res * upd)
}

#' Coverage of time elements for each factor level
#'
#' This function calculates the number of time elements, or the relative
#' coverage, in each level of a factor generated by [CFfactor()].
#'
#' @param cf CFtime. An instance of CFtime.
#' @param f factor or list. A factor or a list of factors derived from the
#'   parameter `cf`. The factor or list thereof should generally be generated by
#'   the function [CFfactor()].
#' @param coverage "absolute" or "relative".
#'
#' @returns If `f` is a factor, a numeric vector with a length equal to the
#'   number of levels in the factor, indicating the number of units from the
#'   time series in `cf` contained in each level of the factor when
#'   `coverage = "absolute"` or the proportion of units present relative to the
#'   maximum number when `coverage = "relative"`. If `f` is a list of factors, a
#'   list with each element a numeric vector as above.
#' @export
#'
#' @examples
#' cf <- CFtime("days since 2001-01-01", "365_day", 0:364)
#' f <- CFfactor(cf, "dekad")
#' CFfactor_coverage(cf, f, "absolute")
CFfactor_coverage <- function(cf, f, coverage = "absolute") {
  if (!(methods::is(cf, "CFtime"))) stop("First argument to `CFfactor_coverage()` must be an instance of the `CFtime` class")

  if (is.list(f)) factors <- f else factors <- list(f)
  if (!(all(unlist(lapply(factors, function(x) is.factor(x) && is.numeric(attr(x, "epoch")) &&
                          attr(x, "period") %in% CFt$factor_periods)))))
    stop("Argument `f` must be a factor generated by the function `CFfactor()`")

  if (!(is.character(coverage) && coverage %in% c("absolute", "relative")))
    stop("Argument `coverage` must be an atomic string with a value of \"absolute\" or \"relative\"")

  if (coverage == "relative") {
    cal <- cf@datum@cal_id
    upd <- CFt$units$per_day[cf@datum@unit]
    out <- lapply(factors, function(fac) {
      res <- tabulate(fac) / .factor_units(fac, cal, upd)
      yrs <- attr(fac, "epoch")
      if (yrs > 0) res <- res / yrs
      return(res)
    })
  } else {
    out <- lapply(factors, tabulate)
  }

  if (is.factor(f)) out <- out[[1]]
  return(out)
}
