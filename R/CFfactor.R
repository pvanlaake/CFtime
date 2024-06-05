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
#' dekads, months, quarters, seasons or years).
#'
#' The factor will be generated in the order of the offsets of the `CFtime`
#' instance. While typical CF-compliant data sources use ordered time series
#' there is, however, no guarantee that the factor is ordered as multiple
#' `CFtime` objects may have been merged out of order.
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
#'   "Sx", with x being 1-4, preceeded by "YYYY" if no `epoch` is
#'   specified. Note that December dates are labeled as belonging to the
#'   subsequent year, so the date "2020-12-01" yields "2021S1". This implies
#'   that for standard CMIP files having one or more full years of data the
#'   first season will have data for the first two months (January and
#'   February), while the final season will have only a single month of data
#'   (December).
#'   \item `quarter`, the calendar quarter of each offset is returned as "Qx",
#'   with x being 1-4, preceeded by "YYYY" if no `epoch` is specified.
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
#' trivially be constructed from the output of the [as_timestamp()] function.
#'
#' For non-epoch factors the attribute 'CFtime' of the result contains a CFtime
#' instance that is valid for the result of applying the factor to a data set
#' that the `cf` argument is associated with. In other words, if CFtime instance
#' 'Acf' describes the temporal dimension of data set 'A' and a factor 'Af' is
#' generated from 'Acf', then `attr(Af, "CFtime")` describes the temporal
#' dimension of the result of, say, `apply(A, 1:2, tapply, Af, FUN)`. The
#' 'CFtime' attribute is `NULL` for epoch factors.
#'
#' @param cf CFtime. An instance of the `CFtime` class whose offsets will
#'   be used to construct the factor.
#' @param period character. A character string with one of the values
#'   "year", "season", "quarter", "month" (the default), "dekad" or "day".
#' @param epoch numeric or list, optional. Vector of years for which to
#'   construct the factor, or a list whose elements are each a vector of years.
#'   If `epoch` is not specified, the factor will use the entire time series for
#'   the factor.
#'
#' @returns If `epoch` is a single vector or not specified, a factor with a
#'   length equal to the number of offsets in `cf`. If `epoch` is a list, a list
#'   with the same number of elements and names as `epoch`, each containing a
#'   factor. Elements in the factor will be set to `NA` for time series values
#'   outside of the range of specified years.
#'
#'   The factor, or factors in the list, have attributes 'period', 'epoch' and
#'   'CFtime'. Attribute 'period' holds the value of the `period` argument.
#'   Attribute 'epoch' indicates the number of years that are included in the
#'   epoch, or -1 if no `epoch` is provided. Attribute 'CFtime' holds an
#'   instance of CFtime that has the same definition as `cf`, but with offsets
#'   corresponding to the mid-point of non-epoch factor levels; if the `epoch`
#'   argument is specified, attribute 'CFtime' is `NULL`.
#' @seealso [cut()] creates a non-epoch factor for arbitrary cut points.
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
  if (length(cf@offsets) < 10L) stop("Cannot create a factor for very short time series")

  period <- tolower(period)
  if (!((length(period) == 1L) && (period %in% CFt$factor_periods)))
    stop("Period specifier must be a single value of a supported period")

  # No fine-grained period factors for coarse source data
  timestep <- CFt$units$seconds[cf@datum@unit] * cf@resolution;
  if ((period == "year") && (timestep > 86400 * 366) ||
      (period %in% c("season", "quarter")) && (timestep > 86400 * 90) || # Somewhat arbitrary
      (period == "month") && (timestep > 86400 * 31) ||
      (period == "dekad") && (timestep > 86400) ||       # Must be constructed from daily or finer data
      (period == "day") && (timestep > 86400))           # Must be no longer than a day
    stop("Cannot produce a short period factor from source data with long time interval")

  time <- .offsets2time(cf@offsets, cf@datum)
  months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

  if (is.null(epoch)) {
    # Create the factor for the specified period as well as bounds dates for a
    # new CFtime instance for the factor. Lower bounds for the factor level is
    # easy, upper bound of last level takes effort.
    switch(period,
      "year"    = {
        out <- as.factor(sprintf("%04d", time$year))
        l  <- levels(out)
        dt <- c(paste0(l, "-01-01"), sprintf("%04d-01-01", as.integer(l[nlevels(out)]) + 1L))
      },
      "season"  = {
        if (!requireNamespace("stringr"))
          stop("Must install package `stringr` to use this functionality.")

        out <- as.factor(
                 ifelse(time$month == 12L, sprintf("%04dS1", time$year + 1L),
                                           sprintf("%04dS%d", time$year, time$month %/% 3L + 1L)))
        l  <- levels(out)
        dt <- ifelse(substr(l, 6L, 6L) == "1", paste0(as.integer(substr(l, 1L, 4L)) - 1L, "-12-01"),
                     stringr::str_replace_all(l, c("S2" = "-03-01", "S3" = "-06-01", "S4" = "-09-01")))
        ll <- l[nlevels(out)]
        lp <- as.integer(substr(ll, 6L, 6L))
        if (lp == 1L)
          dt <- c(dt, sprintf("%04d-03-01", as.integer(substr(ll, 1L, 4L)) + 1L))
        else dt <- c(dt, sprintf("%s-%02d-01", substr(ll, 1L, 4L), lp * 3L))
      },
      "quarter" = {
        if (!requireNamespace("stringr"))
          stop("Must install package `stringr` to use this functionality.")

        out <- as.factor(sprintf("%04dQ%d", time$year, (time$month - 1L) %/% 3L + 1L))
        l  <- levels(out)
        dt <- stringr::str_replace_all(l, c("Q1" = "-01-01", "Q2" = "-04-01", "Q3" = "-07-01", "Q4" = "-10-01"))
        ll <- l[nlevels(out)]
        lp <- as.integer(substr(ll, 6L, 6L))
        if (lp == 4L)
          dt <- c(dt, sprintf("%04d-01-01", as.integer(substr(ll, 1L, 4L)) + 1L))
        else dt <- c(dt, sprintf("%s-%02d-01", substr(ll, 1L, 4L), lp * 3L + 1L))
      },
      "month"   = {
        out <- as.factor(sprintf("%04d-%s", time$year, months[time$month]))
        l  <- levels(out)
        dt <- paste0(l, "-01")
        ll <- l[nlevels(out)]
        lp <- as.integer(substr(ll, 6L, 7L))
        if (lp == 12L)
          dt <- c(dt, sprintf("%04d-01-01", as.integer(substr(ll, 1L, 4L)) + 1L))
        else dt <- c(dt, sprintf("%s-%02d-01", substr(ll, 1L, 4L), lp + 1L))
      },
      "dekad"   = {
        out <- as.factor(sprintf("%04dD%02d", time$year, (time$month - 1L) * 3L + pmin.int((time$day - 1L) %/% 10L + 1L, 3L)))
        l  <- levels(out)
        dk <- as.integer(substr(l, 6L, 7L)) - 1L
        dt <- sprintf("%s-%02d-%s", substr(l, 1L, 4L), dk %/% 3L + 1L, c("01", "11", "21")[dk %% 3L + 1L])
        ll <- l[nlevels(out)]
        lp <- as.integer(substr(ll, 6L, 7L))
        yr <- as.integer(substr(lp, 1L, 4L))
        if (lp == 36L)
          dt <- c(dt, sprintf("%04d-01-01", yr + 1L))
        else dt <- c(dt, sprintf("%04d-%02d-%s", yr, (lp + 1L) %/% 3L + 1L, c("01", "11", "21")[(lp + 1L) %% 3L + 1L]))
      },
      "day"     = {
        out <- as.factor(sprintf("%04d-%02d-%02d", time$year, time$month, time$day))
        l <- levels(out)
        lp <- l[nlevels(out)]
        last <- .offsets2time(.parse_timestamp(cf@datum, lp)$offset, cf@datum)
        dt <- c(l, sprintf("%04d-%02d-%02d", last$year, last$month, last$day))
      }
    )

    # Convert bounds dates to an array of offsets, find mid-points, create new CFtime instance
    off  <- .parse_timestamp(cf@datum, dt)$offset
    off[is.na(off)] <- 0     # This can happen only when the time series starts at or close to the datum origin, for seasons
    noff <- length(off)
    bnds <- rbind(off[1L:(noff - 1L)], off[2L:noff])
    off  <- bnds[1L,] + (bnds[2L,] - bnds[1L,]) * 0.5
    new_cf <- CFtime(cf@datum@definition, cf@datum@calendar, off)
    bounds(new_cf) <- TRUE

    # Bind attributes to the factor
    attr(out, "epoch") <- -1L
    attr(out, "period") <- period
    attr(out, "CFtime") <- new_cf
    return(out)
  }

  # Epoch factor
  if (is.numeric(epoch)) ep <- list(epoch)
  else if ((is.list(epoch) && all(unlist(lapply(epoch, is.numeric))))) ep <- epoch
  else stop("When specified, the `epoch` parameter must be a numeric vector or a list thereof")

  out <- lapply(ep, function(years) {
    f <- switch(period,
                "year"    = ifelse(time$year %in% years, sprintf("%04d", time$year), NA_character_),
                "season"  = ifelse((time$month == 12L) & ((time$year + 1L) %in% years), "S1",
                            ifelse((time$month < 12L) & (time$year %in% years), sprintf("S%d", time$month %/% 3L + 1L), NA_character_)),
                "quarter" = ifelse(time$year %in% years, sprintf("Q%d", (time$month - 1L) %/% 3L + 1L), NA_character_),
                "month"   = ifelse(time$year %in% years, months[time$month], NA_character_),
                "dekad"   = ifelse(time$year %in% years, sprintf("D%02d", (time$month - 1L) * 3L + pmin.int((time$day - 1L) %/% 10L + 1L, 3L)), NA_character_),
                "day"     = ifelse(time$year %in% years, sprintf("%s-%02d", months[time$month], time$day), NA_character_)
    )
    f <- as.factor(f)
    attr(f, "epoch") <- length(years)
    attr(f, "period") <- period
    attr(f, "CFtime") <- NULL
    f
  })
  if (is.numeric(epoch)) out <- out[[1L]]
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
  if (is.factor(f)) out <- out[[1L]]
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
  if (cal == 3L) {
    res <- rep(c(360L, 90L, 90L, 30L, 10L, 1L)[which(CFt$factor_periods == period)], nlevels(f))
  } else {
    if (attr(f, "epoch") > 0L) {
      if (cal %in% c(1L, 2L, 4L)) {
        res <- switch(period,
                      "year"    = rep(365L, nlevels(f)),
                      "season"  = c(90L, 92L, 92L, 91L)[as.integer(substr(levels(f), 2, 2))],
                      "quarter" = c(90L, 91L, 92L, 92L)[as.integer(substr(levels(f), 2, 2))],
                      "month"   = c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[as.integer(levels(f))],
                      "dekad"   = {
                        dk <- as.integer(substr(levels(f), 2L, 3L))
                        ifelse(dk %% 3L > 0L | dk %in% c(12L, 18L, 27L, 33L), 10L,
                               ifelse(dk %in% c(3L, 9L, 15L, 21L, 24L, 30L, 36L), 11L, 8L))
                      },
                      "day"     = rep(1L, nlevels(f))
        )
      } else if (cal == 5L) {
        res <- switch(period,
                      "year"    = rep(366L, nlevels(f)),
                      "season"  = c(91L, 92L, 92L, 91L)[as.integer(substr(levels(f), 2, 2))],
                      "quarter" = c(91L, 91L, 92L, 92L)[as.integer(levels(f))],
                      "month"   = c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[as.integer(levels(f))],
                      "dekad"   = {
                        dk <- as.integer(substr(levels(f), 2L, 3L))
                        ifelse(dk %% 3L > 0L | dk %in% c(12L, 18L, 27L, 33L), 10L,
                               ifelse(dk %in% c(3L, 9L, 15L, 21L, 24L, 30L, 36L), 11L, 9L))
                      },
                      "day"     = rep(1L, nlevels(f))
        )
      }
    } else {  # not an epoch factor
      res <- switch(period,
                    "year"    = ifelse(.is_leap_year(as.integer(levels(f)), cal), 366L, 365L),
                    "season"  = {
                      year <- as.integer(substr(levels(f), 1L, 4L))
                      season <- as.integer(substr(levels(f), 6L, 6L))
                      ifelse(.is_leap_year(year, cal), c(91L, 92L, 92L, 91L)[season], c(90L, 92L, 92L, 91L)[season])
                    },
                    "quarter" = {
                      year <- as.integer(substr(levels(f), 1L, 4L))
                      qtr  <- as.integer(substr(levels(f), 6L, 6L))
                      ifelse(.is_leap_year(year, cal), c(91L, 91L, 92L, 92L)[qtr], c(90L, 91L, 92L, 92L)[qtr])
                    },
                    "month"   = {
                      year  <- as.integer(substr(levels(f), 1L, 4L))
                      month <- as.integer(substr(levels(f), 6L, 7L))
                      ifelse(.is_leap_year(year, cal), c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month],
                             c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[month])
                    },
                    "dekad"   = {
                      year  <- as.integer(substr(levels(f), 1L, 4L))
                      dk <- as.integer(substr(levels(f), 6L, 7L))
                      ifelse(dk %% 3L > 0L | dk %in% c(12L, 18L, 27L, 33L), 10L,
                             ifelse(dk %in% c(3L, 9L, 15L, 21L, 24L, 30L, 36L), 11L,
                                    ifelse(.is_leap_year(year, cal), 9L, 8L)))
                    },
                    "day"     = rep(1L, nlevels(f))
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
    stop("Argument `coverage` must be a chaarcter string with a value of \"absolute\" or \"relative\"")

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

  if (is.factor(f)) out <- out[[1L]]
  return(out)
}
