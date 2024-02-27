#' CF Metadata Conventions time representation
#'
#' @slot datum CFdatum. The atomic origin upon which the `offsets` are based.
#' @slot resolution numeric. The average number of time units between offsets.
#' @slot offsets numeric. A vector of offsets from the datum.
#'
#' @returns An object of class CFtime.
#' @export
setClass("CFtime",
         slots = c(
           datum      = "CFdatum",
           resolution = "numeric",
           offsets    = "numeric"
         ))

#' Create a CFtime object
#'
#' This function creates an instance of the `CFtime` class. The arguments to
#' the call are typically read from a CF-compliant data file with climatological
#' observations or climate projections. Specification of arguments can also be
#' made manually in a variety of combinations.
#'
#' @param definition character. An atomic string describing the time coordinate
#'   of a CF-compliant data file.
#' @param calendar character. An atomic string describing the calendar to use
#'   with the time dimension definition string. Default value is "standard".
#' @param offsets numeric or character, optional. When numeric, a vector of
#'   offsets from the origin in the time series. When a character vector,
#'   timestamps in ISO8601 or UDUNITS format. When an atomic character string, a
#'   timestamp in ISO8601 or UDUNITS format and then a time series will be
#'   generated with a separation between steps equal to the unit of measure in
#'   the definition, inclusive of the definition timestamp. The unit of measure
#'   of the offsets is defined by the time series definition.
#'
#' @returns An instance of the `CFtime` class.
#' @export
#'
#' @examples
#' CFtime("days since 1850-01-01", "julian", 0:364)
#'
#' CFtime("hours since 2023-01-01", "360_day", "2023-01-30T23:00")
CFtime <- function(definition, calendar = "standard", offsets = NULL) {
  if (is.null(calendar)) calendar <- "standard" # This may occur when "calendar" attribute is not defined in the NC file
  datum <- CFdatum(definition, calendar)

  if (is.array(offsets)) dim(offsets) <- NULL

  if (is.null(offsets)) {
    methods::new("CFtime", datum = datum, resolution = NA_real_, offsets = numeric())
  } else if (is.numeric(offsets)) {
    stopifnot(.validOffsets(offsets, CFt$units$per_day[datum@unit]))

    if (length(offsets) > 1L) {
      resolution <- (max(offsets) - min(offsets)) / (length(offsets) - 1L)
    } else {
      resolution <- NA_real_
    }
    methods::new("CFtime", datum = datum, resolution = resolution, offsets = offsets)
  } else if (is.character(offsets)) {
    time <- .parse_timestamp(datum, offsets)
    if (anyNA(time$year)) stop("Offset argument contains invalid timestamps")

    if (length(offsets) == 1L) {
      off <- seq(0L, time$offset[1L])
      resolution <- 1
    } else {
      off <- time$offset
      resolution <- (max(time$offset) - min(time$offset)) / (length(time$offset) - 1L)
    }
    methods::new("CFtime", datum = datum, resolution = resolution, offsets = off)
  } else stop("Invalid offsets for CFtime object")
}

#' @aliases CFproperties
#' @title Properties of a CFtime object
#'
#' @description These functions return the properties of an instance of the
#'   `CFtime` class. The properties are all read-only, but offsets can be added
#'   using the `+` operator.
#'
#' @param cf CFtime. An instance of `CFtime`.
#'
#' @returns `CFcalendar()` and `CFunit()` return an atomic character string.
#'   `CForigin()` returns a data frame of timestamp elements with a single row
#'   of data. `CFtimezone()` returns the datum time zone as an atomic character
#'   string. `CFoffsets()` returns a vector of offsets or `NULL` if no offsets
#'   have been set.
#'
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' CFdefinition(cf)
#' CFcalendar(cf)
#' CFunit(cf)
#' CFtimezone(cf)
#' CForigin(cf)
#' CFoffsets(cf)
#' CFresolution(cf)

#' @describeIn CFproperties The definition string of the CFtime instance
#' @export
CFdefinition <- function(cf) definition(cf@datum)

#' @describeIn CFproperties The calendar of the CFtime instance
#' @export
CFcalendar <- function(cf) calendar(cf@datum)

#' @describeIn CFproperties The unit of the CFtime instance
#' @export
CFunit <- function(cf) CFt$units$name[unit(cf@datum)]

#' @describeIn CFproperties The origin of the CFtime instance in timestamp elements
#' @export
CForigin <- function(cf) cf@datum@origin

#' @describeIn CFproperties The time zone of the datum of the CFtime instance as a character string
#' @export
CFtimezone <- function(cf) timezone(cf@datum)

#' @describeIn CFproperties The offsets of the CFtime instance as a vector
#' @export
CFoffsets <- function(cf) cf@offsets

#' @describeIn CFproperties The average separation between the offsets in the CFtime instance
#' @export
CFresolution <- function(cf) cf@resolution

setMethod("show", "CFtime", function(object) {
  noff <- length(object@offsets)
  if (noff == 0L) {
    el <- "  Elements: (no elements)\n"
  } else {
    d <- CFrange(object)
    if (noff > 1L) {
      el <- sprintf("  Elements: [%s .. %s] (average of %f %s between %d elements)\n",
                    d[1L], d[2L], object@resolution, CFt$units$name[unit(object@datum)], noff)
    } else {
      el <- paste("  Elements:", d[1L], "\n")
    }
  }
  cat("CF time series:\n", methods::show(object@datum), el, sep = "")
})

#' @aliases  CFrange
#'
#' @title Extreme time series values
#'
#' @description Character representation of the extreme values in the time series
#'
#' @param x An instance of the `CFtime` class
#'
#' @returns character. Vector of two character representations of the extremes of the time series.
#' @export
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' CFrange(cf)
setGeneric("CFrange", function(x) standardGeneric("CFrange"))

#' @describeIn CFrange Extreme values of the time series
setMethod("CFrange", "CFtime", function(x) .ts_extremes(x))

#' Indicates if the time series is complete
#'
#' This function indicates if the time series is complete, meaning that the time
#' steps are equally spaced and there are thus no gaps in the time series.
#'
#' This function gives exact results for time series where the nominal
#' *unit of separation* between observations in the time series is exact in terms of the
#' datum unit. As an example, for a datum unit of "days" where the observations
#' are spaced a fixed number of days apart the result is exact, but if the same
#' datum unit is used for data that is on monthly a basis, the *assessment* is
#' approximate because the number of days per month is variable and dependent on
#' the calendar (the exception being the `360_day` calendar, where the
#' assessment is exact). The *result* is still correct in most cases (including
#' all CF-compliant data sets that the developers have seen) although
#' there may be esoteric constructions of CFtime and offsets that trip up this
#' implementation.
#'
#' @param x An instance of the `CFtime` class
#'
#' @returns logical. `TRUE` if the time series is complete, with no gaps;
#'   `FALSE` otherwise. If no offsets have been added to the CFtime instance,
#'   `NA` is returned.
#' @export
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' CFcomplete(cf)
CFcomplete <- function(x) {
  if (!methods::is(x, "CFtime")) stop("Argument must be an instance of CFtime")
  if (length(x@offsets) == 0L) NA
  else .ts_equidistant(x)
}

#' Which time steps fall within two extreme values
#'
#' Given two extreme character timestamps, return a logical vector of a length
#' equal to the number of time steps in the CFtime instance with values `TRUE`
#' for those time steps that fall between the two extreme values, `FALSE`
#' otherwise. This can be used to select slices from the time series in reading
#' or analysing data.
#'
#' @param x CFtime. The time series to operate on.
#' @param extremes character. Vector of two timestamps that represent the
#'   extremes of the time period of interest. The timestamps must be in
#'   increasing order. The timestamps need not fall in the range of the time
#'   steps in the CFtime stance.
#'
#' @returns A logical vector with a length equal to the number of time steps in
#'   `x` with values `TRUE` for those time steps that fall between the two
#'   extreme values, `FALSE` otherwise. The earlier timestamp is included, the
#'   later timestamp is excluded. A specification of `c("2022-01-01", "2023-01-01")`
#'   will thus include all time steps that fall in the year 2022.
#' @export
#'
#' @examples
#' cf <- CFtime("hours since 2023-01-01 00:00:00", "standard", 0:23)
#' CFsubset(cf, c("2022-12-01", "2023-01-01 03:00"))
CFsubset <- function(x, extremes) {
  if (!methods::is(x, "CFtime")) stop("First argument must be an instance of CFtime")
  if (!is.character(extremes) || length(extremes) != 2L)
    stop("Second argument must be a character vector of two timestamps")
  if (extremes[2L] < extremes[1L]) extremes <- c(extremes[2L], extremes[1L])
  .ts_subset(x, extremes)
}

#' Equivalence of CFtime objects
#'
#' This operator can be used to test if two `CFtime` objects represent the same
#' CF-convention time coordinates. Two `CFtime` objects are considered equivalent
#' if they have an equivalent datum and the same offsets.
#'
#' @param e1,e2 CFtime. Instances of the `CFtime` class.
#'
#' @returns `TRUE` if the `CFtime` objects are equivalent, `FALSE` otherwise.
#' @export
#' @aliases CFtime-equivalent
#'
#' @examples
#' e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
#' e2 <- CFtime("days since 1850-01-01 00:00:00", "standard", 0:364)
#' e1 == e2
setMethod("==", c("CFtime", "CFtime"), function(e1, e2)
  .datum_equivalent(e1@datum, e2@datum) &&
  length(e1@offsets) == length(e2@offsets) &&
  all(e1@offsets == e2@offsets))

#' Merge two CFtime objects
#'
#' Two `CFtime` instances can be merged into one with this operator, provided
#' that the units and calendars of the datums of the two instances are
#' equivalent.
#'
#' If the origins of the two datums are not identical, the earlier origin is
#' preserved and the offsets of the later origin are updated in the resulting
#' CFtime instance.
#'
#' The order of the two parameters is indirectly significant. The resulting
#' `CFtime` instance will have the offsets of both instances in the order that
#' they are specified. There is no reordering or removal of duplicates. This is
#' because the time series are usually associated with a data set and the
#' correspondence between the data in the files and the CFtime instance is thus
#' preserved. When merging the data sets described by this time series, the
#' order must be identical to the merging here.
#'
#' @param e1,e2 CFtime. Instances of the `CFtime` class.
#'
#' @returns A `CFtime` object with a set of offsets composed of the offsets of
#'   the instances of `CFtime` that the operator operates on. If the datum units
#'   or calendars of the `CFtime` instances are not equivalent, an error is
#'   thrown.
#' @export
#' @aliases CFtime-merge
#'
#' @examples
#' e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
#' e2 <- CFtime("days since 1850-01-01 00:00:00", "standard", 365:729)
#' e1 + e2
setMethod("+", c("CFtime", "CFtime"), function(e1, e2) {
  if (!.datum_compatible(e1@datum, e2@datum)) stop('Datums not compatible')
  if (all(e1@datum@origin[1:6] == e2@datum@origin[1:6]))
    CFtime(definition(e1@datum), calendar(e1@datum), c(e1@offsets, e2@offsets))
  else {
    diff <- .parse_timestamp(e1@datum, paste(origin_date(e2@datum), origin_time(e2@datum)))$offset
    if (is.na(diff)) {
      diff <- .parse_timestamp(e2@datum, paste(origin_date(e1@datum), origin_time(e1@datum)))$offset
      CFtime(definition(e2@datum), calendar(e2@datum), c(e1@offsets + diff, e2@offsets))
    } else
      CFtime(definition(e1@datum), calendar(e1@datum), c(e1@offsets, e2@offsets + diff))
  }
})

#' Extend a CFtime object with additional offsets
#'
#' A `CFtime` instance can be extended by adding additional offsets using this
#' operator.
#'
#' The resulting `CFtime` instance will have its offsets in the order that they
#' are added, meaning that the offsets from the `CFtime` instance come first and
#' those from the numeric vector follow. There is no reordering or removal of
#' duplicates. This is because the time series are usually associated with a
#' data set and the correspondence between the two is thus preserved, if and
#' only if the data sets are merged in the same order.
#'
#' Note that when adding multiple vectors of offsets to a `CFtime` instance, it
#' is more efficient to first concatenate the vectors and then do a final
#' addition to the `CFtime` instance. So avoid `CFtime(definition, calendar, e1) + CFtime(definition, calendar, e2) + CFtime(definition, calendar, e3) + ...`
#' but rather do `CFtime(definition, calendar, e1) + c(e2, e3, ...)`. It is the
#' responsibility of the operator to ensure that the offsets of the different
#' data sets are in reference to the same datum.
#'
#' Negative offsets will generate an error.
#'
#' @param e1 CFtime. Instance of the `CFtime` class.
#' @param e2 numeric. Vector of offsets to be added to the `CFtime` instance.
#'
#' @returns A `CFtime` object with offsets composed of the `CFtime` instance and
#'   the numeric vector.
#' @export
#' @aliases CFtime-append
#'
#' @examples
#' e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
#' e2 <- 365:729
#' e1 + e2
setMethod("+", c("CFtime", "numeric"), function(e1, e2) {
  if (is.array(e2)) dim(e2) <- NULL
  if (.validOffsets(e2, CFt$units$per_day[unit(e1@datum)]))
    CFtime(definition(e1@datum), calendar(e1@datum), c(e1@offsets, e2))
})

#' Validate offsets passed into a CFtime instance
#'
#' This is an internal function that should not be used outside the CFtime
#' package.
#'
#' Tests the `offsets` values. Throws an error if the argument contains negative or `NA` values.
#'
#' @param offsets The offsets to test
#'
#' @returns logical. `TRUE` if the offsets are valid, throws an error otherwise.
#' @noRd
.validOffsets <- function(offsets, upd) {
  if (any(is.na(offsets) | (offsets < 0))) stop("Offsets cannot contain negative or `NA` values.")
  if (any(offsets > 1000000 * upd)) stop("Offset values are outside of reasonable range (year 1 - 2500).")
  TRUE
}

#' Return the extremes of the time series as character strings
#'
#' This function returns the first and last timestamp of the time series as a
#' vector. Note that the offsets do not have to be sorted.
#'
#' This is an internal function that should not be used outside of the CFtime
#' package.
#'
#' @param x CFtime. The time series to operate on.
#'
#' @returns Vector of two character strings that represent the starting and
#'   ending timestamps in the time series. If all of the timestamps in the time
#'   series have a time component of `00:00:00` the date of the timestamp is
#'   returned, otherwise the full timestamp (without any time zone information).
#'
#' @noRd
.ts_extremes <- function(x) {
  if (length(x@offsets) == 0L) return(c(NA_character_, NA_character_))

  time <- .offsets2time(range(x@offsets), x@datum)

  if (sum(time$hour, time$minute, time$second) == 0) { # all times are 00:00:00
    return(sprintf("%04d-%02d-%02d", time$year, time$month, time$day))
  } else {
    t <- .format_time(time)
    return(sprintf("%04d-%02d-%02dT%s", time$year, time$month, time$day, t))
  }
}

#' Indicates if the time series has equidistant time steps
#'
#' This function returns `TRUE` if the time series has uniformly distributed
#' time steps between the extreme values, `FALSE` otherwise. First test without
#' sorting; this should work for most data sets. If not, only then offsets are
#' sorted. For most data sets that will work but for implied resolutions of
#' month, season, year, etc based on a "days" or finer datum unit this will fail
#' due to the fact that those coarser units have a variable number of days per
#' time step, in all calendars except for `360_day`. For now, an approximate
#' solution is used that should work in all but the most non-conformal exotic
#' arrangements.
#'
#' This function should only be called after offsets have been added.
#'
#' This is an internal function that should not be used outside of the CFtime
#' package.
#'
#' @param x CFtime. The time series to operate on.
#'
#' @returns `TRUE` if all time steps are equidistant, `FALSE` otherwise.
#'
#' @noRd
.ts_equidistant <- function(x) {
  out <- all(diff(x@offsets) == x@resolution)
  if (!out) {
    doff <- diff(sort(x@offsets))
    out <- all(doff == x@resolution)
    if (!out) {
      # Don't try to make sense of totally non-standard arrangements such as
      # datum units "years" or "months" describing sub-daily time steps.
      # Also, 360_day calendar should be well-behaved so we don't want to get here.
      if (unit(x@datum) > 4L || calendar_id(x@datum) == 3L) return(FALSE)

      # Check if we have monthly or yearly data on a finer-scale datum
      # This is all rather approximate but should be fine in most cases
      # This accommodates middle-of-the-time-period offsets as per the CF Metadata Conventions
      # Please report problems at https://github.com/pvanlaake/CFtime/issues
      ddays <- range(doff) * CFt$units$per_day[unit(x@datum)]
      return((ddays[1] >= 28 && ddays[2] <= 31) ||    # months
             (ddays[1] >= 90 && ddays[2] <= 92) ||    # seasons
             (ddays[1] >= 365 && ddays[2] <= 366))    # years
    }
  }
  out
}

#' Which time steps fall within two extreme values
#'
#' Given two extreme character timestamps, return a logical vector of a length
#' equal to the number of time steps in the CFtime instance with values `TRUE`
#' for those time steps that fall between the two extreme values, `FALSE`
#' otherwise.
#'
#' **NOTE** Giving crap as the earlier timestamp will set that value to 0. So
#' invalid input will still generate a result. To be addressed. Crap in later
#' timestamp is not tolerated.
#'
#' @param x CFtime. The time series to operate on.
#' @param extremes character. Vector of two timestamps that represent the
#'   extremes of the time period of interest. The timestamps must be in
#'   increasing order.
#'
#' @returns A logical vector with a length equal to the number of time steps in
#'   `x` with values `TRUE` for those time steps that fall between the two
#'   extreme values, `FALSE` otherwise. The earlier timestamp is included, the
#'   later timestamp is excluded. A specification of `c("2022-01-01", "2023-01-01)`
#'   will thus include all time steps that fall in the year 2022.
#' @noRd
.ts_subset <- function(x, extremes) {
  ext <- .parse_timestamp(x@datum, extremes)$offset
  if (is.na(ext[1L])) ext[1L] <- 0
  if (ext[1L] > max(x@offsets) || is.na(ext[2L])) rep(FALSE, length(x@offsets))
  else x@offsets >= ext[1L] & x@offsets < ext[2L]
}

#' Decompose a vector of offsets, in units of the datum, to their timestamp
#' values
#'
#' This function adds a specified amount of time to the origin of a CFts object.
#'
#' This is an internal function that should not be used outside of the CFtime
#' package.
#'
#' This functions may introduce inaccuracies where the datum unit is "months" or
#' "years", due to the ambiguous definition of these units.
#'
#' @param offsets numeric. Vector of offsets to add to the datum.
#' @param datum CFdatum. The datum that defines the unit of the offsets and the
#'   origin to add the offsets to.
#'
#' @returns A data.frame with columns for the timestamp elements and as many
#' rows as there are offsets.
#' @noRd
.offsets2time <- function(offsets, datum) {
  len <- length(offsets)
  if(len == 0L) return(data.frame(year = integer(), month = integer(), day = integer(),
                                  hour = integer(), minute = integer(), second = numeric(),
                                  tz = character(), offset = numeric()))

  if (unit(datum) <= 4L) { # Days, hours, minutes, seconds
    # First add time: convert to seconds first, then recompute time parts
    secs <- offsets * CFt$units$seconds[unit(datum)]
    secs <- secs + datum@origin$hour[1L] * 3600L + datum@origin$minute[1L] * 60L + datum@origin$second[1L]
    days <- secs %/% 86400L            # overflow days
    secs <- round(secs %% 86400L, 3L)  # drop overflow days from time, round down to milli-seconds avoid errors

    # Time elements for output
    hrs <- secs %/% 3600L
    mins <- (secs %% 3600L) %/% 60L
    secs <- secs %% 60L

    # Now add days using the calendar of the datum
    origin <- unlist(datum@origin[1L,1L:3L]) # origin ymd as a named vector
    if (any(days > 0)) {
      switch (calendar_id(datum),
              out <- .offset2date_standard(days, origin),
              out <- .offset2date_julian(days, origin),
              out <- .offset2date_360(days, origin),
              out <- .offset2date_fixed(days, origin, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 365),
              out <- .offset2date_fixed(days, origin, c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 366))
    } else {
      out <- data.frame(year = rep(origin[1L], len), month = rep(origin[2L], len), day = rep(origin[3L], len))
    }

    # Put it all back together again
    out$hour <- hrs
    out$minute <- mins
    out$second <- secs
    out$tz <- rep(datum@origin$tz, len)
  } else { # Months, years
    out <- datum@origin[rep(1L, len), ]
    if (unit(datum) == 5L) { # Offsets are months
      months <- out$month + offsets - 1L
      out$month <- months %% 12L + 1L
      out$year <- out$year + months %/% 12L
    } else {               # Offsets are years
      out$year <- out$year + offsets
    }
  }
  out$offset <- offsets
  return(out)
}

#' 360_day, use integer arithmetic
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param x integer. Vector of days to add to the origin.
#' @param origin integer. Vector of year, month, day and seconds to add days to.
#'
#' @returns A data frame with time elements year, month and day in columns and as
#' many rows as the length of vector `x`.
#' @noRd
.offset2date_360 <- function(x, origin) {
  y <- origin[1L] + x %/% 360L
  m <- origin[2L] + (x %% 360L) %/% 30L
  d <- origin[3L] + x %% 30L
  over <- which(d > 30L)
  d[over] <- d[over] - 30L
  m[over] <- m[over] + 1L
  over <- which(m > 12L)
  m[over] <- m[over] - 12L
  y[over] <- y[over] + 1L
  data.frame(year = y, month = m, day = d, row.names = NULL)
}

#' Fixed year length, either 365_day or 366_day
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param x numeric. Vector of days to add to the origin.
#' @param origin numeric. Vector of year, month, day and seconds to add days to.
#' @param month numeric. Vector of days per month in the year.
#' @param ydays numeric. Number of days per year, either 365 or 366.
#'
#' @returns A data frame with time elements year, month and day in columns and as
#' many rows as the length of vector `x`.
#' @noRd
.offset2date_fixed <- function(x, origin, month, ydays) {
  # First process full years over the vector
  yr <- origin[1L] + (x %/% ydays)
  x <- x %% ydays

  # Remaining portion per datum
  x <- x + origin[3L]
  ymd <- mapply(function(y, m, d) {
    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1L
      if (m == 13L) {
        y <- y + 1L
        m <- 1L
      }
    }
    return(c(y, m, d))
  }, yr, origin[2L], x)
  data.frame(year = ymd[1L,], month = ymd[2L,], day = ymd[3L,], row.names = NULL)
}

#' Julian calendar offsetting
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param x numeric. Vector of days to add to the origin.
#' @param origin numeric. Vector of year, month, day and seconds to add days to.
#'
#' @returns A data frame with time elements year, month and day in columns and as
#' many rows as the length of vector `x`.
#' @noRd
.offset2date_julian <- function(x, origin) {
  common_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leap_days   <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Is the leap day to consider ahead in the year from the base date (offset = 0) or in the next year (offset = 1)
  offset <- as.integer(origin[2L] > 2L)

  # First process 4-year cycles of 1,461 days over the vector
  yr <- origin[1L] + (x %/% 1461L) * 4L
  x <- x %% 1461L

  # Remaining portion per datum
  x <- x + origin[3L]
  ymd <- mapply(function(y, m, d) {
    repeat {
      leap <- (y + offset) %% 4L == 0L
      ydays <- 365L + as.integer(leap)

      if (d > ydays) {
        d <- d - ydays
        y <- y + 1L
      } else break
    }

    if (leap) month <- leap_days else month <- common_days

    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1L
      if (m == 13L) {
        y <- y + 1L
        m <- 1L
      }
    }
    return(c(y, m, d))
  }, yr, origin[2L], x)
  data.frame(year = ymd[1L,], month = ymd[2L,], day = ymd[3L,], row.names = NULL)
}

#' Standard calendar offsetting
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param x numeric. Vector of days to add to the origin.
#' @param origin numeric. Vector of year, month, day and seconds to add days to.
#'
#' @returns A data frame with time elements year, month and day in columns and as
#' many rows as the length of vector `x`.
#' @noRd
.offset2date_standard <- function(x, origin) {
  common_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leap_days   <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Is the leap day to consider ahead in the year from the base date (offset = 0) or in the next year (offset = 1)
  offset <- as.integer(origin[2L] > 2L)

  x <- x + origin[3L]
  ymd <- mapply(function(y, m, d) {
    repeat {
      test <- y + offset
      leap <- (test %% 4L == 0L && test %% 100L > 0L) || test %% 400L == 0L
      ydays <- 365L + as.integer(leap)

      if (d > ydays) {
        d <- d - ydays
        y <- y + 1L
      } else break
    }

    if (leap) month <- leap_days else month <- common_days

    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1L
      if (m == 13L) {
        y <- y + 1L
        m <- 1L
      }
    }
    return(c(y, m, d))
  }, origin[1L], origin[2L], x)
  data.frame(year = ymd[1L,], month = ymd[2L,], day = ymd[3L,], row.names = NULL)
}

