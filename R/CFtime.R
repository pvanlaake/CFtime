#' CF Metadata Conventions time representation
#'
#' @slot datum CFdatum. The atomic origin upon which the `offsets` are based.
#' @slot resolution numeric. The average number of time units between offsets.
#' @slot time data.frame. Data frame of numeric date elements year, month, day,
#' hour, minute, second and time zone from offsets defined by the `datum`, as
#' well as the offset itself.
#'
#' @returns An object of class CFtime.
#' @export
setClass("CFtime",
         slots = c(
           datum      = "CFdatum",
           resolution = "numeric",
           time       = "data.frame"
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
  datum <- CFdatum(definition, calendar)

  if (is.array(offsets)) dim(offsets) <- NULL

  if (is.null(offsets)) {
    methods::new("CFtime", datum = datum, resolution = NA_real_,
                 time = data.frame(c(year = integer(), month = integer(), day = integer(),
                                     hour = integer(), minute = integer(), second = numeric(),
                                     tz = character(), offset = numeric())))
  } else if (methods::is(offsets, "numeric")) {
    stopifnot(.validOffsets(offsets))

    if (length(offsets) > 1) {
      resolution <- (max(offsets) - min(offsets)) / (length(offsets) - 1)
    } else {
      resolution <- NA_real_
    }
    time <- .add_offsets(offsets, datum)
    methods::new("CFtime", datum = datum, resolution = resolution, time = time)
  } else if (methods::is(offsets, "character")) {
    time <- .parse_timestamp(datum, offsets)
    if (any(is.na(time$year))) stop("Offset argument contains invalid timestamps")

    if (length(offsets) == 1) {
      ts <- seq(0, time$offset)
      time <- .add_offsets(ts, datum)
      resolution <- 1
    } else resolution <- (max(time$offset) - min(time$offset)) / (length(time$offset) - 1)
    methods::new("CFtime", datum = datum, resolution = resolution, time = time)
  } else stop("Invalid offsets for CFtime object")
}

#' @aliases CFproperties
#' @title Properties of a CFtime object
#'
#' @description These functions return the properties of an instance of the `CFtime` class.
#' The properties are all read-only, but offsets can be added using the `+`
#' operator.
#'
#' @param cf CFtime. An instance of `CFtime`.
#'
#' @returns `CFcalendar()` and `CFunit()` return an atomic character string.
#' `CForigin()` returns a data frame of timestamp elements with a single row of
#' data. `CFoffsets()` returns a vector of offsets or `NULL` if no offsets have
#' been set.
#'
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' CFdefinition(cf)
#' CFcalendar(cf)
#' CFunit(cf)
#' CForigin(cf)
#' CFoffsets(cf)

#' @describeIn CFproperties The defintion string of the CFtime instance
#' @export
CFdefinition <- function(cf) cf@datum@definition

#' @describeIn CFproperties The calendar of the CFtime instance
#' @export
CFcalendar <- function(cf) cf@datum@calendar

#' @describeIn CFproperties The unit of the CFtime instance
#' @export
CFunit <- function(cf) CFt$units$name[cf@datum@unit]

#' @describeIn CFproperties The origin of the CFtime instance in timestamp elements
#' @export
CForigin <- function(cf) cf@datum@origin

#' @describeIn CFproperties The offsets of the CFtime instance as a vector
#' @export
CFoffsets <- function(cf) cf@time$offset

setMethod("show", "CFtime", function(object) {
  if (nrow(object@time) == 0) {
    el <- "  Elements: (no elements)\n"
  } else {
    d <- CFrange(object)
    if (nrow(object@time) > 1) {
      el <- sprintf("  Elements: [%s .. %s] (average of %f %s between %d elements)\n",
                    d[1], d[2], object@resolution, CFt$units$name[object@datum@unit], nrow(object@time))
    } else {
      el <- paste("  Elements:", d[1], "\n")
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
#'
#' @examples
#' e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
#' e2 <- CFtime("days since 1850-01-01 00:00:00", "standard", 0:364)
#' e1 == e2
setMethod("==", c("CFtime", "CFtime"), function(e1, e2)
  .datum_equivalent(e1@datum, e2@datum) &&
  nrow(e1@time) == nrow(e2@time) &&
  all(e1@time$offset == e2@time$offset))

#' Merge two CFtime objects
#'
#' Two `CFtime` instances can be merged into one with this operator, provided that
#' the datums of the two instances are equivalent.
#'
#' The order of the two parameters is indirectly significant. The resulting `CFtime`
#' will have the offsets of both instances in the order that they are specified.
#' There is no reordering or removal of duplicates. This is because the time
#' series are usually associated with a data set and the correspondence between
#' the two is thus preserved. When merging the data sets described by this time
#' series, the order must be identical to the ordering here.
#'
#' @param e1,e2 CFtime. Instances of the `CFtime` class.
#'
#' @returns A `CFtime` object with a set of offsets equal to the offsets of the
#' instances of `CFtime` that the operator operates on. If the datums of the `CFtime`
#' instances are not equivalent, an error is thrown.
#' @export
#'
#' @examples
#' e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
#' e2 <- CFtime("days since 1850-01-01 00:00:00", "standard", 365:729)
#' e1 + e2
setMethod("+", c("CFtime", "CFtime"), function(e1, e2)
  if (.datum_equivalent(e1@datum, e2@datum))
    CFtime(e1@datum@definition, e1@datum@calendar, c(e1@time$offset, e2@time$offset))
  else stop('Datums not equivalent'))

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
#'
#' @examples
#' e1 <- CFtime("days since 1850-01-01", "gregorian", 0:364)
#' e2 <- 365:729
#' e1 + e2
setMethod("+", c("CFtime", "numeric"), function(e1, e2) {if (.validOffsets(e2)) CFtime(e1@datum@definition, e1@datum@calendar, c(e1@time$offset, e2))})

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
.validOffsets <- function(offsets) {
  if (any(is.na(offsets) | (offsets < 0))) stop("Offsets cannot contain negative or `NA` values.")
  TRUE
}

#' Return the extremes of the time series as character strings
#'
#' This function returns the first and last timestamp of the time series as a
#' vector. Note that the time series is not necessarily sorted.
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
  if (nrow(x@time) == 0) return(c(NA_character_, NA_character_))

  mn <- which.min(x@time$offset)
  mx <- which.max(x@time$offset)
  if (sum(x@time$hour, x@time$minute, x@time$second) == 0) { # all times are 00:00:00
    return(c(sprintf("%04d-%02d-%02d", x@time$year[mn], x@time$month[mn], x@time$day[mn]),
             sprintf("%04d-%02d-%02d", x@time$year[mx], x@time$month[mx], x@time$day[mx])))
  } else {
    t <- .format_time(x@time[c(mn, mx), ])
    return(c(sprintf("%04d-%02d-%02dT%s", x@time$year[mn], x@time$month[mn], x@time$day[mn], t[1]),
             sprintf("%04d-%02d-%02dT%s", x@time$year[mx], x@time$month[mx], x@time$day[mx], t[2])))
  }
}

#' Decompose a vector of offsets, in units of the datum, to their timestamp values
#'
#' This function adds a specified amount of time to the origin of a CFts object.
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' This functions introduces an error where the datum unit is "months" or "years",
#' due to the ambiguous definition of these units.
#'
#' @param offsets numeric. Vector of offsets to add to the datum.
#' @param datum CFdatum. The datum that defines the unit of the offsets and the
#' origin to add the offsets to.
#'
#' @returns A data.frame with columns for the timestamp elements and the offset
#' value and as many rows as there are offsets.
#' @noRd
.add_offsets <- function(offsets, datum) {
  len <- length(offsets)

  # First add time: convert to seconds first, then recompute time parts
  secs <- offsets * CFt$units$seconds[datum@unit]
  secs <- secs + datum@origin$hour[1] * 3600 + datum@origin$minute[1] * 60 + datum@origin$second[1]
  days <- secs %/% 86400            # overflow days
  secs <- round(secs %% 86400, 3)   # drop overflow days from time, round down to milli-seconds avoid errors

  # Time elements for output
  hrs <- secs %/% 3600
  mins <- (secs %% 3600) %/% 60
  secs <- secs %% 60

  # Now add days using the calendar of the datum
  origin <- unlist(datum@origin[1,1:3]) # origin ymd as a named vector
  if (any(days > 0)) {
    switch (datum@cal_id,
            out <- .offset2date_standard(days, origin),
            out <- .offset2date_julian(days, origin),
            out <- .offset2date_360(days, origin),
            out <- .offset2date_fixed(days, origin, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 365),
            out <- .offset2date_fixed(days, origin, c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 366))
  } else {
    out <- data.frame(year = rep(origin[1], len), month = rep(origin[2], len), day = rep(origin[3], len))
  }

  # Put it all back together again
  out$hour <- hrs
  out$minute <- mins
  out$second <- secs
  out$tz <- rep(datum@origin$tz, len)
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
  y <- origin[1] + x %/% 360
  m <- origin[2] + (x %% 360) %/% 30
  d <- origin[3] + x %% 30
  over <- which(d > 30)
  d[over] <- d[over] - 30
  m[over] <- m[over] + 1
  over <- which(m > 12)
  m[over] <- m[over] - 12
  y[over] <- y[over] + 1
  data.frame(year = y, month = m, day = d)
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
  yr <- origin[1] + (x %/% ydays)
  x <- x %% ydays

  # Remaining portion per datum
  x <- x + origin[3]
  ymd <- mapply(function(y, m, d) {
    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1
      if (m == 13) {
        y <- y + 1
        m <- 1
      }
    }
    return(c(y, m, d))
  }, yr, origin[2], x)
  data.frame(year = ymd[1,], month = ymd[2,], day = ymd[3,])
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
  offset <- as.integer(origin[2] > 2)

  # First process 4-year cycles of 1,461 days over the vector
  yr <- origin[1] + (x %/% 1461) * 4
  x <- x %% 1461

  # Remaining portion per datum
  x <- x + origin[3]
  ymd <- mapply(function(y, m, d) {
    repeat {
      leap <- (y + offset) %% 4 == 0
      ydays <- 365 + as.integer(leap)

      if (d > ydays) {
        d <- d - ydays
        y <- y + 1
      } else break
    }

    if (leap) month <- leap_days else month <- common_days

    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1
      if (m == 13) {
        y <- y + 1
        m <- 1
      }
    }
    return(c(y, m, d))
  }, yr, origin[2], x)
  data.frame(year = ymd[1,], month = ymd[2,], day = ymd[3,])
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
  offset <- as.integer(origin[2] > 2)

  x <- x + origin[3]
  ymd <- mapply(function(y, m, d) {
    repeat {
      test <- y + offset
      leap <- (test %% 4 == 0 && test %% 100 > 0) || test %% 400 == 0
      ydays <- 365 + as.integer(leap)

      if (d > ydays) {
        d <- d - ydays
        y <- y + 1
      } else break
    }

    if (leap) month <- leap_days else month <- common_days

    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1
      if (m == 13) {
        y <- y + 1
        m <- 1
      }
    }
    return(c(y, m, d))
  }, origin[1], origin[2], x)
  data.frame(year = ymd[1,], month = ymd[2,], day = ymd[3,])
}

