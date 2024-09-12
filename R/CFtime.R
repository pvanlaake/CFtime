#' CF Metadata Conventions time representation
#'
#' @slot datum CFdatum. The origin upon which the `offsets` are based.
#' @slot resolution numeric. The average number of time units between offsets.
#' @slot offsets numeric. A vector of offsets from the datum.
#' @slot bounds Optional, the bounds for the offsets. If not set, it is the
#' logical value `FALSE`. If set, it is the logical value `TRUE` if the bounds
#' are regular with respect to the regularly spaced offsets (e.g. successive
#' bounds are contiguous and at mid-points between the offsets); otherwise a
#' `matrix` with columns for `offsets` and low values in the first row, high
#' values in the second row.
#'
#' @returns An object of class CFtime.
#' @export
setClass("CFtime",
         slots = c(
           datum      = "CFdatum",
           resolution = "numeric",
           offsets    = "numeric",
           bounds     = "ANY"
         ))

#' Create a CFtime object
#'
#' This function creates an instance of the `CFtime` class. The arguments to
#' the call are typically read from a CF-compliant data file with climatological
#' observations or climate projections. Specification of arguments can also be
#' made manually in a variety of combinations.
#'
#' @param definition character. A character string describing the time coordinate
#'   of a CF-compliant data file.
#' @param calendar character. A character string describing the calendar to use
#'   with the time dimension definition string. Default value is "standard".
#' @param offsets numeric or character, optional. When numeric, a vector of
#'   offsets from the origin in the time series. When a character vector,
#'   timestamps in ISO8601 or UDUNITS format. When a character string, a
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
    methods::new("CFtime", datum = datum, resolution = NA_real_, offsets = numeric(), bounds = FALSE)
  } else if (is.numeric(offsets)) {
    stopifnot(.validOffsets(offsets, CFt$units$per_day[datum@unit]))

    if (length(offsets) > 1L) {
      resolution <- (max(offsets) - min(offsets)) / (length(offsets) - 1L)
    } else {
      resolution <- NA_real_
    }
    methods::new("CFtime", datum = datum, resolution = resolution, offsets = offsets, bounds = FALSE)
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
    methods::new("CFtime", datum = datum, resolution = resolution, offsets = off, bounds = FALSE)
  } else stop("Invalid offsets for CFtime object")
}

#' @aliases properties
#' @title Properties of a CFtime object
#'
#' @description These functions return the properties of an instance of the
#'   `CFtime` class. The properties are all read-only, but offsets can be added
#'   using the `+` operator.
#'
#' @param cf CFtime. An instance of `CFtime`.
#'
#' @returns `calendar()` and `unit()` return a character string.
#'   `origin()` returns a data frame of timestamp elements with a single row
#'   of data. `timezone()` returns the datum time zone as a character
#'   string. `offsets()` returns a vector of offsets or `NULL` if no offsets
#'   have been set.
#'
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' definition(cf)
#' calendar(cf)
#' unit(cf)
#' timezone(cf)
#' origin(cf)
#' offsets(cf)
#' resolution(cf)

#' @describeIn properties The definition string of the CFtime instance
#' @export
definition <- function(cf) cf@datum@definition

#' @describeIn properties The calendar of the CFtime instance
#' @export
calendar <- function(cf) cf@datum@calendar

#' @describeIn properties The unit of the CFtime instance
#' @export
unit <- function(cf) CFt$units$name[cf@datum@unit]

#' @describeIn properties The origin of the CFtime instance in timestamp elements
#' @export
origin <- function(cf) cf@datum@origin

#' @describeIn properties The time zone of the datum of the CFtime instance as a character string
#' @export
timezone <- function(cf) tz(cf@datum)

#' @describeIn properties The offsets of the CFtime instance as a vector
#' @export
offsets <- function(cf) cf@offsets

#' @describeIn properties The average separation between the offsets in the CFtime instance
#' @export
resolution <- function(cf) cf@resolution

#' Bounds of the time offsets
#'
#' CF-compliant NetCDF files store time information as a single offset value for
#' each step along the dimension, typically centered on the valid interval of
#' the data (e.g. 12-noon for day data). Optionally, the lower and upper values
#' of the valid interval are stored in a so-called "bounds" variable, as an
#' array with two rows (lower and higher value) and a column for each offset.
#' With function `bounds()<-` those bounds can be set for a CFtime instance. The
#' bounds can be retrieved with the `bounds()` function.
#'
#' @param x A `CFtime` instance
#' @param format Optional. A single string with format specifiers, see
#'   [CFtime::format()] for details.
#'
#' @returns If bounds have been set, an array of bounds values with dimensions
#'   (2, length(offsets)). The first row gives the lower bound, the second row
#'   the upper bound, with each column representing an offset of `x`. If the
#'   `format` argument is specified, the bounds values are returned as strings
#'   according to the format. `NULL` when no bounds have been set.
#' @aliases bounds
#'
#' @examples
#' cf <- CFtime("days since 2024-01-01", "standard", seq(0.5, by = 1, length.out = 366))
#' as_timestamp(cf)[1:3]
#' bounds(cf) <- rbind(0:365, 1:366)
#' bounds(cf)[, 1:3]
#' bounds(cf, "%d-%b-%Y")[, 1:3]
setGeneric("bounds", function(x, format) standardGeneric("bounds"), signature = "x")

#' @rdname bounds
#' @export
setMethod("bounds", "CFtime", function (x, format) .get_bounds(x, format))

#' @rdname bounds
#' @param value A `matrix` (or `array`) with dimensions (2, length(offsets))
#'   giving the lower (first row) and higher (second row) bounds of each offset
#'   (this is the format that the CF Metadata Conventions uses for storage in
#'   NetCDF files). Use `FALSE` to unset any previously set bounds, `TRUE` to
#'   set regular bounds at mid-points between the offsets (which must be regular
#'   as well).
setGeneric("bounds<-", function(x, value) standardGeneric("bounds<-"), signature = c("x"))

#' @rdname bounds
#' @export
setMethod("bounds<-", "CFtime", function (x, value) invisible(.set_bounds(x, value)))

#' The length of the offsets contained in the CFtime instance.
#'
#' @param x The CFtime instance whose length will be returned
#'
#' @return The number of offsets in the specified CFtime instance.
#' @export
#'
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' length(cf)
setMethod("length", "CFtime", function(x) length(x@offsets))

#' Return the timestamps contained in the CFtime instance.
#'
#' @param x The CFtime instance whose timestamps will be returned
#'
#' @return The timestamps in the specified CFtime instance.
#' @export
#'
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' as.character(cf)
setMethod("as.character", "CFtime", function(x) {
  if (length(x@offsets) > 0)
    as_timestamp(x)
})

setMethod("show", "CFtime", function(object) {
  noff <- length(object@offsets)
  if (noff == 0L) {
    el <- "  Elements: (no elements)\n"
    b  <- "  Bounds  : (not set)\n"
  } else {
    d <- .ts_extremes(object)
    if (noff > 1L) {
      el <- sprintf("  Elements: [%s .. %s] (average of %f %s between %d elements)\n",
                    d[1L], d[2L], object@resolution, CFt$units$name[object@datum@unit], noff)
    } else {
      el <- paste("  Elements:", d[1L], "\n")
    }
    if (is.logical(object@bounds)) {
      if (object@bounds) b <- "  Bounds  : regular and consecutive\n"
      else b <- "  Bounds  : not set\n"
    } else b <- "  Bounds  : irregular\n"
  }
  cat("CF time series:\n", methods::show(object@datum), el, b, sep = "")
})

#' Format time elements using format specifiers
#'
#' Format timestamps using a specific format string, using the specifiers
#' defined for the [base::strptime()] function, with limitations. The only
#' supported specifiers are `bBdeFhHIjmMpRSTYz%`. Modifiers `E` and `O` are
#' silently ignored. Other specifiers, including their percent sign, are copied
#' to the output as if they were adorning text.
#'
#' The formatting is largely oblivious to locale. The reason for this is that
#' certain dates in certain calendars are not POSIX-compliant and the system
#' functions necessary for locale information thus do not work consistently. The
#' main exception to this is the (abbreviated) names of months (`bB`), which
#' could be useful for pretty printing in the local language. For separators and
#' other locale-specific adornments, use local knowledge instead of depending on
#' system locale settings; e.g. specify `%m/%d/%Y` instead of `%D`.
#'
#' Week information, including weekday names, is not supported at all as a
#' "week" is not defined for non-standard CF calendars and not generally useful
#' for climate projection data. If you are working with observed data and want
#' to get pretty week formats, use the [as_timestamp()] function to generate
#' `POSIXct` timestamps (observed data generally uses a standard calendar) and
#' then use the [base::format()] function which supports the full set of
#' specifiers.
#'
#' @param x CFtime. A CFtime instance whose offsets will be returned as
#'   timestamps.
#' @param format character. A character string with strptime format
#'   specifiers. If omitted, the most economical format will be used: a full
#'   timestamp when time information is available, a date otherwise.
#'
#' @returns A vector of character strings with a properly formatted timestamp.
#'   Any format specifiers not recognized or supported will be returned verbatim.
#' @export
#'
#' @examples
#' cf <- CFtime("days since 2020-01-01", "standard", 0:365)
#' format(cf, "%Y-%b")
#'
#' # Use system facilities on a standard calendar
#' format(as_timestamp(cf, asPOSIX = TRUE), "%A, %x")
#'
setMethod("format", "CFtime", function(x, format) {
  if (!requireNamespace("stringr", quietly = TRUE))
    stop("package `stringr` is required - please install it first") # nocov

  if (missing(format)) format <- ""
  else if (!is.character(format) || length(format) != 1)
    stop("`format` argument must be a character string with formatting specifiers")

  ts <- .offsets2time(x@offsets, x@datum)
  if (nrow(ts) == 0L) return()

  .format_format(ts, tz(x@datum), format)
})

#' Create a factor for a CFtime instance
#'
#' Method for [base::cut()] applied to CFtime objects.
#'
#' When `breaks` is one of `"year", "season", "quarter", "month", "dekad",
#' "day"` a factor is generated like by [CFfactor()].
#'
#' When `breaks` is a vector of character timestamps a factor is produced with a
#' level for every interval between timestamps. The last timestamp, therefore,
#' is only used to close the interval started by the pen-ultimate timestamp -
#' use a distant timestamp (e.g. `range(x)[2]`) to ensure that all offsets to
#' the end of the CFtime time series are included, if so desired. The last
#' timestamp will become the upper bound in the CFtime instance that is returned
#' as an attribute to this function so a sensible value for the last timestamp
#' is advisable. The earliest timestamp cannot be earlier than the origin of the
#' datum of `x`.
#'
#' This method works similar to [base::cut.POSIXt()] but there are some
#' differences in the arguments: for `breaks` the set of options is different
#' and no preceding integer is allowed, `labels` are always assigned using
#' values of `breaks`, and the interval is always left-closed.
#'
#' @param x An instance of CFtime.
#' @param breaks A character string of a factor period (see [CFfactor()] for a
#'   description), or a character vector of timestamps that conform to the
#'   calendar of `x`, with a length of at least 2. Timestamps must be given in
#'   ISO8601 format, e.g. "2024-04-10 21:31:43".
#' @param ... Ignored.
#'
#' @returns A factor with levels according to the `breaks` argument, with
#'   attributes 'period', 'epoch' and 'CFtime'. When `breaks` is a factor
#'   period, attribute 'period' has that value, otherwise it is '"day"'. When
#'   `breaks` is a character vector of timestamps, attribute 'CFtime' holds an
#'   instance of CFtime that has the same definition as `x`, but with (ordered)
#'   offsets generated from the `breaks`. Attribute 'epoch' is always -1.
#' @aliases cut
#' @seealso [CFfactor()] produces a factor for several fixed periods, including
#'   for epochs.
#' @export
#'
#' @examples
#' x <- CFtime("days since 2021-01-01", "365_day", 0:729)
#' breaks <- c("2022-02-01", "2021-12-01", "2023-01-01")
#' cut(x, breaks)
setMethod("cut", "CFtime", function (x, breaks, ...) {
  if (!inherits(x, "CFtime"))
    stop("Argument 'x' must be a CFtime instance")

  if (missing(breaks) || !is.character(breaks) || (len <- length(breaks)) < 1)
    stop("Argument 'breaks' must be a character vector with at least 1 value")

  if(len == 1) {
    breaks <- sub("s$", "", tolower(breaks))
    if (breaks %in% CFt$factor_periods)
      return(CFfactor(x, breaks))
    else stop("Invalid specification of 'breaks'")
  }

  # breaks is a character vector of multiple timestamps
  if (x@datum@unit > 4L) stop("Factorizing on a 'month' or 'year' datum is not supported")
  time <- .parse_timestamp(x@datum, breaks)
  if (anyNA(time$year))
    stop("Invalid specification of 'breaks'")
  sorted <- order(time$offset)
  ooff <- time$offset[sorted]
  intv <- findInterval(offsets(x), ooff)
  intv[which(intv %in% c(0L, len))] <- NA
  f <- factor(intv, labels = breaks[sorted][1L:(len-1L)])

  # Attributes
  bnds <- rbind(ooff[1L:(len-1L)], ooff[2L:len])
  off  <- bnds[1L, ] + (bnds[2L, ] - bnds[1L, ]) * 0.5
  cf <- CFtime(x@datum@definition, x@datum@calendar, off)
  bounds(cf) <- bnds
  attr(f, "period") <- "day"
  attr(f, "epoch")  <- -1L
  attr(f, "CFtime") <- cf
  f
})

setGeneric("indexOf", function(x, y, ...) standardGeneric("indexOf"), signature = c("x", "y"))

#' Find the index of timestamps in the time series
#'
#' In the CFtime instance `y`, find the index in the time series for each
#' timestamp given in argument `x`. Values of `x` that are before the earliest
#' value in `y` will be returned as `0` (except when the value is before the
#' datum of `y`, in which case the value returned is `NA`); values of `x` that
#' are after the latest values in `y` will be returned as
#' `.Machine$integer.max`. Alternatively, when `x` is a numeric vector of index
#' values, return the valid indices of the same vector, with the side effect
#' being the attribute "CFtime" associated with the result.
#'
#' Timestamps can be provided as vectors of character strings, `POSIXct` or
#' `Date.`
#'
#' Matching also returns index values for timestamps that fall between two
#' elements of the time series - this can lead to surprising results when time
#' series elements are positioned in the middle of an interval (as the CF
#' Metadata Conventions instruct us to "reasonably assume"): a time series of
#' days in January would be encoded in a netCDF file as
#' `c("2024-01-01 12:00:00", "2024-01-02 12:00:00", "2024-01-03 12:00:00", ...)`
#' so `x <- c("2024-01-01", "2024-01-02", "2024-01-03")` would result in
#' `(NA, 1, 2)` (or `(NA, 1.5, 2.5)` with `method = "linear"`) because the date
#' values in `x` are at midnight. This situation is easily avoided by ensuring
#' that `y` has bounds set (use `bounds(y) <- TRUE` as a proximate solution if
#' bounds are not stored in the netCDF file). See the Examples.
#'
#' If bounds are set, the indices are taken from those bounds. Returned indices
#' may fall in between bounds if the latter are not contiguous, with the
#' exception of the extreme values in `x`.
#'
#' Values of `x` that are not valid timestamps according to the calendar of `y`
#' will be returned as `NA`.
#'
#' `x` can also be a numeric vector of index values, in which case the valid
#' values in `x` are returned. If negative values are passed, the positive
#' counterparts will be excluded and then the remainder returned. Positive and
#' negative values may not be mixed. Using a numeric vector has
#' the side effect that the result has the attribute "CFtime" describing the
#' temporal dimension of the slice. If index values outside of the range of `y`
#' (`1:length(y)`) are provided, an error will be thrown.
#'
#' @param x Vector of character, POSIXt or Date values to find indices for, or a
#'   numeric vector.
#' @param y CFtime instance.
#' @param method Single value of "constant" or "linear". If `"constant"` or when
#'   bounds are set on argument `y`, return the index value for each match. If
#'   `"linear"`, return the index value with any fractional value.
#'
#' @returns A numeric vector giving indices into the "time" dimension of the
#'   dataset associated with `y` for the values of `x`. If there is at least 1
#'   valid index, then attribute "CFtime"
#'   contains an instance of CFtime that describes the dimension of filtering
#'   the dataset associated with `y` with the result of this function, excluding
#'   any `NA`, `0` and `.Machine$integer.max` values.
#' @aliases indexOf
#' @export
#'
#' @examples
#' cf <- CFtime("days since 2020-01-01", "360_day", 1440:1799 + 0.5)
#' as_timestamp(cf)[1:3]
#' x <- c("2024-01-01", "2024-01-02", "2024-01-03")
#' indexOf(x, cf)
#' indexOf(x, cf, method = "linear")
#'
#' bounds(cf) <- TRUE
#' indexOf(x, cf)
#'
#' # Non-existent calendar day in a `360_day` calendar
#' x <- c("2024-03-30", "2024-03-31", "2024-04-01")
#' indexOf(x, cf)
#'
#' # Numeric x
#' indexOf(c(29, 30, 31), cf)
setMethod("indexOf", c("ANY", "CFtime"), function(x, y, method = "constant") {
  stopifnot(inherits(x, c("character", "POSIXt", "Date")) || is.numeric(x),
            method %in% c("constant", "linear"))

  if (is.numeric(x)) {
    if (!(all(x < 0, na.rm = TRUE) || all(x > 0, na.rm = TRUE)))
      stop("Cannot mix positive and negative index values")

    intv <- (1:length(y))[x]
    xoff <- y@offsets[x]
  } else {
    if (y@datum@unit > 4L)
      stop("Parsing of timestamps on a \"month\" or \"year\" datum is not supported.")

    xoff <- .parse_timestamp(y@datum, as.character(x))$offset
    vals <- .get_bounds(y)
    if (is.null(vals)) vals <- offsets(y)
    else vals <- c(vals[1L, 1L], vals[2L, ])
    intv <- stats::approx(vals, 1L:length(vals), xoff, method = method,
                          yleft = 0, yright = .Machine$integer.max)$y
    intv[which(intv == length(vals))] <- .Machine$integer.max
  }

  valid <- which(!is.na(intv) & intv > 0 & intv < .Machine$integer.max)
  if (any(valid)) {
    cf <- CFtime(definition(y), calendar(y), xoff[valid])
    yb <- bounds(y)
    if (!is.null(yb))
      bounds(cf) <- yb[, intv[valid]]
    attr(intv, "CFtime") <- cf
  }
  intv
})

#' @title Extreme time series values
#'
#' @description Character representation of the extreme values in the time series
#'
#' @param x An instance of the `CFtime` class.
#' @param format A character string with format specifiers, optional. If it is
#' missing or an empty string, the most economical ISO8601 format is chosen:
#' "date" when no time information is present in `x`, "timestamp" otherwise.
#' Otherwise a suitable format specifier can be provided.
#' @param bounds Logical to indicate if the extremes from the bounds should be
#' used, if set. Defaults to `FALSE`.
#' @param ... Ignored.
#' @param na.rm Ignored.
#'
#' @returns Vector of two character representations of the extremes of the time series.
#' @export
#' @examples
#' cf <- CFtime("days since 1850-01-01", "julian", 0:364)
#' range(cf)
#' range(cf, "%Y-%b-%e")
setMethod("range", "CFtime", function(x, format = "", bounds = FALSE, ..., na.rm = FALSE)
  .ts_extremes(x, format, bounds, ..., na.rm))

#' Indicates if the time series is complete
#'
#' This function indicates if the time series is complete, meaning that the time
#' steps are equally spaced and there are thus no gaps in the time series.
#'
#' This function gives exact results for time series where the nominal
#' *unit of separation* between observations in the time series is exact in terms of the
#' datum unit. As an example, for a datum unit of "days" where the observations
#' are spaced a fixed number of days apart the result is exact, but if the same
#' datum unit is used for data that is on a monthly basis, the *assessment* is
#' approximate because the number of days per month is variable and dependent on
#' the calendar (the exception being the `360_day` calendar, where the
#' assessment is exact). The *result* is still correct in most cases (including
#' all CF-compliant data sets that the developers have seen) although there may
#' be esoteric constructions of CFtime and offsets that trip up this
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
#' is_complete(cf)
is_complete <- function(x) {
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
#' If bounds were set these will be preserved.
#'
#' @param x CFtime. The time series to operate on.
#' @param extremes character. Vector of two timestamps that represent the
#'   extremes of the time period of interest. The timestamps must be in
#'   increasing order. The timestamps need not fall in the range of the time
#'   steps in the CFtime stance.
#' @param rightmost.closed Is the larger extreme value included in the result?
#'   Default is `FALSE`.
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
#' slab(cf, c("2022-12-01", "2023-01-01 03:00"))
slab <- function(x, extremes, rightmost.closed = FALSE) {
  if (!methods::is(x, "CFtime")) stop("First argument must be an instance of CFtime")
  if (!is.character(extremes) || length(extremes) != 2L)
    stop("Second argument must be a character vector of two timestamps")
  if (extremes[2L] < extremes[1L]) extremes <- c(extremes[2L], extremes[1L])
  .ts_slab(x, extremes, rightmost.closed)
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
#' Any bounds that were set will be removed. Use [CFtime::bounds()] to retrieve
#' the bounds of the individual `CFtime` instances and then set them again after
#' merging the two instances.
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
    CFtime(e1@datum@definition, e1@datum@calendar, c(e1@offsets, e2@offsets))
  else {
    diff <- .parse_timestamp(e1@datum, paste(origin_date(e2@datum), origin_time(e2@datum)))$offset
    if (is.na(diff)) {
      diff <- .parse_timestamp(e2@datum, paste(origin_date(e1@datum), origin_time(e1@datum)))$offset
      CFtime(e2@datum@definition, e2@datum@calendar, c(e1@offsets + diff, e2@offsets))
    } else
      CFtime(e1@datum@definition, e1@datum@calendar, c(e1@offsets, e2@offsets + diff))
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
#' but rather do `CFtime(definition, calendar) + c(e1, e2, e3, ...)`. It is the
#' responsibility of the operator to ensure that the offsets of the different
#' data sets are in reference to the same datum.
#'
#' Note also that `RNetCDF` and `ncdf4` packages both return the values of the
#' "time" dimension as a 1-dimensional array. You have to `dim(time_values) <- NULL`
#' to de-class the array to a vector before adding offsets to an existing CFtime
#' instance.
#'
#' Negative offsets will generate an error.
#'
#' Any bounds that were set will be removed. Use [CFtime::bounds()] to retrieve
#' the bounds of the individual `CFtime` instances and then set them again after
#' merging the two instances.
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
  if (.validOffsets(e2, CFt$units$per_day[e1@datum@unit]))
    CFtime(e1@datum@definition, e1@datum@calendar, c(e1@offsets, e2))
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
#' @param format character. Value of "date" or "timestamp". Optionally, a
#' character string that specifies an alternate format.
#'
#' @returns Vector of two character strings that represent the starting and
#'   ending timestamps in the time series. If a `format` is supplied, that
#'   format will be used. Otherwise, if all of the timestamps in the time series
#'   have a time component of `00:00:00` the date of the timestamp is returned,
#'   otherwise the full timestamp (without any time zone information).
#'
#' @noRd
.ts_extremes <- function(x, format = "", bounds = FALSE, ..., na.rm) {
  if (length(x@offsets) == 0L) return(c(NA_character_, NA_character_))
  if (!missing(format) && ((!is.character(format)) || length(format) != 1L))
    stop("`format` argument, when present, must be a character string with formatting specifiers")
  if (!is.logical(bounds) || length(bounds) != 1L)
    stop("`bounds` argument, when present, must be a single logical value")

  if (bounds) {
    bnds <- .get_bounds(x)
    if (is.null(bnds)) time <- .offsets2time(range(x@offsets), x@datum)
    else time <- .offsets2time(c(bnds[1L, 1L], bnds[2L, length(x)]), x@datum)
  } else time <- .offsets2time(range(x@offsets), x@datum)

  .format_format(time, tz(x@datum), format)
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
#' @param closed Is the right side closed, i.e. included in the result?
#'
#' @returns A logical vector with a length equal to the number of time steps in
#'   `x` with values `TRUE` for those time steps that fall between the two
#'   extreme values, `FALSE` otherwise. The earlier timestamp is included, the
#'   later timestamp is excluded. A specification of `c("2022-01-01", "2023-01-01)`
#'   will thus include all time steps that fall in the year 2022.
#'
#'   An attribute 'CFtime' will have the same definition as `x` but with offsets
#'   corresponding to the time steps falling between the two extremes. If there
#'   are no values between the extremes, the attribute is `NULL`.
#' @noRd
.ts_slab <- function(x, extremes, closed) {
  ext <- .parse_timestamp(x@datum, extremes)$offset
  if (is.na(ext[1L])) ext[1L] <- 0
  off <- x@offsets
  if (ext[1L] > max(off) || is.na(ext[2L])) {
    out <- rep(FALSE, length(off))
    attr(out, "CFtime") <- NULL
  } else {
    out <- if (closed) off >= ext[1L] & off <= ext[2L]
                  else off >= ext[1L] & off < ext[2L]
    cf <- CFtime(x@datum@definition, x@datum@calendar, off[out])
    xb <- bounds(x)
    if (!is.null(xb))
      bounds(cf) <- xb[, out]
    attr(out, "CFtime") <- cf
  }
  out
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

  if (datum@unit <= 4L) { # Days, hours, minutes, seconds
    # First add time: convert to seconds first, then recompute time parts
    secs <- offsets * CFt$units$seconds[datum@unit]
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
      switch (datum@cal_id,
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
    out$tz <- rep(tz(datum), len)
  } else { # Months, years
    out <- datum@origin[rep(1L, len), ]
    if (datum@unit == 5L) { # Offsets are months
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

