#' @title CFTime class
#'
#' @description This class manages the "time" dimension of netCDF files that
#' follow the CF Metadata Conventions, and its productive use in R.
#'
#' The class has a field `cal` which holds a specific calendar from the
#' allowed types (9 named calendars are currently supported). The calendar is
#' also implemented as a (hidden) class which converts netCDF file encodings to
#' timestamps as character strings, and vice-versa. Bounds information (the
#' period of time over which a timestamp is valid) is used when defined in the
#' netCDF file.
#'
#' Additionally, this class has functions to ease use of the netCDF "time"
#' information when processing data from netCDF files. Filtering and indexing of
#' time values is supported, as is the generation of factors.
#'
#' @export
#' @references
#'   https://cfconventions.org/Data/cf-conventions/cf-conventions-1.12/cf-conventions.html#time-coordinate
#' @docType class
CFTime <- R6::R6Class("CFTime",
  public = list(
    #' @field cal The calendar of this `CFTime` instance, a descendant of the
    #' [CFCalendar] class.
    cal = NULL,

    #' @field offsets A numeric vector of offsets from the origin of the
    #'   calendar.
    offsets = numeric(),

    #' @field resolution The average number of time units between offsets.
    resolution = NA_real_,

    #' @field bounds Optional, the bounds for the offsets. If not set, it is the
    #'   logical value `FALSE`. If set, it is the logical value `TRUE` if the
    #'   bounds are regular with respect to the regularly spaced offsets (e.g.
    #'   successive bounds are contiguous and at mid-points between the
    #'   offsets); otherwise a `matrix` with columns for `offsets` and low
    #'   values in the first row, high values in the second row. Use
    #'   `get_bounds()` to get bounds values when they are regularly spaced.
    bounds = FALSE,

    #' @description Create a new instance of this class.
    #' @param definition Character string of the units and origin of the
    #'   calendar.
    #' @param calendar Character string of the calendar to use. Must be one of
    #'   the values permitted by the CF Metadata Conventions. If `NULL`, the
    #'   "standard" calendar will be used.
    #' @param offsets Numeric or character vector, optional. When numeric, a
    #'   vector of offsets from the origin in the time series. When a character
    #'   vector of length 2 or more, timestamps in ISO8601 or UDUNITS format.
    #'   When a character string, a timestamp in ISO8601 or UDUNITS format and
    #'   then a time series will be generated with a separation between steps
    #'   equal to the unit of measure in the definition, inclusive of the
    #'   definition timestamp. The unit of measure of the offsets is defined by
    #'   the `definition` argument.
    initialize = function(definition, calendar, offsets = NULL) {
      if (is.null(calendar)) calendar <- "standard" # This may occur when "calendar" attribute is not defined in the NC file
      calendar <- tolower(calendar)
      self$cal <- switch(calendar,
        "standard" = CFCalendarStandard$new(calendar, definition),
        "gregorian" = CFCalendarStandard$new(calendar, definition),
        "proleptic_gregorian" = CFCalendarProleptic$new(calendar, definition),
        "tai" = CFCalendarTAI$new(calendar, definition),
        "utc" = CFCalendarUTC$new(calendar, definition),
        "julian" = CFCalendarJulian$new(calendar, definition),
        "360_day" = CFCalendar360$new(calendar, definition),
        "365_day" = CFCalendar365$new(calendar, definition),
        "noleap" = CFCalendar365$new(calendar, definition),
        "366_day" = CFCalendar366$new(calendar, definition),
        "all_leap" = CFCalendar366$new(calendar, definition),
        stop("Invalid calendar specification", call. = FALSE)
      )

      if (is.null(offsets)) return()

      if (is.numeric(offsets)) {
        dim(offsets) <- NULL
        stopifnot(.validOffsets(offsets))

        if (length(offsets) > 1L) {
          self$resolution <- (max(offsets) - min(offsets)) / (length(offsets) - 1L)
          if (any(diff(offsets) <= 0))
            warning("Offsets not monotonically increasing.", call. = FALSE)
        } else {
          self$resolution <- NA_real_
        }
        self$offsets <- as.numeric(offsets)
      } else if (is.character(offsets)) {
        time <- self$cal$parse(offsets)
        if (anyNA(time$year)) stop("Argument `offsets` contains invalid timestamps", call. = FALSE) # nocov

        if (length(offsets) == 1L) {
          self$offsets <- seq(0L, time$offset[1L])
          self$resolution <- 1
        } else {
          self$offsets <- time$offset
          self$resolution <- (max(self$offsets) - min(self$offsets)) / (length(self$offsets) - 1L)
          if (any(diff(self$offsets) <= 0))
            warning("Offsets not monotonically increasing.", call. = FALSE)
        }
      } else if (!is.null(offsets)) stop("Invalid offsets for CFTime object", call. = FALSE)
    },

    #' @description Print a summary of the `CFTime` object to the console.
    #' @param ... Ignored.
    #' @return `self` invisibly.
    print = function(...) {
      noff <- length(self$offsets)
      if (noff == 0L) {
        el <- "  Elements: (no elements)\n"
        b  <- "  Bounds  : (not set)\n"
      } else {
        d <- self$range()
        el <- if (noff > 1L) {
          sprintf("  Elements: [%s .. %s] (average of %f %s between %d elements)\n",
                 d[1L], d[2L], self$resolution, CFt$units$name[self$cal$unit], noff)
        } else
          paste("  Elements:", d[1L], "\n")

        b <- if (is.logical(self$bounds)) {
          if (self$bounds) "  Bounds  : regular and consecutive\n"
          else "  Bounds  : not set\n"
        } else if (noff == 1L) "  Bounds  : set\n"
        else "  Bounds  : irregular\n"
      }
      cal <- capture.output(self$cal$print())
      cat(paste(cal, collapse = "\n"), "\nTime series:\n",  el, b, sep = "")
      invisible(self)
    },

    #' @description This method returns the first and last timestamp of the time
    #'   series as a vector. Note that the offsets do not have to be sorted.
    #'
    #' @param format Value of "date" or "timestamp". Optionally, a
    #'   character string that specifies an alternate format.
    #' @param bounds Logical to indicate if the extremes from the bounds should
    #'   be used, if set. Defaults to `FALSE`.
    #'
    #' @return Vector of two character strings that represent the starting and
    #'   ending timestamps in the time series. If a `format` is supplied, that
    #'   format will be used. Otherwise, if all of the timestamps in the time
    #'   series have a time component of `00:00:00` the date of the timestamp is
    #'   returned, otherwise the full timestamp (without any time zone
    #'   information).
    range = function(format = "", bounds = FALSE) {
      if (length(self$offsets) == 0L) return(c(NA_character_, NA_character_))
      if (!missing(format) && ((!is.character(format)) || length(format) != 1L))
        stop("`format` argument, when present, must be a character string with formatting specifiers", call. = FALSE) # nocov
      if (!is.logical(bounds) || length(bounds) != 1L)
        stop("`bounds` argument, when present, must be a single logical value", call. = FALSE) # nocov

      if (bounds) {
        bnds <- self$get_bounds()
        if (is.null(bnds)) time <- self$cal$offsets2time(base::range(self$offsets))
        else time <- self$cal$offsets2time(c(bnds[1L, 1L], bnds[2L, length(self$offsets)]))
      } else time <- self$cal$offsets2time(base::range(self$offsets))

      .format_format(time, self$cal$timezone, format)
    },

    #' @description This method generates a vector of character strings or
    #'   `POSIXct`s that represent the date and time in a selectable combination
    #'   for each offset.
    #'
    #'   The character strings use the format `YYYY-MM-DDThh:mm:ss±hhmm`,
    #'   depending on the `format` specifier. The date in the string is not
    #'   necessarily compatible with `POSIXt` - in the `360_day` calendar
    #'   `2017-02-30` is valid and `2017-03-31` is not.
    #'
    #'   For the "proleptic_gregorian" calendar the output can also be generated
    #'   as a vector of `POSIXct` values by specifying `asPOSIX = TRUE`. The
    #'   same is possible for the "standard" and "gregorian" calendars but only
    #'   if all timestamps fall on or after 1582-10-15. If `asPOSIX = TRUE` is
    #'   specified while the calendar does not support it, an error will be
    #'   generated.
    #'
    #' @param format character. A character string with either of the values
    #'   "date" or "timestamp". If the argument is not specified, the format
    #'   used is "timestamp" if there is time information, "date" otherwise.
    #' @param asPOSIX logical. If `TRUE`, for "standard", "gregorian" and
    #'   "proleptic_gregorian" calendars the output is a vector of `POSIXct` -
    #'   for other calendars an error will be thrown. Default value is `FALSE`.
    #'
    #' @return A character vector where each element represents a moment in
    #'   time according to the `format` specifier.
    as_timestamp = function(format = NULL, asPOSIX = FALSE) {
      if (asPOSIX && !self$cal$POSIX_compatible(self$offsets))
        stop("Cannot make a POSIX timestamp with this calendar.", call. = FALSE)
      if (length(self$offsets) == 0L) return()

      time <- self$cal$offsets2time(self$offsets)

      if (is.null(format))
        format <- ifelse(self$cal$unit < 4L || .has_time(time), "timestamp", "date")
      else if (!(format %in% c("date", "timestamp")))
        stop("Format specifier not recognized", call. = FALSE) # nocov

      if (asPOSIX) {
        if (format == "date") ISOdate(time$year, time$month, time$day, 0L)
        else ISOdatetime(time$year, time$month, time$day, time$hour, time$minute, time$second, "UTC")
      } else .format_format(time, self$cal$timezone, format)
    },

    #' @description Format timestamps using a specific format string, using the
    #'   specifiers defined for the [base::strptime()] function, with
    #'   limitations. The only supported specifiers are `bBdeFhHImMpRSTYz%`.
    #'   Modifiers `E` and `O` are silently ignored. Other specifiers, including
    #'   their percent sign, are copied to the output as if they were adorning
    #'   text.
    #'
    #'   The formatting is largely oblivious to locale. The reason for this is
    #'   that certain dates in certain calendars are not POSIX-compliant and the
    #'   system functions necessary for locale information thus do not work
    #'   consistently. The main exception to this is the (abbreviated) names of
    #'   months (`bB`), which could be useful for pretty printing in the local
    #'   language. For separators and other locale-specific adornments, use
    #'   local knowledge instead of depending on system locale settings; e.g.
    #'   specify `%m/%d/%Y` instead of `%D`.
    #'
    #'   Week information, including weekday names, is not supported at all as a
    #'   "week" is not defined for non-standard CF calendars and not generally
    #'   useful for climate projection data. If you are working with observed
    #'   data and want to get pretty week formats, use the [as_timestamp()]
    #'   method to generate `POSIXct` timestamps (observed data generally uses a
    #'   "standard" calendar) and then use the [base::format()] function which
    #'   supports the full set of specifiers.
    #'
    #' @param format A character string with `strptime` format specifiers. If
    #'   omitted, the most economical format will be used: a full timestamp when
    #'   time information is available, a date otherwise.
    #'
    #' @return A vector of character strings with a properly formatted
    #'   timestamp. Any format specifiers not recognized or supported will be
    #'   returned verbatim.
    format = function(format) {
      if (length(self$offsets) == 0L) return(character(0L))

      if (!requireNamespace("stringr", quietly = TRUE))
        stop("package `stringr` is required - please install it first", call. = FALSE) # nocov

      if (missing(format)) format <- ""
      else if (!is.character(format) || length(format) != 1L)
        stop("`format` argument must be a character string with formatting specifiers", call. = FALSE)

      ts <- self$cal$offsets2time(self$offsets)
      .format_format(ts, self$cal$timezone, format)
    },

    #' @description Find the index in the time series for each timestamp given
    #'   in argument `x`. Values of `x` that are before the earliest value in
    #'   the time series will be returned as `0`; values of `x` that are after
    #'   the latest values in the time series will be returned as
    #'   `.Machine$integer.max`. Alternatively, when `x` is a numeric vector of
    #'   index values, return the valid indices of the same vector, with the
    #'   side effect being the attribute "CFTime" associated with the result.
    #'
    #'   Matching also returns index values for timestamps that fall between two
    #'   elements of the time series - this can lead to surprising results when
    #'   time series elements are positioned in the middle of an interval (as
    #'   the CF Metadata Conventions instruct us to "reasonably assume"): a time
    #'   series of days in January would be encoded in a netCDF file as
    #'   `c("2024-01-01 12:00:00", "2024-01-02 12:00:00", "2024-01-03 12:00:00", ...)`
    #'   so `x <- c("2024-01-01", "2024-01-02", "2024-01-03")` would
    #'   result in `(NA, 1, 2)` (or `(NA, 1.5, 2.5)` with `method = "linear"`)
    #'   because the date values in `x` are at midnight. This situation is
    #'   easily avoided by ensuring that this `CFTime` instance has bounds set
    #'   (use `bounds(y) <- TRUE` as a proximate solution if bounds are not
    #'   stored in the netCDF file). See the Examples.
    #'
    #'   If bounds are set, the indices are taken from those bounds. Returned
    #'   indices may fall in between bounds if the latter are not contiguous,
    #'   with the exception of the extreme values in `x`.
    #'
    #'   Values of `x` that are not valid timestamps according to the calendar
    #'   of this `CFTime` instance will be returned as `NA`.
    #'
    #'   `x` can also be a numeric vector of index values, in which case the
    #'   valid values in `x` are returned. If negative values are passed, the
    #'   positive counterparts will be excluded and then the remainder returned.
    #'   Positive and negative values may not be mixed. Using a numeric vector
    #'   has the side effect that the result has the attribute "CFTime"
    #'   describing the temporal dimension of the slice. If index values outside
    #'   of the range of `self` are provided, an error will be thrown.
    #'
    #' @param x Vector of character, POSIXt or Date values to find indices for,
    #'   or a numeric vector.
    #' @param method Single value of "constant" or "linear". If `"constant"` or
    #'   when bounds are set on `self`, return the index value for each
    #'   match. If `"linear"`, return the index value with any fractional value.
    #'
    #' @return A numeric vector giving indices into the "time" dimension of the
    #'   dataset associated with `self` for the values of `x`. If there is at
    #'   least 1 valid index, then attribute "CFTime" contains an instance of
    #'   `CFTime` that describes the dimension of filtering the dataset
    #'   associated with `self` with the result of this function, excluding any
    #'   `NA`, `0` and `.Machine$integer.max` values.
    indexOf = function(x, method = "constant") {
      stopifnot(inherits(x, c("character", "POSIXt", "Date")) || is.numeric(x),
                method %in% c("constant", "linear"))

      if (is.numeric(x)) {
        if (!(all(x < 0, na.rm = TRUE) || all(x > 0, na.rm = TRUE)))
          stop("Cannot mix positive and negative index values", call. = FALSE)

        intv <- (1:length(self$offsets))[x]
        xoff <- self$offsets[x]
      } else {
        if (self$cal$unit > 4L)
          stop("Parsing of timestamps on a 'month' or 'year' time unit is not supported.", call. = FALSE)

        xoff <- self$cal$parse(as.character(x))$offset
        vals <- self$get_bounds()
        vals <- if (is.null(vals)) self$offsets
                else c(vals[1L, 1L], vals[2L, ])
        intv <- stats::approx(vals, 1L:length(vals), xoff, method = method,
                              yleft = 0, yright = .Machine$integer.max)$y
        intv[which(intv == length(vals))] <- .Machine$integer.max
      }

      valid <- which(!is.na(intv) & intv > 0 & intv < .Machine$integer.max)
      if (any(valid)) {
        t <- CFTime$new(self$cal$definition, self$cal$name, xoff[valid])
        bnds <- self$get_bounds()
        if (!is.null(bnds))
          t$set_bounds(bnds[, intv[valid], drop = FALSE])
        attr(intv, "CFTime") <- t
      }
      intv
    },

    #' @description Return bounds.
    #'
    #' @param format A string specifying a format for output, optional.
    #' @return An array with dims(2, length(offsets)) with values for the
    #'   bounds. `NULL` if the bounds have not been set.
    get_bounds = function(format) {
      len <- length(self$offsets)
      if (len == 0L) return(NULL)

      bnds <- self$bounds
      if (is.logical(bnds)) {
        if (!bnds) return(NULL)

        b <- seq(from       = self$offsets[1L] - self$resolution * 0.5,
                 by         = self$resolution,
                 length.out = len + 1L)
        if (!missing(format)) {
          ts <- self$cal$offsets2time(b)
          b <- .format_format(ts, self$cal$timezone, format)
        }
        return(rbind(b[1L:len], b[2L:(len+1L)]))
      }

      # bnds is a matrix
      if (missing(format)) return(bnds)

      ts <- self$cal$offsets2time(as.vector(bnds))
      b <- .format_format(ts, self$cal$timezone, format)
      dim(b) <- c(2L, len)
      b
    },

    #' @description Set the bounds of the `CFTime` instance.
    #'
    #' @param value The bounds to set, in units of the offsets. Either a matrix
    #'   `(2, length(self$offsets))` or a single logical value.
    #' @return `self` invisibly.
    set_bounds = function(value) {
      if (isFALSE(value)) self$bounds <- FALSE
      else if (isTRUE(value)) self$bounds <- TRUE
      else {
        off <- self$offsets
        len <- length(off)

        if (len == 0L)
          stop("Cannot set bounds when there are no offsets", call. = FALSE)

        if (is.matrix(value) && is.numeric(value)) {
          if (!all(dim(value) == c(2L, len)))
            stop("Replacement value has incorrect dimensions", call. = FALSE)
        } else stop("Replacement value must be a numeric matrix or a single logical value", call. = FALSE)

        if (!(all(value[2L,] >= off) && all(off >= value[1L,])))
          stop("Values of the replacement value must surround the offset values", call. = FALSE)

        # Compress array to `TRUE`, if regular
        if (len > 1L && identical(value[1L,2L:len], value[2L,1L:(len-1L)]) &&
            diff(range(diff(value[1L,]))) == 0) value <- TRUE

        self$bounds <- value
        invisible(self)
      }
    },

    #' This method returns `TRUE` if the time series has uniformly distributed
    #' time steps between the extreme values, `FALSE` otherwise. First test
    #' without sorting; this should work for most data sets. If not, only then
    #' offsets are sorted. For most data sets that will work but for implied
    #' resolutions of month, season, year, etc based on a "days" or finer
    #' calendar unit this will fail due to the fact that those coarser units
    #' have a variable number of days per time step, in all calendars except for
    #' `360_day`. For now, an approximate solution is used that should work in
    #' all but the most non-conformal exotic arrangements.
    #'
    #' @return `TRUE` if all time steps are equidistant, `FALSE` otherwise, or
    #' `NA` if no offsets have been set.
    equidistant = function() {
      if (length(self$offsets) == 0L) return(NA)
      out <- all(diff(self$offsets) == self$resolution)
      if (!out) {
        doff <- diff(sort(self$offsets))
        out <- all(doff == self$resolution)
        if (!out) {
          # Don't try to make sense of totally non-standard arrangements such as
          # calendar units "years" or "months" describing sub-daily time steps.
          # Also, 360_day calendar should be well-behaved so we don't want to get here.
          if (self$cal$unit > 4L || inherits(self$cal, "CFCalendar360")) return(FALSE)

          # Check if we have monthly or yearly data on a finer-scale calendar
          # This is all rather approximate but should be fine in most cases
          # This accommodates middle-of-the-time-period offsets as per the
          # CF Metadata Conventions
          # Please report problems at https://github.com/pvanlaake/CFtime/issues
          ddays <- range(doff) * CFt$units$per_day[self$cal$unit]
          return((ddays[1] >= 28 && ddays[2] <= 31) ||      # months
                   (ddays[1] >= 8 && ddays[2] <= 11) ||     # dekads
                   (ddays[1] >= 90 && ddays[2] <= 92) ||    # seasons, quarters
                   (ddays[1] >= 365 && ddays[2] <= 366))    # years
        }
      }
      out
    },

    #' @description Given a vector of character timestamps, return a logical
    #'   vector of a length equal to the number of time steps in the time series
    #'   with values `TRUE` for those time steps that fall between the two
    #'   extreme values of the vector values, `FALSE` otherwise.
    #'
    #' @param extremes Character vector of timestamps that represent the
    #'   time period of interest. The extreme values are selected. Badly
    #'   formatted timestamps are silently dropped.
    #' @param closed Is the right side closed, i.e. included in the result?
    #'   Default is `FALSE`. A specification of `c("2022-01-01", "2023-01-01)`
    #'   will thus include all time steps that fall in the year 2022 when
    #'   `closed = FALSE` but include `2023-01-01` if that exact value is
    #'   present in the time series.
    #' @return A logical vector with a length equal to the number of time steps
    #'   in `self` with values `TRUE` for those time steps that fall between the
    #'   extreme values, `FALSE` otherwise.
    #'
    #'   An attribute 'CFTime' will have the same definition as `self` but with
    #'   offsets corresponding to the time steps falling between the two
    #'   extremes. If there are no values between the extremes, the attribute is
    #'   `NULL`.
    slice = function(extremes, closed = FALSE) {
      if (!is.character(extremes) || length(extremes) < 1L)
        stop("Second argument must be a character vector of at least one timestamp.", call. = FALSE)

      off <- self$offsets
      roff <- range(off)
      ext <- range(self$cal$parse(extremes)$offset, na.rm = TRUE)
      if (all(is.na(ext)) || ext[1L] > roff[2L] || ext[2L] < roff[1L])
        out <- rep(FALSE, length(off))
      else {
        if (ext[1L] == ext[2L]) closed <- TRUE
        out <- if (closed) off >= ext[1L] & off <= ext[2L]
               else off >= ext[1L] & off < ext[2L]
        if (any(out)) {
          t <- CFTime$new(self$cal$definition, self$cal$name, off[out])
          bnds <- self$get_bounds()
          if (!is.null(bnds))
            t$set_bounds(bnds[, out, drop = FALSE])
          attr(out, "CFTime") <- t
        } else
          out <- rep(FALSE, length(off))
      }
      out
    },

    #' @description Can the time series be converted to POSIXt?
    #' @return `TRUE` if the calendar support coversion to POSIXt, `FALSE`
    #' otherwise.
    POSIX_compatible = function() {
      self$cal$POSIX_compatible(self$offsets)
    },

    #' @description Create a factor for a `CFTime` instance.
    #'
    #'   When argument `breaks` is one of `"year", "season", "quarter", "month",
    #'   "dekad", "day"`, a factor is generated like by [CFfactor()]. When
    #'   `breaks` is a vector of character timestamps a factor is produced with
    #'   a level for every interval between timestamps. The last timestamp,
    #'   therefore, is only used to close the interval started by the
    #'   pen-ultimate timestamp - use a distant timestamp (e.g. `range(x)[2]`)
    #'   to ensure that all offsets to the end of the CFTime time series are
    #'   included, if so desired. The last timestamp will become the upper bound
    #'   in the `CFTime` instance that is returned as an attribute to this
    #'   function so a sensible value for the last timestamp is advisable.
    #'
    #'   This method works similar to [base::cut.POSIXt()] but there are some
    #'   differences in the arguments: for `breaks` the set of options is
    #'   different and no preceding integer is allowed, `labels` are always
    #'   assigned using values of `breaks`, and the interval is always
    #'   left-closed.
    #'
    #' @param breaks A character string of a factor period (see [CFfactor()] for
    #'   a description), or a character vector of timestamps that conform to the
    #'   calendar of `x`, with a length of at least 2. Timestamps must be given
    #'   in ISO8601 format, e.g. "2024-04-10 21:31:43".
    #'
    #' @return A factor with levels according to the `breaks` argument, with
    #'   attributes 'period', 'era' and 'CFTime'. When `breaks` is a factor
    #'   period, attribute 'period' has that value, otherwise it is '"day"'.
    #'   When `breaks` is a character vector of timestamps, attribute 'CFTime'
    #'   holds an instance of `CFTime` that has the same definition as `x`, but
    #'   with (ordered) offsets generated from the `breaks`. Attribute 'era'
    #'   is always -1.
    cut = function(breaks) {
      if (missing(breaks) || !is.character(breaks) || (len <- length(breaks)) < 1L)
        stop("Argument 'breaks' must be a character vector with at least 1 value", call. = FALSE) # nocov

      if(len == 1L) {
        breaks <- sub("s$", "", tolower(breaks))
        if (breaks %in% CFt$factor_periods)
          return(CFfactor(self, breaks)) # FIXME after CFfactor is done
        else stop("Invalid specification of 'breaks'", call. = FALSE) # nocov
      }

      # breaks is a character vector of multiple timestamps
      if (self$cal$unit > 4L) stop("Factorizing on a 'month' or 'year' time unit is not supported", call. = FALSE) # nocov
      time <- self$cal$parse(breaks)
      if (anyNA(time$year))
        stop("Invalid specification of 'breaks'", call. = FALSE) # nocov
      sorted <- order(time$offset)
      ooff <- time$offset[sorted]
      intv <- findInterval(self$offsets, ooff)
      intv[which(intv %in% c(0L, len))] <- NA
      f <- factor(intv, labels = breaks[sorted][1L:(len-1L)])

      # Attributes
      bnds <- rbind(ooff[1L:(len-1L)], ooff[2L:len])
      off  <- bnds[1L, ] + (bnds[2L, ] - bnds[1L, ]) * 0.5
      t <- CFTime$new(self$cal$definition, self$cal$name, off)
      bounds(t) <- bnds
      attr(f, "period") <- "day"
      attr(f, "era")  <- -1L
      attr(f, "CFTime") <- t
      f
    },

    #' @description Generate a factor for the offsets, or a part thereof. This is
    #' specifically interesting for creating factors from the date part of the
    #' time series that aggregate the time series into longer time periods (such
    #' as month) that can then be used to process daily CF data sets using, for
    #' instance, `tapply()`.
    #'
    #' The factor will respect the calendar that the time series is built on.
    #'
    #' The factor will be generated in the order of the offsets. While typical
    #' CF-compliant data sources use ordered time series there is, however, no
    #' guarantee that the factor is ordered. For most processing with a factor
    #' the ordering is of no concern.
    #'
    #' If the `era` parameter is specified, either as a vector of years to
    #' include in the factor, or as a list of such vectors, the factor will only
    #' consider those values in the time series that fall within the list of
    #' years, inclusive of boundary values. Other values in the factor will be
    #' set to `NA`. The years need not be contiguous, within a single vector or
    #' among the list items, or in order.
    #'
    #' The following periods are supported by this method:
    #'
    #' \itemize{
    #'   \item `year`, the year of each offset is returned as "YYYY".
    #'   \item `season`, the meteorological season of each offset is returned as
    #'   "Sx", with x being 1-4, preceeded by "YYYY" if no `era` is
    #'   specified. Note that December dates are labeled as belonging to the
    #'   subsequent year, so the date "2020-12-01" yields "2021S1". This implies
    #'   that for standard CMIP files having one or more full years of data the
    #'   first season will have data for the first two months (January and
    #'   February), while the final season will have only a single month of data
    #'   (December).
    #'   \item `quarter`, the calendar quarter of each offset is returned as "Qx",
    #'   with x being 1-4, preceeded by "YYYY" if no `era` is specified.
    #'   \item `month`, the month of each offset is returned as "01" to
    #'   "12", preceeded by "YYYY-" if no `era` is specified. This is the default
    #'   period.
    #'   \item `dekad`, ten-day periods are returned as
    #'   "Dxx", where xx runs from "01" to "36", preceeded by "YYYY" if no `era`
    #'   is specified. Each month is subdivided in dekads as follows: 1- days 01 -
    #'   10; 2- days 11 - 20; 3- remainder of the month.
    #'   \item `day`, the month and day of each offset are returned as "MM-DD",
    #'   preceeded by "YYYY-" if no `era` is specified.
    #' }
    #'
    #' It is not possible to create a factor for a period that is shorter than
    #' the temporal resolution of the calendar. As an example, if the calendar
    #' has a monthly unit, a dekad or day factor cannot be created.
    #'
    #' Creating factors for other periods is not supported by this method.
    #' Factors based on the timestamp information and not dependent on the
    #' calendar can trivially be constructed from the output of the
    #' [as_timestamp()] function.
    #'
    #' For non-era factors the attribute 'CFTime' of the result contains a
    #' `CFTime` instance that is valid for the result of applying the factor to
    #' a resource that this instance is associated with. In other words, if
    #' `CFTime` instance 'At' describes the temporal dimension of resource 'A'
    #' and a factor 'Af' is generated from `Af <- At$factor()`, then
    #' `Bt <- attr(Af, "CFTime")` describes the temporal dimension of the result
    #' of, say, `B <- apply(A, 1:2, tapply, Af, FUN)`. The 'CFTime' attribute is
    #' `NULL` for era factors.
    #'
    #' @param period character. A character string with one of the values
    #'   "year", "season", "quarter", "month" (the default), "dekad" or "day".
    #' @param era numeric or list, optional. Vector of years for which to
    #'   construct the factor, or a list whose elements are each a vector of
    #'   years. If `era` is not specified, the factor will use the entire time
    #'   series for the factor.
    #' @return If `era` is a single vector or not specified, a factor with a
    #'   length equal to the number of offsets in this instance. If `era` is a
    #'   list, a list with the same number of elements and names as `era`,
    #'   each containing a factor. Elements in the factor will be set to `NA`
    #'   for time series values outside of the range of specified years.
    #'
    #'   The factor, or factors in the list, have attributes 'period', 'era'
    #'   and 'CFTime'. Attribute 'period' holds the value of the `period`
    #'   argument. Attribute 'era' indicates the number of years that are
    #'   included in the era, or -1 if no `era` is provided. Attribute
    #'   'CFTime' holds an instance of `CFTime` that has the same definition as
    #'   this instance, but with offsets corresponding to the mid-point of
    #'   non-era factor levels; if the `era` argument is specified,
    #'   attribute 'CFTime' is `NULL`.
    factor = function(period = "month", era = NULL) {
      if (length(self$offsets) < 10L) stop("Cannot create a factor for very short time series", call. = FALSE) # nocov

      period <- tolower(period)
      if (!((length(period) == 1L) && (period %in% CFt$factor_periods)))
        stop("Period specifier must be a single value of a supported period", call. = FALSE) # nocov

      # No fine-grained period factors for coarse source data
      timestep <- CFt$units$seconds[self$cal$unit] * self$resolution;
      if ((period == "year") && (timestep > 86400 * 366) ||
          (period %in% c("season", "quarter")) && (timestep > 86400 * 90) || # Somewhat arbitrary
          (period == "month") && (timestep > 86400 * 31) ||
          (period == "dekad") && (timestep > 86400) ||       # Must be constructed from daily or finer data
          (period == "day") && (timestep > 86400))           # Must be no longer than a day
        stop("Cannot produce a short period factor from source data with long time interval", call. = FALSE) # nocov

      time <- self$cal$offsets2time(self$offsets)
      months <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")

      if (is.null(era)) {
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
                   stop("Must install package `stringr` to use this functionality.", call. = FALSE) # nocov

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
                   stop("Must install package `stringr` to use this functionality.", call. = FALSE) # nocov

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
                 yr <- as.integer(substr(ll, 1L, 4L))
                 if (lp == 36L)
                   dt <- c(dt, sprintf("%04d-01-01", yr + 1L))
                 else dt <- c(dt, sprintf("%04d-%02d-%s", yr, (lp + 1L) %/% 3L + 1L, c("01", "11", "21")[(lp + 1L) %% 3L + 1L]))
               },
               "day"     = {
                 out <- as.factor(sprintf("%04d-%02d-%02d", time$year, time$month, time$day))
                 l <- levels(out)
                 lp <- l[nlevels(out)]
                 last <- self$cal$offsets2time(self$cal$parse(lp)$offset)
                 dt <- c(l, sprintf("%04d-%02d-%02d", last$year, last$month, last$day))
               }
        )

        # Convert bounds dates to an array of offsets, find mid-points, create new CFTime instance
        off  <- self$cal$parse(dt)$offset
        off[is.na(off)] <- 0     # This can happen only when the time series starts at or close to the origin, for seasons
        noff <- length(off)
        bnds <- rbind(off[1L:(noff - 1L)], off[2L:noff])
        off  <- bnds[1L,] + (bnds[2L,] - bnds[1L,]) * 0.5
        new_cf <- CFTime$new(self$cal$definition, self$cal$name, off)
        bounds(new_cf) <- TRUE

        # Bind attributes to the factor
        attr(out, "era") <- -1L
        attr(out, "period") <- period
        attr(out, "CFTime") <- new_cf
        return(out)
      }

      # Era factor
      if (is.numeric(era)) ep <- list(era)
      else if ((is.list(era) && all(unlist(lapply(era, is.numeric))))) ep <- era
      else stop("When specified, the `era` parameter must be a numeric vector or a list thereof", call. = FALSE)

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
        attr(f, "era") <- length(years)
        attr(f, "period") <- period
        attr(f, "CFTime") <- NULL
        f
      })
      if (is.numeric(era)) out <- out[[1L]]
      else names(out) <- names(era)
      out
    },

    #' @description Given a factor as produced by `CFTime$factor()`, this method
    #'   will return a numeric vector with the number of time units in each
    #'   level of the factor.
    #'
    #'   The result of this method is useful to convert between absolute and
    #'   relative values. Climate change anomalies, for instance, are usually
    #'   computed by differencing average values between a future period and a
    #'   baseline period. Going from average values back to absolute values for
    #'   an aggregate period (which is typical for temperature and
    #'   precipitation, among other variables) is easily done with the result of
    #'   this method, without having to consider the specifics of the calendar
    #'   of the data set.
    #'
    #'   If the factor `f` is for an era (e.g. spanning multiple years and the
    #'   levels do not indicate the specific year), then the result will
    #'   indicate the number of time units of the period in a regular single
    #'   year. In other words, for an era of 2041-2060 and a monthly factor on a
    #'   standard calendar with a `days` unit, the result will be
    #'   `c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)`. Leap days are thus
    #'   only considered for the `366_day` and `all_leap` calendars.
    #'
    #'   Note that this function gives the number of time units in each level of
    #'   the factor - the actual number of data points in the time series per
    #'   factor level may be different. Use [CFfactor_coverage()] to determine
    #'   the actual number of data points or the coverage of data points
    #'   relative to the factor level.
    #'
    #' @param f A factor or a list of factors derived from the method
    #'   `CFTime$factor()`.
    #' @return If `f` is a factor, a numeric vector with a length equal to the
    #'   number of levels in the factor, indicating the number of time units in
    #'   each level of the factor. If `f` is a list of factors, a list with each
    #'   element a numeric vector as above.
    factor_units = function(f) {
      if (is.list(f)) factors <- f else factors <- list(f)
      if (!(all(unlist(lapply(factors, function(x) is.factor(x) && is.numeric(attr(x, "era")) &&
                              attr(x, "period") %in% CFt$factor_periods)))))
        stop("Argument `f` must be a factor generated by the method `CFTime$factor()`", call. = FALSE) # nocov

      out <- lapply(factors, function(fac) .factor_units(fac, self$cal, CFt$units$per_day[self$cal$unit]))
      if (is.factor(f)) out <- out[[1L]]
      out
    },

    #' @description Calculate the number of time elements, or the relative
    #' coverage, in each level of a factor generated by `CFTime$factor()`.
    #'
    #' @param f A factor or a list of factors derived from the method
    #'   `CFTime$factor()`.
    #' @param coverage "absolute" or "relative".
    #' @return If `f` is a factor, a numeric vector with a length equal to the
    #'   number of levels in the factor, indicating the number of units from the
    #'   time series contained in each level of the factor when
    #'   `coverage = "absolute"` or the proportion of units present relative to the
    #'   maximum number when `coverage = "relative"`. If `f` is a list of factors, a
    #'   list with each element a numeric vector as above.
    factor_coverage = function(f, coverage = "absolute") {
      if (is.list(f)) factors <- f else factors <- list(f)
      if (!(all(unlist(lapply(factors, function(x) is.factor(x) && is.numeric(attr(x, "era")) &&
                              attr(x, "period") %in% CFt$factor_periods)))))
        stop("Argument `f` must be a factor generated by the method `CFTime$factor()`", call. = FALSE) # nocov

      if (!(is.character(coverage) && coverage %in% c("absolute", "relative")))
        stop("Argument `coverage` must be a character string with a value of 'absolute' or 'relative'", call. = FALSE) # nocov

      if (coverage == "relative") {
        out <- lapply(factors, function(fac) {
          res <- tabulate(fac) / .factor_units(fac, self$cal, CFt$units$per_day[self$cal$unit])
          yrs <- attr(fac, "era")
          if (yrs > 0) res <- res / yrs
          return(res)
        })
      } else {
        out <- lapply(factors, tabulate)
      }

      if (is.factor(f)) out <- out[[1L]]
      out
    }
  ),
  active = list(
    #' @field unit (read-only) The unit string of the calendar and time series.
    unit = function(value) {
      if (missing(value))
        CFt$units$name[self$cal$unit]
    }
  )
)
