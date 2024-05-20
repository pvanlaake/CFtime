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
      if (x@datum@unit > 4L || x@datum@cal_id == 3L) return(FALSE)

      # Check if we have monthly or yearly data on a finer-scale datum
      # This is all rather approximate but should be fine in most cases
      # This accommodates middle-of-the-time-period offsets as per the CF Metadata Conventions
      # Please report problems at https://github.com/pvanlaake/CFtime/issues
      ddays <- range(doff) * CFt$units$per_day[x@datum@unit]
      return((ddays[1] >= 28 && ddays[2] <= 31) ||    # months
             (ddays[1] >= 8 && ddays[2] <= 11) ||     # dekads
             (ddays[1] >= 90 && ddays[2] <= 92) ||    # seasons, quarters
             (ddays[1] >= 365 && ddays[2] <= 366))    # years
    }
  }
  out
}

#' Set the bounds of a CFtime instance
#'
#' @param cf The CFtime instance
#' @param value The bounds to set. Either an array (2, length(cf)) or a logical
#'
#' @returns Returns `cf` invisibly
#' @noRd
.set_bounds <- function(cf, value) {
  if (isFALSE(value)) cf@bounds <- FALSE
  else if (isTRUE(value)) cf@bounds <- TRUE
  else {
    off <- cf@offsets
    len <- length(off)

    if (len == 0L)
      stop("Cannot set bounds when there are no offsets")

    if (is.matrix(value) && is.numeric(value)) {
      if (!all(dim(value) == c(2L, len)))
        stop("Replacement value has incorrect dimensions")
    } else stop("Replacement value must be a numeric matrix or a single logical value")

    if (!(all(value[2L,] >= off) && all(off >= value[1L,])))
      stop("Values of the replacement value must surround the offset values")

    # Compress array to `TRUE`, if regular
    if (len > 1L && identical(value[1L,2L:len], value[2L,1L:(len-1L)]) &&
        diff(range(diff(value[1L,]))) == 0) value <- TRUE

    cf@bounds <- value
  }
  invisible(cf)
}

#' Return bounds
#'
#' @param cf The CFtime instance
#' @param format Optional. A string specifying a format for output
#'
#' @returns An array with dims(2, length(offsets)) with values for the bounds.
#'   `NULL` if the bounds have not been set.
#' @noRd
.get_bounds <- function (cf, format) {
  len <- length(cf@offsets)
  if (len == 0L) return(NULL)

  bnds <- cf@bounds
  if (is.logical(bnds)) {
    if (!bnds) return(NULL)

    b <- seq(from       = cf@offsets[1L] - cf@resolution * 0.5,
             by         = cf@resolution,
             length.out = len + 1L)
    if (!missing(format)) {
      ts <- .offsets2time(b, cf@datum)
      b <- .format_format(ts, tz(cf@datum), format)
    }
    return(rbind(b[1L:len], b[2L:(len+1L)]))
  }

  # bnds is a matrix
  if (missing(format)) return(bnds)

  ts <- .offsets2time(as.vector(bnds), cf@datum)
  b <- .format_format(ts, tz(cf@datum), format)
  dim(b) <- c(2L, len)
  b
}
