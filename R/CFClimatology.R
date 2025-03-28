#' @title CFClimatology class
#'
#' @description This class represents a climatological time coordinate, both its
#' "calendar" and its "climatology" bounds values.
#'
#' The "calendar" portion is managed by the [CFTime] class, from which this
#' class is inherited. This class implements the specific behaviour of the
#' climatological bounds values and includes methods to query the structure of
#' the climatological data.
#'
#' @export
#' @references
#'   https://cfconventions.org/Data/cf-conventions/cf-conventions-1.12/cf-conventions.html#climatological-statistics
#' @docType class
CFClimatology <- R6::R6Class("CFClimatology",
  inherit = CFTime,
  private = list(
    # The base period over which the climatology is calculated. This must be
    # one of "year", "season", "quarter", "month", "dekad" or "day". Set in
    # `initialize()`.
    .period = "",

    # The years over which the climatology is calculated. Set in `initialize()`.
    .years = NA
  ),
  public = list(
    #' @description Create a new instance of this class.
    #' @param definition Character string of the units and origin of the
    #'   calendar.
    #' @param calendar Character string of the calendar to use. Must be one of
    #'   the values permitted by the CF Metadata Conventions. If `NULL`, the
    #'   "standard" calendar will be used.
    #' @param offsets Numeric or character vector. When numeric, a
    #'   vector of offsets from the origin in the time series. When a character
    #'   vector of length 2 or more, timestamps in ISO8601 or UDUNITS format.
    #' @param bounds The climatological bounds for the offsets. A `matrix` with
    #'   columns for `offsets` and low values in the first row, high values in
    #'   the second row. The bounds will oftentimes overlap or be discontinuous.
    initialize = function(definition, calendar = "standard", offsets, bounds) {
      if (!is.matrix(bounds) || !all(dim(bounds) == c(2, length(offsets))))
        stop("Argument 'bounds' must be a matrix with two rows and as many columns as the length of argument 'offset'", call. = FALSE)

      super$initialize(definition, calendar, offsets)
      self$bounds <- self$set_bounds(bounds)

      # Determine period and years from the bounds of the first offset
      time <- self$cal$offsets2time(bounds[1L:2L, 1L])
      private$.years <- time$year
      if (all(time$day == 1L)) {
        if (all(time$month == 1L)) {
          private$.period <- "year"
          private$.years <- c(time$year[1L], time$year[2L] - 1L)
        } else if (all(time$month %in% c(3L, 6L, 9L, 12L))) {
          private$.period <- "season"
          private$.years <- c(time$year[1L] + 1L, time$year[2L])
        } else if (all(time$month %in% c(1L, 4L, 7L, 10L)))
          private$.period <- "quarter"
        else
          private$.period <- "month"
      } else if (all(time$day %in% c(1L, 11L, 21L)))
        private$.period <- "dekad"
      else
        private$.period <- "day"
    },

    #' @description Print a summary of the `CFClimatology` object to the console.
    #' @param ... Ignored.
    #' @return `self` invisibly.
    print = function(...) {
      noff <- length(self$offsets)
      d <- self$range()
      el <- if (noff > 1L) {
        sprintf("  Elements: [%s .. %s] (average of %f %s between %d elements)\n",
                d[1L], d[2L], self$resolution, CFt$units$name[self$cal$unit], noff)
      } else
        paste0("  Elements: ", d[1L], "\n")

      p <- paste0("  Period  : ", private$.period, "\n")
      if (private$.years[1L] == private$.years[2L]) y <- paste0("  Year    : ", private$.years[1L], "\n")
      else y <- paste0("  Years   : ", private$.years[1L], " - ", private$.years[2L], " (inclusive)\n")

      cal <- capture.output(self$cal$print())
      cat(paste(cal, collapse = "\n"), "\nClimatological time series:\n",  el, p, y, sep = "")
      invisible(self)
    }
  ),
  active = list(
    #' @field period (read-only) Character string indicating the period during
    #'   the year over which the climatology was calculated.
    period = function(value) {
      if (missing(value))
        private$.period
    },

    #' @field years (read-only) Vector of two integer values indicating the
    #'   years over which the climatology was calculated.
    years = function(value) {
      if (missing(value))
        private$.years
    },

    #' @field friendlyClassName Character string with class name for display
    #'   purposes.
    friendlyClassName = function(value) {
      if (missing(value))
        "CFClimatology"
    }
  )
)
