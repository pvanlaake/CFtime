#' @title 360-day CF calendar
#'
#' @description This class represents a CF calendar of 360 days per year, evenly
#'   divided over 12 months of 30 days. This calendar is obviously not
#'   compatible with the standard POSIXt calendar.
#'
#'   This calendar supports dates before year 1 and includes the year 0.
#'
#' @aliases CFCalendar360
#' @docType class
CFCalendar360 <- R6::R6Class("CFCalendar360",
  inherit = CFCalendar,
  public = list(
    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must be "360_day". This argument
    #' is superfluous but maintained to be consistent with the initialization
    #' methods of the parent and sibling classes.
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    #' @return A new instance of this class.
    initialize = function(nm, definition) {
      super$initialize(nm, definition)
    },

    #' @description Indicate which of the supplied dates are valid.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for valid days and `FALSE` for invalid days, or `NA` where
    #'   the row in argument `ymd` has `NA` values.
    valid_days = function(ymd) {
      ymd$year & ymd$month >= 1L & ymd$month <= 12L & ymd$day >= 1L & ymd$day <= 30L
    },

    #' @description Determine the number of days in the month of the calendar.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return A vector indicating the number of days in each month for the
    #'   dates supplied as argument `ymd`. If no dates are supplied, the number
    #'   of days per month for the calendar as a vector of length 12.
    month_days = function(ymd = NULL) {
      if (is.null(ymd)) return(rep(30L, 12L))

      res <- rep(30L, nrow(ymd))
      res[which(is.na(ymd$year))] <- NA
      res
    },

    #' @description Indicate which years are leap years.
    #' @param yr Integer vector of years to test.
    #' @return Logical vector with the same length as argument `yr`. Since this
    #' calendar does not use leap days, all values will be `FALSE`, or `NA`
    #' where argument `yr` is `NA`.
    leap_year = function(yr) {
      res <- rep(FALSE, length(yr))
      res[which(is.na(yr))] <- NA
      res
    },

    #' @description Calculate difference in days between a `data.frame` of time
    #'   parts and the origin.
    #'
    #' @param x `data.frame`. Dates to calculate the difference for.
    #'
    #' @return Integer vector of a length equal to the number of rows in
    #' argument `x` indicating the number of days between `x` and the `origin`,
    #' or `NA` for rows in `x` with `NA` values.
    date2offset = function(x) {
      (x$year - self$origin$year) * 360L + (x$month - self$origin$month) * 30L + x$day - self$origin$day
    },

    #' @description Calculate date parts from day differences from the origin.
    #'   This only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #'
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      y <- self$origin$year + x %/% 360L
      m <- self$origin$month + (x %% 360L) %/% 30L
      d <- self$origin$day + x %% 30L
      over <- which(d > 30L)
      d[over] <- d[over] - 30L
      m[over] <- m[over] + 1L
      over <- which(m > 12L)
      m[over] <- m[over] - 12L
      y[over] <- y[over] + 1L
      data.frame(year = y, month = m, day = d, row.names = NULL)
    }
  )
)
