#' @title CF calendar with no annual cycle
#'
#' @description This class represents a CF calendar with no annual cycle. All
#' datetimes in the calendar are the same. This is useful only for repeated
#' experiments simulating a fixed time in the year.
#'
#' @docType class
CFCalendarNone <- R6::R6Class("CFCalendarNone",
  inherit = CFCalendar,
  public = list(
    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must be "none". This argument
    #' is superfluous but maintained to be consistent with the initialization
    #' methods of the parent and sibling classes.
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    #' @return A new instance of this class.
    initialize = function(nm, definition) {
      super$initialize(nm, definition)
    },

    #' @description Indicate which of the supplied dates are valid. In this
    #' calendar only one date is valid, namely the one that the calendar is
    #' initialized with.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for valid days and `FALSE` for invalid days, or `NA` where
    #'   the row in argument `ymd` has `NA` values.
    valid_days = function(ymd) {
      if (!nrow(self$origin)) TRUE
      else
        ymd$year &
        ymd$year == self$origin$year &
        ymd$month == self$origin$month &
        ymd$day == self$origin$day
    },

    #' @description Determine the number of days in the month of the calendar.
    #'   This always returns a vector of `NA` values because this method has no
    #'   meaning for this class.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return A vector with `NA` values for the dates supplied as argument
    #'   `ymd`. If no dates are supplied, a vector of `NA` values of length 12.
    month_days = function(ymd = NULL) {
      if (is.null(ymd)) rep(NA, 12L) else rep(NA, nrow(ymd))
    },

    #' @description Indicate which years are leap years. This always returns a
    #'   vector of `NA` values because this method has no meaning for this
    #'   class.
    #' @param yr Integer vector of years to test.
    #' @return Vector with the same length as argument `yr` with `NA` values.
    leap_year = function(yr) {
      rep(NA, length(yr))
    },

    #' @description Calculate difference in days between a `data.frame` of time
    #'   parts and the origin. The difference is always 0, given that
    #'   this is not a true calendar and there can be no calculations to
    #'   determine any difference.
    #' @param x `data.frame`. Dates to calculate the difference for.
    #' @return Integer vector of a length equal to the number of rows in
    #'   argument `x` indicating 0 for the days that equal the origin
    #'   of the calendar, or `NA` otherwise.
    date2offset = function(x) {
      res <- rep(0L, nrow(x))
      res[which(is.na(x$year))] <- NA_integer_
      res
    },

    #' @description Calculate date parts from day differences from the origin.
    #'   This always returns 0's for the year, month and day as other values are
    #'   not valid in this calendar. Hour-minute-second datetime parts are
    #'   handled in [CFCalendar].
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`. Rows with values of `x` other than 0
    #'   will return the values for the origin of the calendar nonetheless, in
    #'   accordance with the CF Metadata Conventions.
    offset2date = function(x) {
      len <- length(x)
      data.frame(year = rep(self$origin$year, len), month = rep(self$origin$month, len), day = rep(self$origin$day, len))
    }
  )
)
