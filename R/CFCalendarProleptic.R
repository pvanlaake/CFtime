#' @title Proleptic Gregorian CF calendar
#'
#' @description This class represents a standard CF calendar, but with the
#'   Gregorian calendar extended backwards to before the introduction of the
#'   Gregorian calendar. This calendar is compatible with the standard POSIXt
#'   calendar, but note that daylight savings time is not considered.
#'
#'   This calendar includes dates 1582-10-14 to 1582-10-05 (the gap between the
#'   Gregorian and Julian calendars, which is observed by the standard
#'   calendar), and extends to years before the year 1, including year 0.
#'
#' @aliases CFCalendarProleptic
#' @docType class
CFCalendarProleptic <- R6::R6Class("CFCalendarProleptic",
  inherit = CFCalendar,
  public = list(
    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must be "proleptic_gregorian".
    #'   This argument is superfluous but maintained to be consistent with the
    #'   initialization methods of the parent and sibling classes.
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
      ymd$year & ymd$month >= 1L & ymd$month <= 12L & ymd$day >= 1L &
      ifelse(self$leap_year(ymd$year),
        ymd$day <= c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[ymd$month],
        ymd$day <= c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[ymd$month])
    },

    #' @description Determine the number of days in the month of the calendar.
    #' @param ymd `data.frame`, optional, with dates parsed into their parts.
    #' @return Integer vector indicating the number of days in each month for
    #'   the dates supplied as argument `ymd`. If no dates are supplied, the
    #'   number of days per month for the calendar as a vector of length 12, for
    #'   a regular year without a leap day.
    month_days = function(ymd = NULL) {
      if (is.null(ymd)) return(c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L))

      ifelse(self$leap_year(ymd$year),
             c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month],
             c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month])
    },

    #' @description Indicate which years are leap years.
    #' @param yr Integer vector of years to test.
    #' @return Logical vector with the same length as argument `yr`. `NA` is
    #'   returned where elements in argument `yr` are `NA`.
    leap_year = function(yr) {
      ((yr %% 4L == 0L) & (yr %% 100L > 0L)) | (yr %% 400L == 0L)
    },

    #' @description Indicate if the time series described using this calendar
    #'   can be safely converted to a standard date-time type (`POSIXct`,
    #'   `POSIXlt`, `Date`).
   #' @param offsets The offsets from the CFtime instance.
    #' @return `TRUE`.
    POSIX_compatible = function(offsets) {
      TRUE
    },

    #' @description Calculate difference in days between a `data.frame` of time
    #'   parts and the origin.
    #'
    #' @param x `data.frame`. Dates to calculate the difference for.
    #' @return Integer vector of a length equal to the number of rows in
    #' argument `x` indicating the number of days between `x` and the `origin`,
    #' or `NA` for rows in `x` with `NA` values.
    date2offset = function(x) {
      origin <- lubridate::make_date(self$origin$year, self$origin$month, self$origin$day)
      as.integer(lubridate::make_date(x$year, x$month, x$day) - origin)
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      dt <- lubridate::make_date(self$origin$year, self$origin$month, self$origin$day) + lubridate::days(x)
      data.frame(year = lubridate::year(dt), month = lubridate::month(dt), day = lubridate::mday(dt), row.names = NULL)
    }
  )
)
