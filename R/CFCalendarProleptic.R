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
  private = list(
    # Rata Die, the number of days from the day before 0001-01-01 to
    # origin of this calendar. Used to convert offsets from the calendar origin
    # to the day before 0001-01-01 for arithmetic calculations.
    rd = 0L
  ),
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
      private$rd <- .gregorian_date2offset(self$origin, self$leap_year(self$origin$year))
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
      TRUE # nocov
    },

    #' @description Calculate difference in days between a `data.frame` of time
    #'   parts and the origin.
    #'
    #' @param x `data.frame`. Dates to calculate the difference for.
    #' @return Integer vector of a length equal to the number of rows in
    #' argument `x` indicating the number of days between `x` and the `origin`,
    #' or `NA` for rows in `x` with `NA` values.
    date2offset = function(x) {
      .gregorian_date2offset(x, self$leap_year(x$year)) - private$rd
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      .gregorian_offset2date(x + private$rd)
    }
  )
)

# The below functions use arithmetic offset calculation from date parts and
# vice-versa. These functions are R-ified from pseudo-functions in Reingold &
# Derschowitz, "Calendrical Calculations", 2018.

#' Dates to offset, from function `fixed-from-gregorian()`
#'
#' @param x `data.frame` with columns "year", "month" and "date"
#' @param leapyear Logical vector of the same length as `x` has rows indicating
#' for each row in `x` if this is a leap year.
#' @return Integer vector of offsets for the dates in `x`. The offsets are
#' relative to the day before 0001-01-01.
#' @noRd
.gregorian_date2offset <- function(x, leapyear) {
  year1 <- x$year - 1L
  corr <- ifelse(x$month <= 2L, 0L, as.integer(leapyear) - 2L)
  365L * year1 + year1 %/% 4L - year1 %/% 100L + year1 %/% 400L +
    (367L * x$month - 362L) %/% 12L + corr + x$day
}

#' Offsets to dates, from function `gregorian-from-fixed()` and support functions.
#'
#' @param x Integer vector of offsets. The offsets must be relative to the day
#' before 0001-01-01.
#' @return `data.frame` with date elements "year", "month" and "day".
#' @noRd
.gregorian_offset2date <- function(x) {
  d0 <- x - 1L
  n400 <- d0 %/% 146097L; d1 <- d0 %% 146097L
  n100 <- d1 %/% 36524L;  d2 <- d1 %% 36524L
  n4 <-   d2 %/% 1461L;   d3 <- d2 %% 1461L
  n1 <-   d3 %/% 365L
  yr <- 400L * n400 + 100L * n100 + 4L * n4 + n1
  yr <- ifelse(n100 == 4L | n1 == 4L, yr, yr + 1L)
  leapyear <- ((yr %% 4L == 0L) & (yr %% 100L > 0L)) | (yr %% 400L == 0L)
  yr1 <- yr - 1L
  jan1 <- 365L * yr1 + yr1 %/% 4L - yr1 %/% 100L + yr1 %/% 400L + 1L
  prior_days <- x - jan1 + ifelse(x < jan1 + 59L + as.integer(leapyear), 0L, 2L - as.integer(leapyear))
  mon <- (12L * prior_days + 373L) %/% 367L
  day <- x - .gregorian_date2offset(data.frame(year = yr, month = mon, day = 1), leapyear) + 1L
  data.frame(year = yr, month = mon, day = day)
}
