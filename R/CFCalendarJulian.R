#' @title Julian CF calendar
#'
#' @description This class represents a Julian calendar of 365 days per year,
#'   with every fourth year being a leap year of 366 days. The months and the
#'   year align with the standard calendar. This calendar is not compatible with
#'   the standard POSIXt calendar.
#'
#'   This calendar starts on 1 January of year 1: 0001-01-01 00:00:00. Any dates
#'   before this will generate an error.
#'
#' @aliases CFCalendarJulian
#' @docType class
CFCalendarJulian <- R6::R6Class("CFCalendarJulian",
  inherit = CFCalendar,
  public = list(
    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must be "julian". This argument
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
      ymd$year >= 1L & ymd$month >= 1L & ymd$month <= 12L & ymd$day >= 1L &
      ifelse(self$leap_year(ymd$year),
        ymd$day <= c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[ymd$month],
        ymd$day <= c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[ymd$month])
    },

    #' @description Determine the number of days in the month of the calendar.
    #' @param ymd `data.frame`, optional, with dates parsed into their parts.
    #' @return A vector indicating the number of days in each month for the
    #'   dates supplied as argument `ymd`. If no dates are supplied, the number
    #'   of days per month for the calendar as a vector of length 12, for a
    #'   regular year without a leap day.
    month_days = function(ymd = NULL) {
      if (is.null(ymd)) return(c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31))

      ifelse(self$leap_year(ymd$year),
             c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[ymd$month],
             c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)[ymd$month])
    },

    #' @description Indicate which years are leap years.
    #' @param yr Integer vector of years to test.
    #' @return Logical vector with the same length as argument `yr`. `NA` is
    #'   returned where elements in argument `yr` are `NA`.
    leap_year = function(yr) {
      yr %% 4L == 0L
    },

    #' @description Calculate difference in days between a `data.frame` of time
    #'   parts and the origin.
    #'
    #' @param x `data.frame`. Dates to calculate the difference for.
    #' @return Integer vector of a length equal to the number of rows in
    #'   argument `x` indicating the number of days between `x` and the origin
    #'   of the calendar, or `NA` for rows in `x` with `NA` values.
    date2offset = function(x) {
      .julian_date2offset(x, self$origin)
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      common_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
      leap_days   <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

      # Is the leap day to consider ahead in the year from the base date (offset = 0) or in the next year (offset = 1)
      offset <- as.integer(self$origin$month > 2L)

      # First process 4-year cycles of 1,461 days over the vector
      yr <- self$origin$year + (x %/% 1461L) * 4L
      x <- x %% 1461L

      # Remaining portion relative to the origin
      x <- x + self$origin$day
      ymd <- mapply(function(y, m, d) {
        repeat {
          leap <- (y + offset) %% 4L == 0L
          ydays <- 365L + as.integer(leap)

          if (d <= 0L) {
            d <- d + ydays
            y <- y - 1L
            if (d > 0L) break
          } else if (d > ydays) {
            d <- d - ydays
            y <- y + 1L
          } else break
        }

        month <- if (leap) leap_days else common_days

        while (d > month[m]) {
          d <- d - month[m]
          m <- m + 1L
          if (m == 13L) {
            y <- y + 1L
            m <- 1L
          }
        }
        return(c(y, m, d))
      }, yr, self$origin$month, x)
      data.frame(year = ymd[1L,], month = ymd[2L,], day = ymd[3L,], row.names = NULL)
    }
  )
)

# Internal function to calculate dates from offsets. This function is here
# because it is used by CFCalendarStandard. See further description in the
# methods.
.julian_date2offset <- function(x, origin) {
  # days diff of 1st of month to 1 January in normal year
  yd0 <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L)

  origin_year <- origin$year
  days_into_year <- yd0[origin$month] + origin$day
  if (origin$month <= 2L && origin_year %% 4L == 0L)
    days_into_year <- days_into_year - 1L

  mapply(function(y, m, d) {
    if (is.na(y)) return(NA_integer_)

    # Adjust for where the leap day falls
    if (y >= origin_year)
      days <- if (m <= 2L && y %% 4L == 0L) -1L else 0L
    else
      days <- if (m > 2L && y %% 4L == 0L) 0L else -1L

    repeat {
      if (y > origin_year) {
        days <- days + 365L + as.integer(y %% 4L == 0L)
        y <- y - 1L
      } else if (y < origin_year) {
        days <- days - 365L - as.integer(y %% 4L == 0L)
        y <- y + 1L
      } else break
    }
    days + yd0[m] + d - days_into_year
  }, x$year, x$month, x$day)
}
