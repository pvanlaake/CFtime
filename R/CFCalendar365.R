#' @title 365-day CF calendar
#'
#' @description This class represents a CF calendar of 365 days per year, having
#'   no leap days in any year. This calendar is not compatible with the standard
#'   POSIXt calendar.
#'
#'   This calendar supports dates before year 1 and includes the year 0.
#'
#' @aliases CFCalendar365
#' @docType class
CFCalendar365 <- R6::R6Class("CFCalendar365",
  inherit = CFCalendar,
  public = list(
    #' @description Create a new CF calendar of 365 days per year.
    #' @param nm The name of the calendar. This must be "365_day" or "noleap".
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
      ymd$day <= c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month]
    },

    #' @description Determine the number of days in the month of the calendar.
    #' @param ymd `data.frame`, optional, with dates parsed into their parts.
    #' @return A vector indicating the number of days in each month for the
    #'   dates supplied as argument `ymd`. If no dates are supplied, the number
    #'   of days per month for the calendar as a vector of length 12.
    month_days = function(ymd = NULL) {
      if (is.null(ymd)) return(c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L))

      res <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month]
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
      yd0 <- c(0L, 31L, 59L, 90L, 120L, 151L, 181L, 212L, 243L, 273L, 304L, 334L) # days diff of 1st of month to 1 January
      (x$year - self$origin$year) * 365L + yd0[x$month] - yd0[self$origin$month] + x$day - self$origin$day
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      month <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

      # First process full years over the vector
      yr <- self$origin$year + (x %/% 365L)
      x <- x %% 365L

      # Remaining portion relative to the origin
      x <- x + self$origin$day
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
      }, yr, self$origin$month, x)
      data.frame(year = ymd[1L,], month = ymd[2L,], day = ymd[3L,], row.names = NULL)
    }
  )
)
