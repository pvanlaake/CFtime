#' @title 366-day CF calendar
#'
#' @description This class represents a CF calendar of 366 days per year, having
#'   leap days in every year. This calendar is not compatible with the standard
#'   POSIXt calendar.
#'
#'   This calendar supports dates before year 1 and includes the year 0.
#'
#' @aliases CFCalendar366
#' @docType class
CFCalendar366 <- R6::R6Class("CFCalendar366",
  inherit = CFCalendar,
  private = list(
    # Rata Die, the number of days from the day before 0001-01-01 to
    # origin of this calendar. Used to convert offsets from the calendar origin
    # to the day before 0001-01-01 for arithmetic calculations.
    rd = 0L
  ),
  public = list(
    #' @description Create a new CF calendar of 366 days per year.
    #' @param nm The name of the calendar. This must be "366_day" or "all_leap".
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    #' @return A new instance of this class.
    initialize = function(nm, definition) {
      super$initialize(nm, definition)
      private$rd <- self$date2offset(self$origin)
    },

    #' @description Indicate which of the supplied dates are valid.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for valid days and `FALSE` for invalid days, or `NA` where
    #'   the row in argument `ymd` has `NA` values.
    valid_days = function(ymd) {
      ymd$year & ymd$month >= 1L & ymd$month <= 12L & ymd$day >= 1L &
      ymd$day <= c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month]
    },

    #' @description Determine the number of days in the month of the calendar.
    #' @param ymd `data.frame`, optional, with dates parsed into their parts.
    #' @return A vector indicating the number of days in each month for the
    #'   dates supplied as argument `ymd`. If no dates are supplied, the number
    #'   of days per month for the calendar as a vector of length 12.
    month_days = function(ymd = NULL) {
      if (is.null(ymd)) return(c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L))

      res <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month]
      res[which(is.na(ymd$year))] <- NA
      res
    },

    #' @description Indicate which years are leap years.
    #' @param yr Integer vector of years to test.
    #' @return Logical vector with the same length as argument `yr`. Since in
    #'   this calendar all years have a leap day, all values will be `TRUE`, or
    #'   `NA` where argument `yr` is `NA`.
    leap_year = function(yr) {
      res <- rep(TRUE, length(yr))
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
      corr <- ifelse(x$month <= 2L, 0L, -1L)
      366L * (x$year - 1L) + (367L * x$month - 362L) %/% 12L + corr + x$day - private$rd
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      d0 <- x - 1L + private$rd        # d0 is offset relative to year 0, 0-based
      yr <- d0 %/% 366L + 1L           # full years
      d1 <- d0 %% 366L                 # remaining days
      corr <- ifelse(d1 < 60L, 0L, 1L) # correct for days past February
      mon <- (12L * (d1 + corr) + 373L) %/% 367L
      day <- d1 - (367L * mon - 362L) %/% 12L + corr + 1L
      data.frame(year = yr, month = mon, day = day)
    }
  )
)
