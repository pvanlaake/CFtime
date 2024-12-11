#' @title Standard CF calendar
#'
#' @description This class represents a standard calendar of 365 or 366 days per
#'   year. This calendar is compatible with the standard POSIXt calendar for
#'   periods after the introduction of the Gregorian calendar, 1582-10-15
#'   00:00:00. The calendar starts at 0001-01-01 00:00:00, e.g. the start of the
#'   Common Era.
#'
#'   Note that this calendar, despite its name, is not the same as that used in
#'   ISO8601 or many computer systems for periods prior to the introduction of
#'   the Gregorian calendar. Use of the "proleptic_gregorian" calendar is
#'   recommended for periods before or straddling the introduction date, as that
#'   calendar is compatible with POSIXt on most OSes.
#'
#' @importFrom lubridate make_date year month mday days
#' @aliases CFCalendarStandard
#' @docType class
CFCalendarStandard <- R6::R6Class("CFCalendarStandard",
  inherit = CFCalendar,
  public = list(
    #' @field gap The integer offset for 1582-10-15 00:00:00, when the Gregorian
    #' calendar started, or 1582-10-05, when the gap between Julian and
    #' Gregorian calendars started. The former is set when the calendar origin
    #' is more recent, the latter when the origin is prior to the gap.
    gap = -1L,

    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must be "standard" or
    #'   "gregorian" (deprecated).
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    #' @return A new instance of this class.
    initialize = function(nm, definition) {
      super$initialize(nm, definition)

      self$gap <- if (self$is_gregorian_date(self$origin))
        as.integer(lubridate::make_date(1582, 10, 15) -
                   lubridate::make_date(self$origin$year, self$origin$month, self$origin$day))
      else
        .julian_date2offset(data.frame(year = 1582, month = 10, day = 5), self$origin)
    },

    #' @description Indicate which of the supplied dates are valid.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for valid days and `FALSE` for invalid days, or `NA` where
    #'   the row in argument `ymd` has `NA` values.
    valid_days = function(ymd) {
      ymd$year >= 1L & ymd$month >= 1L & ymd$month <= 12L & ymd$day >= 1L &
      ifelse(self$is_gregorian_date(ymd),
        # Gregorian calendar
        ifelse(self$leap_year(ymd$year),
               ymd$day <= c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month],
               ymd$day <= c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month]),
        # Julian calendar
        ifelse(ymd$year == 1582L & ymd$month == 10L & ymd$day > 4L,
               FALSE, # days 1582-10-05 - 1582-10-14 do not exist
               ifelse(ymd$year %% 4L == 0L,
                      ymd$day <= c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month],
                      ymd$day <= c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)[ymd$month])
        )
      )
    },

    #' @description Indicate which of the supplied dates are in the Gregorian
    #'   part of the calendar, e.g. 1582-10-15 or after.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for days in the Gregorian part of the calendar and `FALSE`
    #'   otherwise, or `NA` where the row in argument `ymd` has `NA` values.
    is_gregorian_date = function(ymd) {
      ymd$year > 1582L | (ymd$year == 1582L & (ymd$month > 10L | (ymd$month == 10L & ymd$day >= 15L)))
    },

    #' @description Indicate if the time series described using this calendar
    #'   can be safely converted to a standard date-time type (`POSIXct`,
    #'   `POSIXlt`, `Date`). This is only the case if all offsets are for
    #'   timestamps fall on or after the start of the Gregorian calendar,
    #'   1582-10-15 00:00:00.
    #' @param offsets The offsets from the CFtime instance.
    #' @return `TRUE`.
    POSIX_compatible = function(offsets) {
      all(offsets >= self$gap)
    },

    #' @description Determine the number of days in the month of the calendar.
    #' @param ymd `data.frame`, optional, with dates parsed into their parts.
    #' @return A vector indicating the number of days in each month for the
    #'   dates supplied as argument `ymd`. If no dates are supplied, the number
    #'   of days per month for the calendar as a vector of length 12, for a
    #'   regular year without a leap day.
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
      ifelse(yr <= 1582L,
             yr %% 4L == 0L,
             ((yr %% 4L == 0L) & (yr %% 100L > 0L)) | (yr %% 400L == 0L)
      )
    },

    #' @description Calculate difference in days between a `data.frame` of time
    #'   parts and the origin.
    #'
    #' @param x `data.frame`. Dates to calculate the difference for.
    #' @return Integer vector of a length equal to the number of rows in
    #'   argument `x` indicating the number of days between `x` and the origin
    #'   of the calendar, or `NA` for rows in `x` with `NA` values.
    date2offset = function(x) {
      if (self$gap > 0L) {
        # self$origin in Julian calendar part
        greg0 <- lubridate::make_date(1582, 10, 15)
        ifelse(self$is_gregorian_date(x),
               # Calculate Gregorian dates from 1582-10-15, add gap
               as.integer(lubridate::make_date(x$year, x$month, x$day) - greg0) + self$gap,
               # Calculate julian days from self$origin
               .julian_date2offset(x, self$origin)
        )
      } else {
        # self$origin in Gregorian calendar part
        self_origin <- lubridate::make_date(self$origin$year, self$origin$month, self$origin$day)
        julian0 <- data.frame(year = 1582L, month = 10L, day = 5L)
        ifelse(self$is_gregorian_date(x),
               # Calculate Gregorian dates from self$origin
               as.integer(lubridate::make_date(x$year, x$month, x$day) - self_origin),
               # Calculate julian days from 1582-10-05, add gap
               .julian_date2offset(x, julian0) + self$gap
        )
      }
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      if (self$gap <= 0L && all(x >= self$gap, na.rm = TRUE)) {
        # If self$origin and all offsets are in the Gregorian calendar, use
        # lubridate. Presumed to cover the majority of cases.
        dt <- lubridate::make_date(self$origin$year, self$origin$month, self$origin$day) + lubridate::days(x)
        data.frame(year = lubridate::year(dt), month = lubridate::month(dt), day = lubridate::mday(dt), row.names = NULL)
      } else {
        # Manage cases where self$origin is in the Julian calendar and/or `x`
        # values straddle the Julian/Gregorian boundary.
        common_days <- c(31L, 28L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)
        leap_days   <- c(31L, 29L, 31L, 30L, 31L, 30L, 31L, 31L, 30L, 31L, 30L, 31L)

        # Is the leap day to consider ahead in the year from the base date (offset = 0) or in the next year (offset = 1)
        offset <- as.integer(self$origin$month > 2L)

        # Correct `x` values that straddle the gap from self$origin
        if (self$gap <= 0L) { # Gregorian origin
          ndx <- which(x < self$gap)
          x[ndx] <- x[ndx] - 10L
        } else { # Julian origin
          ndx <- which(x >= self$gap)
          if (length(ndx))
            x[ndx] <- x[ndx] + 10L
        }

        x <- x + self$origin$day
        ymd <- mapply(function(y, m, d) {
          repeat {
            test <- y + offset
            leap <- if (test <= 1582) (test) %% 4L == 0L
                    else ((test %% 4L == 0L) && (test %% 100L > 0L)) || (test %% 400L == 0L)
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
        }, self$origin$year, self$origin$month, x)
        data.frame(year = ymd[1L,], month = ymd[2L,], day = ymd[3L,], row.names = NULL)
      }
    }
  )
)
