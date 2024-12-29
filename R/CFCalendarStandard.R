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
#' @aliases CFCalendarStandard
#' @docType class
CFCalendarStandard <- R6::R6Class("CFCalendarStandard",
  inherit = CFCalendar,
  private = list(
    # Rata Die, the number of days from the day before 0001-01-01 to
    # origin of the Gregorian part of this calendar, if present. Used to
    # convert offsets from the Gregorian calendar origin to the day before
    # 0001-01-01 for arithmetic calculations.
    rd_greg = 0L,

    # Rata Die, the number of days from the day before 0001-01-01 to
    # origin of the Julian part of this calendar, if present. Used to convert
    # offsets from the Julian calendar origin to the day before 0001-01-01
    #for arithmetic calculations.
    rd_juli = 0L,

    # The integer offset for 1582-10-15 00:00:00, when the Gregorian
    # calendar started, or 1582-10-05, when the gap between Julian and
    # Gregorian calendars started. The former is set when the calendar origin
    # is more recent, the latter when the origin is prior to the gap.
    gap = -1L
  ),
  public = list(
    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must be "standard" or
    #'   "gregorian" (deprecated).
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    #' @return A new instance of this class.
    initialize = function(nm, definition) {
      super$initialize(nm, definition)
      private$rd_greg <- .gregorian_date2offset(self$origin, self$leap_year(self$origin$year))
      private$rd_juli <- .julian_date2offset(self$origin, self$leap_year(self$origin$year))

      private$gap <- if (self$is_gregorian_date(self$origin))
        .gregorian_date2offset(data.frame(year = 1582, month = 10, day = 15), self$leap_year(self$origin$year)) - private$rd_greg
      else
        .julian_date2offset(data.frame(year = 1582, month = 10, day = 5), self$leap_year(self$origin$year)) - private$rd_juli
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
      all(offsets >= private$gap)
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
      leap <- self$leap_year(x$year)
      ifelse(self$is_gregorian_date(x),
             .gregorian_date2offset(x, leap),
             .julian_date2offset(x, leap)
      ) - if (private$gap > 0L) private$rd_juli else private$rd_greg
    },

    #' @description Calculate date parts from day differences from the origin. This
    #'   only deals with days as these are impacted by the calendar.
    #'   Hour-minute-second timestamp parts are handled in [CFCalendar].
    #'
    #' @param x Integer vector of days to add to the origin.
    #' @return A `data.frame` with columns 'year', 'month' and 'day' and as many
    #'   rows as the length of vector `x`.
    offset2date = function(x) {
      rd <- if (private$gap > 0L) private$rd_juli else private$rd_greg
      len <- length(x)
      gndx <- x >= private$gap & !is.na(x)
      if (any(gndx)) greg <- .gregorian_offset2date(x[gndx] + rd)
      else greg <- data.frame()
      jndx <- x < private$gap & !is.na(x)
      if (any(jndx)) juli <- .julian_offset2date(x[jndx] + rd)
      else juli <- data.frame()
      yr <- mon <- day <- rep(NA_integer_, len)
      yr[gndx]  <- greg$year;  yr[jndx]  <- juli$year
      mon[gndx] <- greg$month; mon[jndx] <- juli$month
      day[gndx] <- greg$day;   day[jndx] <- juli$day
      data.frame(year = yr, month = mon, day = day)
    }
  )
)
