#' @title Coordinated Universal Time CF calendar
#'
#' @description This class represents a calendar based on the Coordinated
#'   Universal Time. Validity is from 1972 onwards, with dates represented using
#'   the Gregorian calendar, up to the present (so future timestamps are not
#'   allowed). Leap seconds are considered in all calculations. Also, time zone
#'   information is irrelevant and may not be given.
#'
#'   In general, the calendar should use a unit of time of a second. Minute,
#'   hour and day are allowed but discouraged. Month and year as time unit are
#'   not allowed as there is no practical way to maintain leap second accuracy.
#'
#' @aliases CFCalendarUTC
#' @docType class
CFCalendarUTC <- R6::R6Class("CFCalendarUTC",
  inherit = CFCalendarProleptic,
  private = list(
    # Leap days in various incarnations. More will be added in initialize().
    leapdays = data.frame(
      year      = c(1972L, 1972L, 1973L, 1974L, 1975L, 1976L, 1977L, 1978L, 1979L,
                    1981L, 1982L, 1983L, 1985L, 1987L, 1989L, 1990L, 1992L, 1993L,
                    1994L, 1995L, 1997L, 1998L, 2005L, 2008L, 2012L, 2015L, 2016L),
      month     = c(6L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 12L, 6L, 6L, 6L, 6L,
                    12L, 12L, 12L, 6L, 6L, 6L, 12L, 6L, 12L, 12L, 12L,  6L,  6L, 12L),
      day       = c(30L, 31L, 31L, 31L, 31L, 31L, 31L, 31L, 31L, 30L, 30L, 30L, 30L,
                    31L, 31L, 31L, 30L, 30L, 30L, 31L, 30L, 31L, 31L, 31L, 30L, 30L, 31L),
      compound  = c(19720630L, 19721231L, 19731231L, 19741231L, 19751231L, 19761231L,
                    19771231L, 19781231L, 19791231L, 19810630L, 19820630L, 19830630L,
                    19850630L, 19871231L, 19891231L, 19901231L, 19920630L, 19930630L,
                    19940630L, 19951231L, 19970630L, 19981231L, 20051231L, 20081231L,
                    20120630L, 20150630L, 20161231L),
      epochdays = c(182L, 366L, 731L, 1096L, 1461L, 1827L, 2192L, 2557L, 2922L,
                    3469L, 3834L, 4199L, 4930L, 5844L, 6575L, 6940L, 7487L, 7852L,
                    8217L, 8766L, 9313L, 9862L, 12419L, 13515L, 14792L, 15887L, 16437L)
    ),

    # Offset of the epoch from the origin, in calendar units. Set in initialize().
    epoch = 0,

    # Rata Die of the epoch.
    epoch_rd = 719893L,

    # Number of leap seconds applied to the origin of self. Set in initialize().
    origin_leapsecs = 0L,

    # Check if rows in a data.frame with seconds >= 60 are days when a leap
    # second was applied. Argument `cap` comes from method parse(). If not,
    # values in that row are set to `NA`.
    check_leap_seconds = function(cap) {
      # There will always be at least one row with 60 seconds or this method
      # would not be called
      leap_ndx <- which(cap$second >= 60)
      tst <- cap[leap_ndx, ]
      not2359 <- tst$hour != 23L | tst$minute != 59L
      tst <- tst$year * 10000L + tst$month * 100L + tst$day
      bad <- !(tst %in% private$leapdays$compound)
      cap[leap_ndx[bad | not2359], ] <- rep(NA, 7)
      cap
    }
  ),
  public = list(
    #' @description Create a new CF UTC calendar.
    #' @param nm The name of the calendar. This must be "utc".
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    initialize = function(nm, definition) {
      super$initialize(nm, definition)
      if (self$unit > 4L)
        stop("Unit for an UTC calendar cannot be 'month' or 'year'.", call. = FALSE) # nocov

      # How many leap seconds have been applied to the origin?
      private$origin_leapsecs <- findInterval(self$origin$year * 10000L +
                                              self$origin$month * 100L +
                                              self$origin$day, private$leapdays$compound, left.open = TRUE)

      # Offset of the epoch from the origin
      private$epoch <- self$parse("1972-01-01")$offset

      # Add offsets in self calendar units of the leap seconds
      leapdates <- as.Date(paste(private$leapdays$year, private$leapdays$month, private$leapdays$day, sep = "-"))
      private$leapdays$seconds <-
        as.integer(c(unclass(difftime(leapdates, as.Date("1972-01-01"), units = "secs"))) +
                   CFt$units$per_day[1L] + (1:27))
    },

    #' @description Indicate which of the supplied dates are valid.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for valid days and `FALSE` for invalid days, or `NA` where
    #'   the row in argument `ymd` has `NA` values.
    valid_days = function(ymd) {
      nw <- as.POSIXct(Sys.Date(), tz = "UTC")
      yr <- as.integer(substr(nw, 1, 4))
      mon <- as.integer(substr(nw, 6, 7))
      dt <- as.integer(substr(nw, 9, 10))
      super$valid_days(ymd) & ymd$year >= 1972L &
        (ymd$year < yr | (ymd$year == yr & (ymd$month < mon |
          (ymd$month == mon & ymd$day <= dt))))
    },

    #' @description Parsing a vector of date-time character strings into parts.
    #' This includes any leap seconds. Time zone indications are not allowed.
    #' @param d character. A character vector of date-times.
    #' @return A `data.frame` with columns year, month, day, hour, minute,
    #'   second, time zone, and offset. Invalid input data will appear as `NA`.
    #'   Note that the time zone is always "+0000" and is included to maintain
    #'   compatibility with results from other calendars.
    parse = function(d) {
      # Parsers
      # These parsers are specific to UTC, i.e. starting in 1972 and up to the
      # current year, and a possible leap second as value 60.

      # UDUNITS broken timestamp definition, with some changes
      # broken_timestamp	{broken_date}({space|T}+{broken_clock})? -- T not in definition but present in lexer code
      # broken_date		    {year}-{month}(-{day})?
      # year		        	[+-]?[0-9]{1,4}
      # month			        0?[1-9]|1[0-2]
      # day			          0?[1-9]|[1-2][0-9]|30|31
      # broken_clock		  {hour}:{minute}(:{second})?
      # hour			        [0-1]?[0-9]|2[0-3]      -- sign on hour not allowed
      # minute			      [0-5]?[0-9]
      # second		      	60|{minute}?
      # fractional part   (\.[0-9]*)?
      broken <- paste0(
        "^",                                    # anchor string at start
        "([+-]?[0-9]{1,4})",                    # year, with optional sign
        "-(0?[1-9]|1[012])",                    # month
        "(?:-(0?[1-9]|[12][0-9]|3[01]))?",      # day, optional
        "(?:[T ]",                              # if a time is following, separate with a single whitespace character or a "T"
        "([01]?[0-9]|2[0-3])",                  # hour
        ":([0-5]?[0-9])",                       # minute
        "(?::([0-6]?[0-9]))?",                  # second, optional
        "(?:\\.([0-9]*))?",                     # optional fractional part of the smallest specified unit
        ")?",                                   # close optional time capture group
        "(?:\\s",                               # if a time zone offset is following, separate with a single whitespace character
        "([+-])?([01]?[0-9]|2[0-3])",           # tz hour, with optional sign
        "(?::(00|15|30|45))?",                  # optional tz minute, only 4 possible values
        ")?",                                   # close optional timezone capture group
        "$"                                     # anchor string at end
      )

      iso8601 <- paste0(
        "^",
        "([0-9]{4})",
        "-(0[1-9]|1[012])",
        "-(0[1-9]|[12][0-9]|3[01])?",
        "(?:",
        "[T ]([01][0-9]|2[0-3])",
        "(?::([0-5][0-9]))?",
        "(?::([0-6][0-9]))?",
        "(?:[\\.,]([0-9]*))?",
        ")?",
        "(?:([Z+-])([01][0-9]|2[0-3])?(?::(00|15|30|45))?", ## FIXME: Z?, smaller number of captures
        ")?$"
      )

      # UDUNITS packed timestamp definition - NOT YET USED
      # packed_timestamp	{packed_date}({space|T}+{packed_clock})?  -- T and space only allowed in packed time follows
      # packed_date	    	{year}({month}{day}?)?           -- must be YYYYMMDD or else format is ambiguous, as per lexer code
      # packed_clock		  {hour}({minute}{second}?)?       -- must be HHMMSS to be unambiguous
      # packed <- stringi::stri_join(
      #   "^",                                    # anchor string at start
      #   "([+-]?[0-9]{4})",                      # year, with optional sign
      #   "(0[1-9]|1[012])?",                     # month, optional
      #   "(0[1-9]|[12][0-9]|3[01])?",            # day, optional
      #   "(?:[T,\\s]",                           # if a time is following, separate with a single whitespace character or a "T"
      #     "([01][0-9]|2[0-3])?",                # hour
      #     "([0-5][0-9])?",                      # minute, optional
      #     "([0-5]?[0-9](?:\\.[0-9]*)?)?",       # second, optional, with optional fractional part
      #   ")?",                                   # close optional time capture group
      #   "$"                                     # anchor string at end
      # )

      parse <- data.frame(year = integer(), month = integer(), day = integer(),
                          hour = integer(), minute = integer(), second = numeric(), frac = character(),
                          tz_sign = character(), tz_hour = character(), tz_min = character())

      cap <- utils::strcapture(iso8601, d, parse)
      missing <- which(is.na(cap$year))
      if (length(missing) > 0L)
        cap[missing,] <- utils::strcapture(broken, d[missing], parse)

      # Assign any fraction to the appropriate time part
      cap$frac[is.na(cap$frac)] <- "0"
      frac <- as.numeric(paste0("0.", cap$frac))
      if (sum(frac) > 0) {
        ndx <- which(!(is.na(cap$second)) & frac > 0)
        if (length(ndx) > 0L) cap$second[ndx] <- cap$second[ndx] + frac[ndx]
        ndx <- which(!(is.na(cap$minute)) & is.na(cap$second) & frac > 0)
        if (length(ndx) > 0L) cap$second[ndx] <- 60L * frac[ndx]
        ndx <- which(!(is.na(cap$hour)) & is.na(cap$minute) & frac > 0)
        if (length(ndx) > 0L) {
          secs <- 3600 * frac
          cap$minute[ndx] <- secs[ndx] %/% 60
          cap$second[ndx] <- secs[ndx] %% 60
        }
      }
      cap$frac <- NULL

      # Convert NA time parts to 0 - in CF default time is 00:00:00 when not specified
      cap$hour[is.na(cap$hour)] <- 0L
      cap$minute[is.na(cap$minute)] <- 0L
      cap$second[is.na(cap$second)] <- 0L

      # Set timezone to 00:00 to align cap data.frame with other calendars
      cap$tz_sign <- cap$tz_hour <- cap$tz_min <- NULL
      cap$tz <- "+0000"

      # Set optional date parts to 1 if not specified
      cap$month[is.na(cap$month)] <- 1L
      cap$day[is.na(cap$day)] <- 1L

      # Check date validity
      invalid <- !self$valid_days(cap)
      invalid[is.na(invalid)] <- TRUE
      if (sum(invalid) > 0L) cap[invalid,] <- rep(NA, 7)

      # Check that any supplied leap seconds coincide with official leap seconds
      if (any(cap$second >= 60, na.rm = TRUE))
        cap <- private$check_leap_seconds(cap)

      # Calculate offsets
      if (nrow(self$origin) == 0L) {        # if there's no origin yet, don't calculate offsets
        cap$offset <- rep(0, nrow(cap))     # this happens, f.i., when a CFCalendar is created
      } else {
        days <- self$date2offset(cap)
        chkdays <- cap$year * 10000L + cap$month * 100L + cap$day
        leapsecs <- findInterval(chkdays, private$leapdays$compound, left.open = TRUE) - private$origin_leapsecs
        cap$offset <- round((days * 86400 + (cap$hour - self$origin$hour[1]) * 3600 +
                             (cap$minute - self$origin$minute[1]) * 60 +
                             cap$second - self$origin$second + leapsecs) /
                            CFt$units$seconds[self$unit], 9)
      }
      cap
    },

    #' @description Decompose a vector of offsets, in units of the calendar, to
    #'   their timestamp values. This adds a specified amount of time to the
    #'   origin of a `CFTime` object.
    #' @param offsets Vector of numeric offsets to add to the origin of the
    #'   calendar.
    #' @return A `data.frame` with columns for the timestamp elements and as
    #'   many rows as there are offsets.
    offsets2time = function(offsets) {
      len <- length(offsets)
      if(len == 0L)
        return(data.frame(year = integer(), month = integer(), day = integer(),
                          hour = integer(), minute = integer(), second = numeric(),
                          tz = character(), offset = numeric()))

      # Base offsets on epoch, in seconds
      off <- (offsets - private$epoch) * CFt$units$seconds[self$unit]
      ndx <- findInterval(off, private$leapdays$seconds)
      remainder <- off - ifelse(ndx > 0L, private$leapdays$seconds[ndx], 0L)

      secs <- mins <- hrs <- days <- vector("numeric", len)
      frac <- rep(0, len)
      leap <- ndx < 27L & round(private$leapdays$seconds[ndx + 1L] - off, 5) <= 1L
      if (sum(leap) > 0L) {
        secs[leap] <- 60
        mins[leap] <- 59
        hrs[leap] <- 23
        days[leap] <- ifelse(ndx[leap] == 0L,
                             private$leapdays$epochdays[1L],
                             private$leapdays$epochdays[ndx + 1L] - private$leapdays$epochdays[ndx]) - 1L
      }
      not_leap <- !leap
      if (sum(not_leap) > 0L) {
        # Convert remainder to time parts, no leap seconds anymore
        days[not_leap] <- remainder[not_leap] %/% 86400L            # overflow days
        secs[not_leap] <- round(remainder[not_leap] %% 86400L, 3L)  # round down to milli-seconds to avoid errors
        #frac[not_leap] <- secs[not_leap] %% 1
        #secs[not_leap] <- secs[not_leap] %/% 1

        # Time elements for output
        hrs[not_leap] <- secs[not_leap] %/% 3600L
        mins[not_leap] <- (secs[not_leap] %% 3600L) %/% 60L
        secs[not_leap] <- secs[not_leap] %% 60L
      }

      # Now add days using the calendar
      epoch_days <- ifelse(ndx > 0L, private$leapdays$epochdays[ndx], 0L)
      out <- if (any(days != 0L))
        .gregorian_offset2date(as.integer(days) + private$epoch_rd + epoch_days)
      else
        data.frame(year = rep(self$origin$year, len),
                   month = rep(self$origin$month, len),
                   day = rep(self$origin$day, len))

      # Put it all back together again
      out$hour <- as.integer(hrs)
      out$minute <- as.integer(mins)
      out$second <- secs
      #if (sum(frac) > 0)
      #  out$second <- out$second + frac
      out$tz <- rep("+0000", len)

      out$offset <- offsets
      out
    }
  )
)
