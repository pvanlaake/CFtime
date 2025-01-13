#' @import R6
NULL

#' @title Basic CF calendar
#'
#' @description This class represents a basic CF calendar. It should not be
#'   instantiated directly; instead, use one of the descendant classes.
#'
#'   This internal class stores the information to represent date and time
#'   values using the CF conventions. An instance is created by the exported
#'   [CFTime] class, which also exposes the relevant properties of this class.
#'
#'   The following calendars are supported:
#'
#' \itemize{
#'   \item [`gregorian\standard`][CFCalendarStandard], the international standard calendar for civil use.
#'   \item [`proleptic_gregorian`][CFCalendarProleptic], the standard calendar but extending before 1582-10-15
#'   when the Gregorian calendar was adopted.
#'   \item [`tai`][CFCalendarTAI], International Atomic Time clock with dates expressed using the Gregorian calendar.
#'   \item [`utc`][CFCalendarUTC], Coordinated Universal Time clock with dates expressed using the Gregorian calendar.
#'   \item [`julian`][CFCalendarJulian], every fourth year is a leap year (so including the years 1700, 1800, 1900, 2100, etc).
#'   \item [`noleap\365_day`][CFCalendar365], all years have 365 days.
#'   \item [`all_leap\366_day`][CFCalendar366], all years have 366 days.
#'   \item [`360_day`][CFCalendar360], all years have 360 days, divided over 12 months of 30 days.
#' }
#' @references
#'   https://cfconventions.org/Data/cf-conventions/cf-conventions-1.12/cf-conventions.html#calendar
#' @docType class
CFCalendar <- R6::R6Class("CFCalendar",
  public = list(
    #' @field name Descriptive name of the calendar, as per the CF Metadata
    #' Conventions.
    name = "",

    #' @field definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    definition = "",

    #' @field unit The numeric id of the unit of the calendar.
    unit = -1L,

    #' @field origin `data.frame` with fields for the origin of the calendar.
    origin = data.frame(),

    #' @description Create a new CF calendar.
    #' @param nm The name of the calendar. This must follow the CF Metadata
    #' Conventions.
    #' @param definition The string that defines the units and the origin, as
    #' per the CF Metadata Conventions.
    initialize = function(nm, definition) {
      stopifnot(length(definition) ==  1L, length(nm) == 1L)
      self$name <- tolower(nm)
      self$definition <- definition

      parts <- strsplit(definition, " ")[[1L]]
      if ((length(parts) < 3L) || !(tolower(parts[2L]) %in% c("since", "after", "from", "ref", "per")))
        stop("Definition string does not appear to be a CF-compliant time coordinate description", call. = FALSE)
      u <- which(CFt$CFunits$unit == tolower(parts[1L]))
      if (length(u) == 0L) stop("Unsupported unit: ", parts[1L], call. = FALSE)
      self$unit <- CFt$CFunits$id[u]

      dt <- self$parse(paste(parts[3L:length(parts)], collapse = " "))
      if (is.na(dt$year[1L]))
        stop("Definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification", call. = FALSE)
      self$origin <- dt
    },

    #' @description Print information about the calendar to the console.
    #' @param ... Ignored.
    #' @return `self`, invisibly.
    print = function(...) {
      tz <- self$timezone
      if (tz == "+0000") tz <- ""
      cat("CF calendar:",
          "\n  Origin  : ", self$origin_date, " ", self$origin_time, tz,
          "\n  Units   : ", CFt$units$name[self$unit],
          "\n  Type    : ", self$name, "\n",
          sep = "")
      invisible(self)
    },

    #' @description Indicate which of the supplied dates are valid.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. Any other columns are disregarded.
    #' @return `NULL`. A warning will be generated to the effect that a
    #' descendant class should be used for this method.
    valid_days = function(ymd) {
      warning("Use a descendant class from `CFCalendar` to call this method.", call. = FALSE)
      NULL
    },

    #' @description Indicate if the time series described using this calendar
    #'   can be safely converted to a standard date-time type (`POSIXct`,
    #'   `POSIXlt`, `Date`).
    #'
    #'   Only the 'standard' calendar and the 'proleptic_gregorian' calendar
    #'   when all dates in the time series are more recent than 1582-10-15
    #'   (inclusive) can be safely converted, so this method returns `FALSE` by
    #'   default to cover the majority of cases.
    #' @param offsets The offsets from the CFtime instance.
    #' @return `FALSE` by default.
    POSIX_compatible = function(offsets) {
      FALSE
    },

    #' @description This method tests if the `CFCalendar` instance in argument
    #'   `cal` is compatible with `self`, meaning that they are of the same
    #'   class and have the same unit. Calendars "standard", and "gregorian" are
    #'   compatible, as are the pairs of "365_day" and "no_leap", and "366_day"
    #'   and "all_leap".
    #' @param cal Instance of a descendant of the `CFCalendar` class.
    #' @return `TRUE` if the instance in argument `cal` is compatible with
    #'   `self`, `FALSE` otherwise.
    is_compatible = function(cal) {
      self$unit == cal$unit && class(self)[1L] == class(cal)[1L]
    },

    #' @description This method tests if the `CFCalendar` instance in argument
    #'   `cal` is equivalent to `self`, meaning that they are of the same class,
    #'   have the same unit, and equivalent origins. Calendars "standard", and
    #'   "gregorian" are equivalent, as are the pairs of "365_day" and
    #'   "no_leap", and "366_day" and "all_leap".
    #'
    #'   Note that the origins need not be identical, but their parsed values
    #'   have to be. "2000-01" is parsed the same as "2000-01-01 00:00:00", for
    #'   instance.
    #' @param cal Instance of a descendant of the `CFCalendar` class.
    #' @return `TRUE` if the instance in argument `cal` is equivalent to
    #'   `self`, `FALSE` otherwise.
    is_equivalent = function(cal) {
      sum(self$origin[1L,1L:6L] == cal$origin[1L,1L:6L]) == 6L &&  # Offset column is NA
      self$is_compatible(cal)
    },

    #' @description Parsing a vector of date-time character strings into parts.
    #' @param d character. A character vector of date-times.
    #' @return A `data.frame` with columns year, month, day, hour, minute,
    #'   second, time zone, and offset. Invalid input data will appear as `NA`.
    parse = function(d) {
      # Parsers

      # UDUNITS broken timestamp definition, with some changes
      # broken_timestamp	{broken_date}({space|T}+{broken_clock})? -- T not in definition but present in lexer code
      # broken_date		    {year}-{month}(-{day})?
      # year		        	[+-]?[0-9]{1,4}
      # month			        0?[1-9]|1[0-2]
      # day			          0?[1-9]|[1-2][0-9]|30|31
      # broken_clock		  {hour}:{minute}(:{second})?
      # hour			        [0-1]?[0-9]|2[0-3]      -- sign on hour not allowed, but see timezone
      # minute			      [0-5]?[0-9]
      # second		      	{minute}?               -- leap second not supported
      # fractional part   (\.[0-9]*)?
      # timezone          [+-]?{hour}(:{minute})? -- added, present in lexer code
      broken <- paste0(
        "^",                                    # anchor string at start
        "([+-]?[0-9]{1,4})",                    # year, with optional sign
        "-(0?[1-9]|1[012])",                    # month
        "(?:-(0?[1-9]|[12][0-9]|3[01]))?",      # day, optional
        "(?:[T ]",                              # if a time is following, separate with a single whitespace character or a "T"
        "([01]?[0-9]|2[0-3])",                  # hour
        ":([0-5]?[0-9])",                       # minute
        "(?::([0-5]?[0-9]))?",                  # second, optional
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
        "(?::([0-5][0-9]))?",
        "(?:[\\.,]([0-9]*))?",
        ")?",
        "(?:([Z+-])([01][0-9]|2[0-3])?(?::(00|15|30|45))?", ## FIXME: Z?, smaller number of captures
        ")?$"
      )

      # UDUNITS packed timestamp definition - NOT YET USED
      # packed_timestamp	{packed_date}({space|T}+{packed_clock})?  -- T and space only allowed in packed time follows
      # packed_date	    	{year}({month}{day}?)?           -- must be YYYYMMDD or else format is ambiguous, as per lexer code
      # packed_clock		  {hour}({minute}{second}?)?       -- must be HHMMSS to be unambiguous
      # timezone          [+-]?{hour}({minute})?           -- added, present in lexer code, must be HHMM
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
      #   "(?:\\s",                               # if a time zone offset is following, separate with a single whitespace character
      #     "([+-]?[01][0-9]|2[0-3])?",           # hour, with optional sign
      #     "(00|15|30|45)?",                     # minute, only 4 possible values
      #   ")?",                                   # close optional timezone capture group
      #   "$"                                     # anchor string at end
      # )

      parse <- data.frame(year = integer(), month = integer(), day = integer(),
                          hour = integer(), minute = integer(), second = numeric(), frac = character(),
                          tz_sign = character(), tz_hour = character(), tz_min = character())

      # Drop "UTC", if given
      d <- trimws(gsub("UTC$", "", d))

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

      # Set timezone to default value where needed
      ndx <- which(cap$tz_sign == "Z")
      if (length(ndx) > 0L) {
        cap$tz_sign[ndx] <- "+"
        cap$tz_hour[ndx] <- "00"
        cap$tz_min[ndx] <- "00"
      }
      cap$tz <- paste0(ifelse(cap$tz_sign == "", "+", cap$tz_sign),
                       ifelse(cap$tz_hour == "", "00", cap$tz_hour),
                       ifelse(cap$tz_min == "", "00", cap$tz_min))
      cap$tz <- ifelse(cap$tz =="NANANA", "+0000", cap$tz)
      cap$tz_sign <- cap$tz_hour <- cap$tz_min <- NULL

      # Set optional date parts to 1 if not specified
      cap$month[is.na(cap$month)] <- 1L
      cap$day[is.na(cap$day)] <- 1L

      # Check date validity
      invalid <- !self$valid_days(cap)
      invalid[is.na(invalid)] <- TRUE
      if (sum(invalid) > 0L) cap[invalid,] <- rep(NA, 7)

      # Calculate offsets
      if (nrow(self$origin) == 0L) {        # if there's no origin yet, don't calculate offsets
        cap$offset <- rep(0, nrow(cap))     # this happens, f.i., when a CFCalendar is created
      } else {
        days <- self$date2offset(cap)
        cap$offset <- round((days * 86400 + (cap$hour - self$origin$hour[1]) * 3600 +
                             (cap$minute - self$origin$minute[1]) * 60 +
                             cap$second - self$origin$second) / CFt$units$seconds[self$unit], 6)
      }
      cap
    },

    #' @description Decompose a vector of offsets, in units of the calendar, to
    #'   their timestamp values. This adds a specified amount of time to the
    #'   origin of a `CFTime` object.
    #'
    #'   This method may introduce inaccuracies where the calendar unit is
    #'   "months" or "years", due to the ambiguous definition of these units.
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

      if (self$unit <= 4L) { # Days, hours, minutes, seconds
        # First add time: convert to seconds first, then recompute time parts
        secs <- offsets * CFt$units$seconds[self$unit] +
                self$origin$hour * 3600 + self$origin$minute * 60 + self$origin$second
        days <- secs %/% 86400L            # overflow days
        secs <- round(secs %% 86400L, 3L)  # drop overflow days from time, round down to milli-seconds to avoid errors

        # Time elements for output
        hrs <- secs %/% 3600L
        mins <- (secs %% 3600L) %/% 60L
        secs <- secs %% 60L

        # Now add days using the calendar
        out <- if (any(days != 0L))
          self$offset2date(days)
        else
          data.frame(year = rep(self$origin$year, len),
                     month = rep(self$origin$month, len),
                     day = rep(self$origin$day, len))

        # Put it all back together again
        out$hour <- hrs
        out$minute <- mins
        out$second <- secs
        out$tz <- rep(self$timezone, len)
      } else { # Months, years
        out <- self$origin[rep(1L, len), ]
        if (self$unit == 5L) { # Offsets are months
          months <- out$month + offsets - 1L
          out$month <- months %% 12L + 1L
          out$year <- out$year + months %/% 12L
        } else {               # Offsets are years
          out$year <- out$year + offsets
        }
      }
      out$offset <- offsets
      out
    }

  ),
  active = list(
    #' @field origin_date (read-only) Character string with the date of the
    #'   calendar.
    origin_date = function(value) {
      if (missing(value)) {
        sprintf("%04d-%02d-%02d", self$origin$year, self$origin$month, self$origin$day)
      }
    },

    #' @field origin_time (read-only) Character string with the time of the
    #'   calendar.
    origin_time = function(value) {
      if (missing(value)) {
        .format_time(self$origin)
      }
    },

    #' @field timezone (read-only) Character string with the time zone of the
    #' origin of the calendar.
    timezone = function(value) {
      if (missing(value))
        self$origin$tz
    }
  )
)
