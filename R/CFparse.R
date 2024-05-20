#' Parse series of timestamps in CF format to date-time elements
#'
#' This function will parse a vector of timestamps in ISO8601 or UDUNITS format
#' into a data frame with columns for the elements of the timestamp: year,
#' month, day, hour, minute, second, time zone. Those timestamps that could not
#' be parsed or which represent an invalid date in the indicated `CFtime`
#' instance will have `NA` values for the elements of the offending timestamp
#' (which will generate a warning).
#'
#' The supported formats are the *broken timestamp* format from the UDUNITS
#' library and ISO8601 *extended*, both with minor changes, as suggested by the
#' CF Metadata Conventions. In general, the format is `YYYY-MM-DD hh:mm:ss.sss
#' hh:mm`. The year can be from 1 to 4 digits and is interpreted literally, so
#' `79-10-24` is the day Mount Vesuvius erupted and destroyed Pompeii, not
#' `1979-10-24`. The year and month are mandatory, all other fields are
#' optional. There are defaults for all missing values, following the UDUNITS
#' and CF Metadata Conventions. Leading zeros can be omitted in the UDUNITS
#' format, but not in the ISO8601 format. The optional fractional part can have
#' as many digits as the precision calls for and will be applied to the smallest
#' specified time unit. In the result of this function, if the fraction is
#' associated with the minute or the hour, it is converted into a regular
#' `hh:mm:ss.sss` format, i.e. any fraction in the result is always associated
#' with the second, rounded down to milli-second accuracy. The separator between
#' the date and the time can be a single whitespace character or a `T`.
#'
#' The time zone is optional and should have at least the hour or `Z` if
#' present, the minute is optional. The time zone hour can have an optional
#' sign. In the UDUNITS format the separator between the time and the time zone
#' must be a single whitespace character, in ISO8601 there is no separation
#' between the time and the timezone. Time zone names are not supported (as
#' neither UDUNITS nor ISO8601 support them) and will cause parsing to fail when
#' supplied, with one exception: the designator "UTC" is silently dropped (i.e.
#' interpreted as "00:00").
#'
#' Currently only the extended formats (with separators between the elements)
#' are supported. The vector of timestamps may have any combination of ISO8601
#' and UDUNITS formats.
#'
#' Timestamps that are prior to the datum are not allowed. The corresponding row
#' in the result will have `NA` values.
#'
#' @param cf CFtime. An instance of `CFtime` indicating the CF calendar and
#'   datum to use when parsing the date.
#' @param x character. Vector of character strings representing timestamps in
#'   ISO8601 extended or UDUNITS broken format.
#'
#' @returns A data frame with constituent elements of the parsed timestamps in
#'   numeric format. The columns are year, month, day, hour, minute, second
#'   (with an optional fraction), time zone (character string), and the
#'   corresponding offset value from the datum. Invalid input data will appear
#'   as `NA` - if this is the case, a warning message will be displayed - other
#'   missing information on input will use default values.
#' @export
#' @examples
#' cf <- CFtime("days since 0001-01-01", "proleptic_gregorian")
#'
#' # This will have `NA`s on output and generate a warning
#' timestamps <- c("2012-01-01T12:21:34Z", "12-1-23", "today",
#'                 "2022-08-16T11:07:34.45-10", "2022-08-16 10.5+04")
#' CFparse(cf, timestamps)
CFparse <- function(cf, x) {
  stopifnot(is.character(x), methods::is(cf, "CFtime"))
  if (cf@datum@unit > 4) stop("Parsing of timestamps on a \"month\" or \"year\" datum is not supported.")

  out <- .parse_timestamp(cf@datum, x)
  if (anyNA(out$year))
    warning("Some dates could not be parsed. Result contains `NA` values.")
  if (length(unique(out$tz)) > 1)
    warning("Timestamps have multiple time zones. Some or all may be different from the datum time zone.")
  else if (out$tz[1] != timezone(cf))
    warning("Timestamps have time zone that is different from the datum.")
  return(out)
}

#' Parsing a vector of date-time strings, using a CFtime specification
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param datum CFdatum. The `CFdatum` instance that is the datum for the dates.
#' @param d character. A vector of strings of dates and times.
#'
#' @returns A data frame with columns year, month, day, hour, minute, second,
#' time zone, and offset. Invalid input data will appear as `NA`.
#' @noRd
.parse_timestamp  <- function(datum, d) {
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
    "(?:\\.([0-9]*))?",
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
  d <- gsub("UTC$", "", d)

  cap <- utils::strcapture(iso8601, d, parse)
  missing <- which(is.na(cap$year))
  if (length(missing) > 0)
    cap[missing,] <- utils::strcapture(broken, d[missing], parse)

  # Assign any fraction to the appropriate time part
  cap$frac[is.na(cap$frac)] <- "0"
  frac <- as.numeric(paste0("0.", cap$frac))
  if (sum(frac) > 0) {
    ndx <- which(!(is.na(cap$second)) & frac > 0)
    if (length(ndx) > 0) cap$second[ndx] <- cap$second[ndx] + frac[ndx]
    ndx <- which(!(is.na(cap$minute)) & is.na(cap$second) & frac > 0)
    if (length(ndx) > 0) cap$second[ndx] <- 60 * frac[ndx]
    ndx <- which(!(is.na(cap$hour)) & is.na(cap$minute) & frac > 0)
    if (length(ndx) > 0) {
      secs <- 3600 * frac
      cap$minute[ndx] <- secs[ndx] %/% 60
      cap$second[ndx] <- secs[ndx] %% 60
    }
  }
  cap$frac <- NULL

  # Convert NA time parts to 0 - in CF default time is 00:00:00 when not specified
  cap$hour[is.na(cap$hour)] <- 0
  cap$minute[is.na(cap$minute)] <- 0
  cap$second[is.na(cap$second)] <- 0

  # Set timezone to default value where needed
  ndx <- which(cap$tz_sign == "Z")
  if (length(ndx) > 0) {
    cap$tz_sign[ndx] <- "+"
    cap$tz_hour[ndx] <- "00"
    cap$tz_min[ndx] <- "00"
  }
  cap$tz <- paste0(ifelse(cap$tz_sign == "", "+", cap$tz_sign),
                   ifelse(cap$tz_hour == "", "00", cap$tz_hour),
                   ifelse(cap$tz_min == "", "00", cap$tz_min))
  cap$tz_sign <- cap$tz_hour <- cap$tz_min <- NULL

  # Set optional date parts to 1 if not specified
  cap$month[is.na(cap$month)] <- 1
  cap$day[is.na(cap$day)] <- 1

  # Check date validity
  invalid <- mapply(function(y, m, d) {!.is_valid_calendar_date(y, m, d, datum@cal_id)},
                    cap$year, cap$month, cap$day)
  if (nrow(datum@origin) > 0) {
    earlier <- mapply(function(y, m, d, dy, dm, dd) {
      if (is.na(y)) return(TRUE)
      if (y < dy) return(TRUE)
      if (y == dy){
        if (m < dm) return(TRUE)
        if (m == dm && d < dd) return(TRUE)
      }
      return(FALSE)
    }, cap$year, cap$month, cap$day, datum@origin[1, 1], datum@origin[1, 2], datum@origin[1, 3])
    invalid <- invalid | earlier
  }
  if (sum(invalid) > 0) cap[invalid,] <- rep(NA, 7)

  # Calculate offsets
  if (nrow(datum@origin) == 0) {        # if there's no datum yet, don't calculate offsets
    cap$offset <- rep(0, nrow(cap))     # this happens, f.i., when a CFdatum is created
  } else {
    days <- switch(datum@cal_id,
                   .date2offset_standard(cap, datum@origin),
                   .date2offset_julian(cap, datum@origin),
                   .date2offset_360day(cap, datum@origin),
                   .date2offset_365day(cap, datum@origin),
                   .date2offset_366day(cap, datum@origin)
                  )
    cap$offset <- round((days * 86400 + (cap$hour - datum@origin$hour[1]) * 3600 +
                         (cap$minute - datum@origin$minute[1]) * 60 +
                         cap$second - datum@origin$second) / CFt$units$seconds[datum@unit], 3)
  }
  return(cap)
}

#' Calculate difference in days between a data.frame of time parts and a datum
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param x data.frame. Dates to calculate the difference for.
#' @param origin data.frame. The origin to calculate the difference against.
#'
#' @returns Vector of days between `x` and the `origin`, using the `standard` calendar.
#' @noRd
.date2offset_standard <- function(x, origin) {
  yd0 <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334) # days diff of 1st of month to 1 January in normal year

  datum_year <- origin[1, 1]
  datum_days_in_year <- yd0[origin[1, 2]] + origin[1, 3]
  if ((origin[1, 2] <= 2) && ((datum_year %% 4 == 0 && datum_year %% 100 > 0) || datum_year %% 400 == 0))
    datum_days_in_year <- datum_days_in_year - 1

  mapply(function(y, m, d) {
    if (is.na(y)) return(NA_integer_)
    if (m <= 2 && ((y %% 4 == 0 && y %% 100 > 0) || y %% 400 == 0)) days <- -1 else days <- 0 # -1 if in a leap year up to the leap day, 0 otherwise
    repeat {
      if (y > datum_year) {
        days <- days + 365 + as.integer((y %% 4 == 0 && y %% 100 > 0) || y %% 400 == 0)
        y <- y - 1
      } else break
    }
    days + yd0[m] + d - datum_days_in_year
  }, x$year, x$month, x$day)
}

#' Calculate difference in days between a data.frame of time parts and a datum
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param x data.frame. Dates to calculate the difference for.
#' @param origin data.frame. The origin to calculate the difference against.
#'
#' @returns Vector of days between `x` and the `origin`, using the `julian` calendar.
#' @noRd
.date2offset_julian <- function(x, origin) {
  yd0 <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334) # days diff of 1st of month to 1 January in normal year

  datum_year <- origin[1, 1]
  datum_days_in_year <- yd0[origin[1, 2]] + origin[1, 3]
  if (origin[1, 2] <= 2 && datum_year %% 4 == 0)
    datum_days_in_year <- datum_days_in_year - 1

  mapply(function(y, m, d) {
    if (is.na(y)) return(NA_integer_)
    if (m <= 2 && y %% 4 == 0) days <- -1 else days <- 0 # -1 if in a leap year up to the leap day, 0 otherwise
    repeat {
      if (y > datum_year) {
        days <- days + 365 + as.integer(y %% 4 == 0)
        y <- y - 1
      } else break
    }
    days + yd0[m] + d - datum_days_in_year
  }, x$year, x$month, x$day)
}

#' Calculate difference in days between a data.frame of time parts and a datum
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param x data.frame. Dates to calculate the difference for.
#' @param origin data.frame. The origin to calculate the difference against.
#'
#' @returns Vector of days between `x` and the `origin`, using the `360_day` calendar.
#' @noRd
.date2offset_360day <- function(x, origin) {
  (x$year - origin[1, 1]) * 360 + (x$month - origin[1, 2]) * 30 + x$day - origin[1, 3]
}

#' Calculate difference in days between a data.frame of time parts and a datum
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param x data.frame. Dates to calculate the difference for.
#' @param origin data.frame. The origin to calculate the difference against.
#'
#' @returns Vector of days between `x` and the `origin`, using the `365_day` calendar.
#' @noRd
.date2offset_365day <- function(x, origin) {
  yd0 <- c(0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334) # days diff of 1st of month to 1 January
  (x$year - origin[1, 1]) * 365 + yd0[x$month] - yd0[origin[1, 2]] + x$day - origin[1, 3]
}

#' Calculate difference in days between a data.frame of time parts and a datum
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param x data.frame. Dates to calculate the difference for.
#' @param origin data.frame. The origin to calculate the difference against.
#'
#' @returns Vector of days between `x` and the `origin`, using the `366_day` calendar.
#' @noRd
.date2offset_366day <- function(x, origin) {
  yd0 <- c(0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335) # days diff of 1st of month to 1 January
  (x$year - origin[1, 1]) * 366 + yd0[x$month] - yd0[origin[1, 2]] + x$day - origin[1, 3]
}
