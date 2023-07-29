#' Parse series of timestamps in CF format to date-time elements
#'
#' This function will parse a vector of timestamps in ISO8601 or UDUNITS format
#' into a data frame with columns for the elements of the timestamp: year, month,
#' day, hour, minute, second, time zone hour and minute. Those timestamps that
#' could not be parsed or which represent an invalid date in the indicated
#' calendar will have `NA` values for the elements of the offending timestamp.
#'
#' The supported formats are the *broken timestamp* format from the UDUNITS
#' library and ISO8601 *extended*, both with minor changes, as suggested by the
#' CF Metadata Conventions 1.9. In general, the format is `YYYY-MM-DD hh:mm:ss.sss hh:mm`.
#' The year can be from 1 to 4 digits and is interpreted literally, so `79-10-24`
#' is the day Mount Vesuvius erupted and destroyed Pompeii, not `1979-10-24`
#' (when [Ajax Amsterdam beat Omonoia Cyprus 10-0 in the first leg of the second
#' round of the European Cup competition, with 5 goals from Søren Lerby](https://www.uefa.com/uefachampionsleague/match/63624--ajax-vs-omonia/),
#' as everybody vividly remembers). The year and month are mandatory,
#' all other fields are optional. There are defaults for all missing values,
#' following the UDUNITS and CF Metadata Conventions. Leading zeros can be omitted in the
#' UDUNITS format. The optional fractional part can have as many digits as the
#' precision calls for and will be applied to the smallest specified time unit.
#' In the result of this function if the fraction is associated with the minute
#' or the hour, it is converted into a regular `hh:mm:ss.sss` format, i.e. any
#' fraction in the result is always associated with the second.
#' The time zone is optional and should have at least the hour or `Z` if present,
#' the minute is optional. The time zone hour can have an optional sign. The
#' separator between the date and the time can be a single whitespace character
#' or a `T`; in the UDUNITS format the separator between the time and the time
#' zone must be a single whitespace character.
#'
#' Currently only the extended formats (with separators between the elements)
#' are supported. The vector of timestamps may have any combination of ISO8601
#' and UDUNITS formats.
#'
#' @param x character. Vector of character string representing timestamps in
#' ISO8601 extended or UDUNITS broken format.
#' @param calendar character. Atomic character string indicating the CF calendar
#' to use in interpreting the validity of the date.
#'
#' @return A data frame with constituent elements of the parsed timestamps in
#' numeric format. The columns are year, month, day, hour, minute, second (with
#' an optional fraction) and time zone. Invalid input data will appear as `NA`,
#' other missing information on input will use default values.
#' @export
#' @examples
#' timestamps <- c("2012-01-01T12:21:34Z", "12-1-23", "today",
#'                 "2022-08-16T11:07:34.45-10", "2022-08-16 10.5+04")
#' x <- CF_parse_timestamp(timestamps, "proleptic_gregorian")
CF_parse_timestamp <- function(x, calendar = "standard") {
  # Check parameter sanity
  stopifnot(is.character(x), length(calendar) == 1)
  calendar <- tolower(calendar)
  cal <- CFt_cal_ids[which(calendar == CFt_calendars)]
  if (length(cal) == 0) stop("Invalid calendar specification")

  .parse_timestamp(x, cal)
}

#' Parsing a vector of date-time strings, using a calendar
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param d character. A vector of strings of dates and times.
#' @param cal_id numeric. Identifier of the calendar to use.
#'
#' @returns A data frame with columns year, month, day, hour, minute, second,
#' time zone offset hours and minutes. Invalid input data will appear as `NA`.
#' @noRd
.parse_timestamp  <- function(d, cal_id) {
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
    "(?:[T,\\s]",                           # if a time is following, separate with a single whitespace character or a "T"
    "([01]?[0-9]|2[0-3])?",                 # hour
    ":([0-5]?[0-9])?",                      # minute
    "(?::([0-5]?[0-9]))?",                  # second, optional
    "(?:\\.([0-9]*))?",                     # optional fractional part of the smallest specified unit
    ")?",                                   # close optional time capture group
    "(?:\\s",                               # if a time zone offset is following, separate with a single whitespace character
    "([+-]?[01]?[0-9]|2[0-3](?::[0-5]?[0-9]))?",            # hour, with optional sign
#    "(?::([0-5]?[0-9]))?",                  # minute
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
    "(?:",
    "(Z|[+-][01][0-9]|2[0-3](?::[0-5][0-9]))?",
#    "(?::([0-5][0-9]))?",
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
  #     "([0-5][0-9])?",                      # minute
  #   ")?",                                   # close optional timezone capture group
  #   "$"                                     # anchor string at end
  # )

  parts <- data.frame(year = numeric(), month = numeric(), day = numeric(),
                      hour = numeric(), minute = numeric(), second = numeric(), frac = character(),
                      tz = character())

  cap <- utils::strcapture(iso8601, d, parts)
  missing <- which(is.na(cap$year))
  if (length(missing) > 0)
    cap[missing,] <- utils::strcapture(broken, d[missing], parts)

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
  cap$tz[is.na(cap$tz) | (cap$tz %in% c("", "Z"))] <- "00:00"

  # Set optional date parts to 1 if not specified
  cap$month[is.na(cap$month)] <- 1
  cap$day[is.na(cap$day)] <- 1

  # Check date validity
  notok <- mapply(function(y, m, d) {!.is_valid_calendar_date(y, m, d, cal_id)},
                  cap$year, cap$month, cap$day)
  if (sum(notok) > 0) cap[notok,] <- rep(NA, 7)

  return(cap)
}
