#' Parse series of timestamps in CF format to date-time elements
#'
#' This function will parse a vector of timestamps in ISO8601 or UDUNITS format
#' into a tibble with columns for the elements of the timestamp: year, month,
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
#' are supported. The vector of timestamps may have any combination of ISO8601 and UDUNITS formats.
#'
#' @param x character. Vector of character string representing timestamps in
#' ISO8601 extended or UDUNITS broken format.
#' @param calendar character. Atomic character string indicating the CF calendar
#' to use in interpreting the validity of the date.
#'
#' @return A tibble with constituent elements of the parsed timestamps in
#' numeric format. The columns are year, month, day, hour, minute, second (with
#' an optional fraction), tz_hour and tz_minute.
#' Missing and invalid input data will appear as `NA` - if the year is `NA` then
#' the timestamp in its entirety is invalid, otherwise `NA` values specify
#' missing information for which defaults exist.
#' @export
#' @examples
#' timestamps <- c("2012-01-01T12:21:34Z", "12-1-23", "today",
#'                 "2022-08-16T11:07:34.45-10", "2022-08-16 10.5+04")
#' x <- parse_timestamp(timestamps, "proleptic_gregorian")
parse_timestamp <- function(x, calendar = "standard") {
  # Check parameter sanity
  stopifnot(is.character(x), length(calendar) == 1)
  cal <- CFt_cal_ids[which(calendar == CFt_calendars)]
  if (length(cal) == 0) stop("invalid calendar specification")

  # Parse and convert to tibble
  ts <- t(.parse_timestamp(x, cal))
  hr <- ts[, 4] %/% 3600
  min <- (ts[, 4] %% 3600) %/% 60
  sec <- ts[, 4] %% 60
  ts <- cbind(ts[, 1:3], hr, min, sec, ts[, 5:6])
  colnames(ts) <- c("year", "month", "day", "hour", "minute", "second", "tz_hour", "tz_minute")
  return(tibble::as_tibble(ts))
}

#' Parsing a vector of date-time strings, using a calendar
#'
#' This is an internal function that should not generally be used outside of
#' the CFtime package.
#'
#' @param d character. A vector of strings of dates and times.
#' @param cal_id numeric. Identifier of the calendar to use.
#'
#' @returns A matrix with as many columns as the input vector and six rows with
#' year, month, day, second, time zone offset hours and minutes. Invalid input
#' data will appear as `NA`. There are no hours and minutes in the result as
#' these can very easily be derived from the seconds value, which thus has a
#' range of `[0 .. 86400>`.
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
    "([+-]?[01]?[0-9]|2[0-3])?",            # hour, with optional sign
    "(?::([0-5]?[0-9]))?",                  # minute
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
    "(Z|[+-][01][0-9]|2[0-3])?",
    "(?::([0-5][0-9]))?",
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

  len <- length(d)
  parts <- unlist(stringi::stri_match_all_regex(d, iso8601))
  parts[parts == "Z"] <- "0"
  dim(parts) <- c(10, len)
  missing <- which(is.na(parts[1, ]))
  if (length(missing) > 0) {
    udunits <- unlist(stringi::stri_match_all_regex(d[missing], broken))
    dim(udunits) <- c(10, length(missing))
    for (i in 1:length(missing))
      parts[, missing[i]] <- udunits[, i]
  }

  # Convert to numeric
  parts <- parts[-1, ]      # drop the captured string
  parts <- as.numeric(parts)
  dim(parts) <- c(9, len)

  # Convert the time values into seconds, including any fractional part
  secs <- apply(parts, 2, function(p) {
    if (is.na(p[4])) return(0)    # in CF 1.9, default time is 00:00:00 when not specified
    if (is.na(p[7])) frac <- 0
    else frac <- (as.numeric(paste0("0.", p[7])))
    if (is.na(p[5])) return((p[4] + frac) * 3600)
    if (is.na(p[6])) return(p[4] * 3600 + (p[5] + frac) * 60)
    return(p[4] * 3600 + p[5] * 60 + p[6] + frac)
  })

  # Drop useless rows, splice seconds back in
  parts <- parts[-(5:7), ]
  dim(parts) <- c(6, len)
  parts[4, ] <- secs

  # Set timezone values to default value if not specified
  parts[5, is.na(parts[5, ])] <- 0
  parts[6, is.na(parts[6, ])] <- 0

  # Check date validity
  notok <- apply(parts, 2, function(dt) !.is_valid_calendar_date(dt[1], dt[2], dt[3], cal_id))
  if (sum(notok) > 0)
    parts[, notok] <- rep(NA, 6)

  return(parts)
}
