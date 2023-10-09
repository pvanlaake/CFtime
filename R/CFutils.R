#' Return the number of days in a month given a certain CF calendar
#'
#' Given a vector of dates as strings in ISO 8601 or UDUNITS format and a `CFtime` object,
#' this function will return a vector of the same length as the dates,
#' indicating the number of days in the month according to the calendar
#' specification. If no vector of days is supplied, the function will return an
#' integer vector of length 12 with the number of days for each month of the
#' calendar (disregarding the leap day for `standard` and `julian` calendars).
#'
#' @param cf CFtime. The CFtime definition to use.
#' @param x character. An optional vector of dates as strings with format
#'  `YYYY-MM-DD`. Any time part will be silently ingested.
#'
#' @returns A vector indicating the number of days in each month for the vector
#'   of dates supplied as a parameter to the function. If no dates are supplied,
#'   the number of days per month for the calendar as a vector of length 12.
#'   Invalidly specified dates will result in an `NA` value.
#' @export
#' @examples
#' dates <- c("2021-11-27", "2021-12-10", "2022-01-14", "2022-02-18")
#' cf <- CFtime("days since 1850-01-01", "standard")
#' CFmonth_days(cf, dates)
#'
#' cf <- CFtime("days since 1850-01-01", "360_day")
#' CFmonth_days(cf, dates)
#'
#' cf <- CFtime("days since 1850-01-01", "all_leap")
#' CFmonth_days(cf, dates)
#'
#' CFmonth_days(cf)
CFmonth_days <- function(cf, x = NULL) {
  stopifnot(methods::is(cf, "CFtime"))

  days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leapdays_in_month <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # No dates supplied: return standard number of days per month
  if (is.null(x)) {
    if (cf@datum@cal_id %in% c(1, 2, 4)) return(days_in_month)
    if (cf@datum@cal_id == 3) return(rep(30, 12))
    return(leapdays_in_month)
  }

  # Argument x supplied
  if (!(is.character(x))) stop("Argument `x` must be a character vector of dates in 'YYYY-MM-DD' format")

  ymd <- .parse_timestamp(cf@datum, x)

  if (cf@datum@cal_id == 3) {     # 360_day
    res <- rep(30, length(x))
    res[which(is.na(ymd$year))] <- NA
    return(res)
  }

  if (cf@datum@cal_id == 4) return(days_in_month[ymd$month])
  if (cf@datum@cal_id == 5) return(leapdays_in_month[ymd$month])

  # Standard and julian calendars
  ifelse(.is_leap_year(ymd$year, cf@datum@cal_id), leapdays_in_month[ymd$month], days_in_month[ymd$month])
}

#' Check if the supplied year, month and day form a valid date in the specified
#' calendar.
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param yr numeric. The year to test, must be in range 1:9999.
#' @param mon numeric. The month to test, must be in range 1:12
#' @param day numeric. The day to test, must be in the range permitted by the calendar.
#' @param cal_id numeric. Identifier of the calendar to use to test the validity of the date.
#'
#' @returns boolean. TRUE if the date is valid, FALSE otherwise.
#' @noRd
.is_valid_calendar_date <- function(yr, mon, day, cal_id) {
  if (is.na(yr) || is.na(mon)) return(FALSE)

  # Check valid date ranges, no extended syntax
  if ((yr < 1) || (yr > 9999)) return(FALSE)    # year out of range
  if ((mon < 1) || (mon > 12)) return(FALSE)    # month out of range
  if (is.na(day)) return(TRUE)                  # day not specified
  if ((day >= 1) && (day <= 28)) return(TRUE)   # day in safe range, 90% of valid cases
  else if ((day < 1) || day > 31) return(FALSE) # day out of range

  # 360_day calendar: oddball case for month length
  if (cal_id == 3) return(day <= 30)

  # Now all dates should be in regular-length months, but check for leap years
  # Day is in range 29:31 because day in range 1:28 already passed
  if (mon == 2) { # February
    if (day > 29) return(FALSE)
    if (cal_id == 5) return(TRUE)         # all_leap
    if (cal_id == 4) return(FALSE)        # no_leap
    if (cal_id == 2) return(yr %% 4 == 0) # julian: every 4th year is a leap year
    return(((yr %% 4 == 0) && (yr %% 100 > 0)) || (yr %% 400 == 0)) # standard calendar
  }
  return(!((mon %in% c(4, 6, 9, 11)) && (day == 31))) # months other than February
}

#' Flag which years are leap years, given a certain CF calendar
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param yr numeric. Vector of years to test.
#' @param cal integer. The id of the calendar.
#'
#' @returns A logical vector of the same length as argument `yr` which is `TRUE`
#' for elements that are leap years for the given calendar, `FALSE` otherwise.
#' @noRd
.is_leap_year <- function(yr, cal) {
  switch (cal,
          ((yr %% 4 == 0) & (yr %% 100 > 0)) | (yr %% 400 == 0),
          yr %% 4 == 0,
          rep(FALSE, length(yr)),
          rep(FALSE, length(yr)),
          rep(TRUE, length(yr)))
}
