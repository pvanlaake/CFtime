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

#' Return the extremes of the time series as character strings
#'
#' This function returns the first and last timestamp of the time series as a vector. Note that the time series is not
#' necessarily sorted.
#'
#' This is an internal function that should not be used outside of the CFtime package.
#'
#' @param x CFts. The time series to operate on
#'
#' @returns Vector of two character strings that represent the
#' starting and ending timestamps in the time series. If all of the timestamps
#' in the time series have a time component of `00:00:00` the date of the
#' timestamp is returned, otherwise the full timestamp (without any time zone
#' information).
#'
#' @noRd
.ts_range <- function(x) {
  mn <- which.min(x@offsets)
  mx <- which.max(x@offsets)
  ts <- x@ymds
  if (all(ts[ ,4] == 0)) {
    return(c(sprintf("%04d-%02d-%02d", ts[mn, 1], ts[mn, 2], ts[mn, 3]),
             sprintf("%04d-%02d-%02d", ts[mx, 1], ts[mx, 2], ts[mx, 3])))
  } else {
    t <- .format_time(c(ts[mn, 4], ts[mx, 4]))
    return(c(sprintf("%04d-%02d-%02dT%s", ts[mn, 1], ts[mn, 2], ts[mn, 3], t[1]),
             sprintf("%04d-%02d-%02dT%s", ts[mx, 1], ts[mx, 2], ts[mx, 3], t[2])))
  }
}
