#' Return the number of days in a month given a certain CF calendar
#'
#' @description Given a vector of dates as strings in ISO 8601 format and a
#' CFdatum object, this function will return a vector of the same length as the
#' dates, indicating the number of days in the month according to the CFdatum
#' specification.
#'
#' @param x A vector of dates as strings with format `YYYY-MM-DD`.
#' @param datum The CFdatum specification to use.
#'
#' @return A vector indicating the number of days in each month for the vector of
#' dates supplied as a parameter to the function
#' @export
#' @examples
#' dates <- c("2021-11-27", "2021-12-10", "2022-01-14", "2022-02-18")
#' datum <- CFdatum("days since 1850-01-01", "standard")
#' month_days(dates, datum)
#'
#' datum <- CFdatum("days since 1850-01-01", "all_leap")
#' month_days(dates, datum)
#'
#' datum <- CFdatum("days since 1850-01-01", "360_day")
#' month_days(dates, datum)
#'
month_days <- function(x, datum) {
  cal_id <- datum@cal_id
  if (cal_id == 3) days <- rep(30, length(x)) # 360_day
  else {
    days_in_month <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
    if (cal_id == 5) days_in_month[2] <- 29

    months <- as.integer(substr(x, 6, 7))
    if (cal_id %in% c(4, 5)) { # 365_day or 366_day
      dim(months) <- length(months)
      days <- apply(months, 1, function(m) {days_in_month[m]})
    } else {                     # standard and julian: account for leap years
      years <- as.integer(substr(x, 1, 4))
      days <- mapply(function(m, y) {
        if (m == 2) {
          if (y %% 4 > 0) return(28)                     # not divisible by 4, so always a normal year
          else if (calendar == 2) return(29)             # divisible by 4, so on Julian calendar always 29
          if (y %% 100 == 0 && y %% 400 > 0) return(28)  # divisible by 100 but not 400, so a normal year
          else return(29)                                # all other years are leap years
        }
        else return(days_in_month[m])
      }, months, years)
    }
  }
  return(days)
}
