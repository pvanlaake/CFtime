#' @title International Atomic Time CF calendar
#'
#' @description This class represents a calendar based on the International
#'   Atomic Time. Validity is from 1958 onwards, with dates represented using
#'   the Gregorian calendar. Given that this "calendar" is based on a universal
#'   clock, the concepts of leap second, time zone and daylight savings time do
#'   not apply.
#'
#' @aliases CFCalendarTAI
#' @docType class
CFCalendarTAI <- R6::R6Class("CFCalendarTAI",
  inherit = CFCalendarProleptic,
  public = list(
    #' @description Indicate which of the supplied dates are valid.
    #' @param ymd `data.frame` with dates parsed into their parts in columns
    #'   `year`, `month` and `day`. If present, the `tz` column is checked for
    #'   illegal time zone offsets. Any other columns are disregarded.
    #' @return Logical vector with the same length as argument `ymd` has rows
    #'   with `TRUE` for valid days and `FALSE` for invalid days, or `NA` where
    #'   the row in argument `ymd` has `NA` values.
    valid_days = function(ymd) {
      super$valid_days(ymd) & ymd$year >= 1958L
      # FIXME: TZ offsets
    }
  )
)
