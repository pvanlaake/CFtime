#' CFdatum class
#'
#' This class stores the information to represent date and time values using
#' the CF conventions.
#'
#' The following calendars are supported:
#'
#' \itemize{
#'   \item `gregorian` or `standard`, the international standard calendar for civil use.
#'   \item `proleptic_gregorian`, the standard calendar but extending before 1582-10-15
#'   when the Gregorian calendar was adopted.
#'   \item `noleap` or `365_day`, all years have 365 days.
#'   \item `all_leap` or `366_day`, all years have 366 days.
#'   \item `360_day`, all years have 360 days, divided over 12 months of 30 days.
#'   \item `julian`, every fourth year is a leap year (so including the years 1700, 1800, 1900, 2100, etc).
#' }
#'
#' @slot definition character. The string that defines the time unit and base date/time.
#' @slot unit numeric. The unit of time in which offsets are expressed.
#' @slot origin numeric. Vector of components year, month, day and seconds.
#' @slot tz character. Time zone offset hour and minute.
#' @slot calendar character. The CF-calendar for the instance.
#' @slot cal_id numeric. The internal identifier of the CF-calendar to use.
#'
#' @return An object of class CFdatum
#' @export
setClass("CFdatum",
    slots = c(
      definition     = "character",
      unit           = "numeric",
      origin         = "numeric",
      tz             = "character",
      calendar       = "character",
      cal_id         = "numeric"
    ))

#' Create a CFdatum object
#'
#' This function creates an instance of the `CFdatum` class. After creation the
#' instance is read-only. The parameters to the call are typically read from a
#' CF-compliant data file with climatological observations or predictions.
#'
#' @param definition character. An atomic string describing the time coordinate
#' of a CF-compliant data file.
#' @param calendar character. An atomic string describing the calendar to use
#' with the time dimension definition string.
#'
#' @return An object of the `CFdatum` class.
#' @export
#'
#' @examples
#' cf <- CFdatum("days since 1850-01-01", "julian")
CFdatum <- function(definition, calendar = "standard") {
  stopifnot(length(definition) ==  1, length(calendar) == 1)
  parts <- strsplit(definition, " ")[[1]]
  num <- length(parts)
  if ((num < 3) || (!(stringi::stri_trans_tolower(parts[2]) %in% c("since", "after", "from", "ref", "per"))))
    stop("definition string does not appear to be a CF-compliant time coordinate description")
  u <- dplyr::filter(CFt_units, unit == stringi::stri_trans_tolower(parts[1]))
  if (length(u) == 0) stop("unsupported unit: ", parts[1])

  cal <- CFt_cal_ids[which(calendar == CFt_calendars)]
  if (length(cal) == 0) stop("invalid calendar specification")

  dt <- .parse_timestamp(stringi::stri_sub(definition, sum(stringi::stri_length(parts[1:2])) + 3), cal)[, 1]
  if (is.na(dt[1])) stop("definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification")
  tz <- sprintf("%+03d:%02d", dt[5], dt[6])

  methods::new("CFdatum", definition = definition, unit = u$unit_id, origin = dt[1:4], tz = tz, calendar = calendar, cal_id = cal)
}

setMethod("show", "CFdatum", function(object) {
  if (object@tz == "+00:00") tz = "" else tz = object@tz
  cat("CF datum of origin:",
      "\n  Origin  : ", origin_date(object), " ", origin_time(object), tz,
      "\n  Units   : ", CFt_unit_string[object@unit],
      "\n  Calendar: ", object@calendar, "\n",
      sep = "")
})

setGeneric("definition", function(x) standardGeneric("definition"))

setMethod("definition", "CFdatum", function(x) x@definition)

setGeneric("calendar", function(x) standardGeneric("calendar"))

setMethod("calendar", "CFdatum", function(x) x@calendar)

setGeneric("unit", function(x) standardGeneric("unit"))

setMethod("unit", "CFdatum", function(x) x@unit)

setGeneric("origin_date", function(x) standardGeneric("origin_date"))

setMethod("origin_date", "CFdatum", function(x) sprintf("%04d-%02d-%02d", x@origin[1], x@origin[2], x@origin[3]))

setGeneric("origin_time", function(x) standardGeneric("origin_time"))

setMethod("origin_time", "CFdatum", function(x) .format_time(x@origin[4]))

setGeneric("year", function(x) standardGeneric("year"))

setMethod("year", "CFdatum", function(x) x@origin[1])

setGeneric("month", function(x) standardGeneric("month"))

setMethod("month", "CFdatum", function(x) x@origin[2])

setGeneric("day", function(x) standardGeneric("day"))

setMethod("day", "CFdatum", function(x) x@origin[3])

setGeneric("second", function(x) standardGeneric("second"))

setMethod("second", "CFdatum", function(x) x@origin[3])

#' Equivalence of CFdatum objects
#'
#' This operator can be used to test if two CFdatum objects represent the same datum
#' for CF-convention time coordinates. Two CFdatum objects are considered equivalent
#' if they have the same definition string and the same calendar. Calendars
#' "standard", "gregorian" and "proleptic_gregorian" are considered equivalent,
#' as are the pairs of "365_day" and "no_leap", and "366_day" and "all_leap".
#'
#' @param e1,e2 CFdatum Instances of the CFdatum class.
#'
#' @return `TRUE` if the CFdatum objects are equivalent, `FALSE` otherwise.
#' @export
#'
#' @examples
#' e1 <- CFdatum("days since 1850-01-01", "gregorian")
#' e2 <- CFdatum("days since 1850-01-01 00:00:00", "standard")
#' e1 == e2
setMethod("==", c("CFdatum", "CFdatum"), function(e1, e2) sum(e1@origin != e2@origin) == 0 &&
                                                          e1@unit == e2@unit &&
                                                          e1@cal_id == e2@cal_id)

