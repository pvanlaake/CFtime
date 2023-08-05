#' CFdatum class
#'
#' This internal class stores the information to represent date and time values using
#' the CF conventions. This class is not supposed to be used by end-users directly.
#' An instance is created by the exported `CFtime` class, which also exposes the
#' relevant properties of this class.
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
#' @slot origin data.frame. Data frame with 1 row that defines the origin time.
#' @slot calendar character. The CF-calendar for the instance.
#' @slot cal_id numeric. The internal identifier of the CF-calendar to use.
#'
#' @returns An object of class CFdatum
#' @noRd
setClass("CFdatum",
    slots = c(
      definition     = "character",
      unit           = "numeric",
      origin         = "data.frame",
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
#' @returns An object of the `CFdatum` class.
#' @noRd
CFdatum <- function(definition, calendar) {
  stopifnot(length(definition) ==  1, length(calendar) == 1)
  definition <- tolower(definition)
  calendar <- tolower(calendar)

  parts <- strsplit(definition, " ")[[1]]
  if ((length(parts) < 3) || !(parts[2] %in% c("since", "after", "from", "ref", "per")))
    stop("Definition string does not appear to be a CF-compliant time coordinate description")
  u <- which(CFtime_units$unit == parts[1])
  if (length(u) == 0) stop("Unsupported unit: ", parts[1])

  cal <- CFtime_cal_ids[which(calendar == CFtime_calendars)]
  if (length(cal) == 0) stop("Invalid calendar specification")

  nw <- methods::new("CFdatum", definition = definition, unit = CFtime_units$unit_id[u], origin = data.frame(), calendar = calendar, cal_id = cal)

  dt <- .parse_timestamp(nw, paste(parts[3:length(parts)], collapse = " "))
  if (is.na(dt$year[1])) stop("Definition string does not appear to be a CF-compliant time coordinate description: invalid base date specification")
  nw@origin <- dt

  return(nw)
}

setMethod("show", "CFdatum", function(object) {
  if (object@origin$tz[1] == "00:00") tz = "" else tz = object@origin$tz[1]
  cat("CF datum of origin:",
      "\n  Origin  : ", origin_date(object), " ", origin_time(object), tz,
      "\n  Units   : ", CFtime_unit_string[object@unit],
      "\n  Calendar: ", object@calendar, "\n",
      sep = "")
})

#' Equivalence of CFdatum objects
#'
#' This function can be used to test if two CFdatum objects represent the same datum
#' for CF-convention time coordinates. Two CFdatum objects are considered equivalent
#' if they have the same definition string and the same calendar. Calendars
#' "standard", "gregorian" and "proleptic_gregorian" are considered equivalent,
#' as are the pairs of "365_day" and "no_leap", and "366_day" and "all_leap".
#'
#' @param e1,e2 CFdatum Instances of the CFdatum class.
#'
#' @returns `TRUE` if the CFdatum objects are equivalent, `FALSE` otherwise.
#' @noRd
.datum_equivalent <- function(e1, e2) {
  sum(e1@origin[1,1:6] != e2@origin[1,1:6]) == 0 &&  # Offset column is NA
  e1@unit == e2@unit &&
  e1@cal_id == e2@cal_id
}

definition <- function(x) x@definition

calendar <- function(x) x@calendar

unit <- function(x) x@unit

origin_date <- function(x) sprintf("%04d-%02d-%02d", x@origin$year[1], x@origin$month[1], x@origin$day[1])

origin_time <- function(x) .format_time(x@origin)
