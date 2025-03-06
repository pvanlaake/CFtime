#' @name deprecated_functions
#' @title Deprecated functions
#'
#' @description
#' These functions are deprecated and should no longer be used in new code. The
#' below table gives the replacement function to use instead. The function
#' arguments of the replacement function are the same as those of the deprecated
#' function if no arguments are given in the table.
#'
#' | **Deprecated function** | **Replacement function** |
#' | ------------------- | -------------------- |
#' | CFcomplete()        | [is_complete()]      |
#' | CFmonth_days()      | [month_days()]       |
#' | CFparse()           | [parse_timestamps()] |
#' | CFrange()           | [range()]            |
#' | CFsubset()          | [slice()]             |
#' | CFtimestamp()       | [as_timestamp()]     |
#'
#' @param t,x,format,asPOSIX,extremes See replacement functions.
#'
#' @returns See replacement functions.

# nocov start

#' @rdname deprecated_functions
#' @export
CFtimestamp <- function(t, format = NULL, asPOSIX = FALSE) {
  warning("Function `CFtimestamp()` is deprecated. Use `as_timestamp()` instead.")
  as_timestamp(t, format, asPOSIX)
}

#' @rdname deprecated_functions
#' @export
CFmonth_days <- function(t, x = NULL) {
  warning("Function `CFmonth_days()` is deprecated. Use `month_days()` instead.")
  month_days(t, x)
}

#' @rdname deprecated_functions
#' @export
CFcomplete <- function(x) {
  warning("Function `CFcomplete()` is deprecated. Use `is_complete()` instead.")
  is_complete(x)
}

#' @rdname deprecated_functions
#' @export
CFsubset <- function(x, extremes) {
  warning("Function `CFsubset()` is deprecated. Use `slab()` instead.")
  slab(x, extremes)
}

#' @rdname deprecated_functions
#' @export
CFparse <- function(t, x) {
  warning("Function `CFparse()` is deprecated. Use `parse_timestamps()` instead.")
  parse_timestamps(t, x)
}

# nocov end
