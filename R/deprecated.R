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
#' | CFrange()           | [range()]            |
#' | CFsubset()          | [slab()]             |
#' | CFtimestamp()       | [as_timestamp()]     |
#'
#' @param cf,x,format,asPOSIX See replacement functions.
#'
#' @returns See replacement functions.

#' @rdname deprecated_functions
#' @export
CFtimestamp <- function(cf, format = NULL, asPOSIX = FALSE) {
  warning("Function `CFtimestamp()` is deprecated. Use `as_timestamp()` instead.")
  as_timestamp(cf, format, asPOSIX)
}

#' @rdname deprecated_functions
#' @export
CFmonth_days <- function(cf, x = NULL) {
  warning("Function `CFmonth_days()` is deprecated. Use `month_days()` instead.")
  month_days(cf, x)
}

#' @rdname deprecated_functions
#' @export
CFcomplete <- function(x) {
  warning("Function `CFcomplete()` is deprecated. Use `is_complete()` instead.")
  is_complete(x)
}

CFsubset <- function(x, extremes) {
  warning("Function `CFsubset()` is deprecated. Use `slab()` instead.")
  slab(x, extremes)
}
