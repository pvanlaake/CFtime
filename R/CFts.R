#' CF-convention time series
#'
#' @slot origin CFdatum. The atomic origin upon which the `offsets` are based.
#' @slot offsets numeric. Vector of offsets in units and from a timestamp
#' @slot ymds matrix. Matrix of numeric date elements year, month, day and seconds from offsets.
#' defined by the `origin`.
#'
#' @return An object of class CFts.
#' @export
setClass("CFts",
         slots = c(
           origin  = "CFdatum",
           offsets = "numeric",
           ymds    = "matrix"
         ))

#' Create a CFts object
#'
#' This function creates an instance of the CFts class. After creation the
#' instance is read-only. The parameters to the call are typically read from a
#' CF-compliant data file with climatological observations or predictions.
#'
#' @param datum CFdatum. An atomic instance of the `CFdatum` class describing the date-
#' time system upon which to base the offsets.
#' @param offsets numeric. A vector of offsets from the origin in the `datum`
#' instance. The unit of measure is defined by the `datum` instance as well.
#'
#' @return An object of the CFts class.
#' @export
#'
#' @examples
#' datum <- CFdatum("days since 1850-01-01", "julian")
#' ts <- CFts(datum, 0:364)
CFts <- function(datum, offsets) {
  stopifnot(methods::is(datum, "CFdatum"))
  if (is.array(offsets)) dim(offsets) <- NULL
  #stopifnot(methods::is(offsets, "numeric"))

  ymds <- .add_offset(offsets, datum)

  methods::new("CFts", origin = datum, offsets = offsets, ymds = ymds)
}

setMethod("show", "CFts", function(object) {
  len <- length(object@offsets)
  ymds <- object@ymds
  cat("CF time series:\n",
      methods::show(object@origin),
      sprintf("  Elements: [%04d-%02d-%02d .. %04d-%02d-%02d] (average of %f %s between elements)\n",
              ymds[1, 1], ymds[1, 2], ymds[1, 3], ymds[len, 1], ymds[len, 2], ymds[len, 3], (object@offsets[len] - object@offsets[1]) / (len - 1), CFt_unit_string[object@origin@unit]),
      sep = "")
})

#' Equivalence of CFts objects
#'
#' This operator can be used to test if two CFts objects represent the same
#' CF-convention time coordinates. Two CFts objects are considered equivalent
#' if they have an equivalent datum and the same offsets.
#'
#' @param e1,e2 CFts. Instances of the CFts class.
#'
#' @return `TRUE` if the CFts objects are equivalent, `FALSE` otherwise.
#' @export
#'
#' @examples
#' e1 <- CFts(CFdatum("days since 1850-01-01", "gregorian"), 0:364)
#' e2 <- CFts(CFdatum("days since 1850-01-01 00:00:00", "standard"), 0:364)
#' e1 == e2
setMethod("==", c("CFts", "CFts"), function(e1, e2)
  e1@origin == e2@origin &&
  length(e1@offsets) == length(e2@offsets) &&
  all(e1@offsets == e2@offsets))

#' Merge two CFts objects
#'
#' Two CFts instances can be merged into one with this operator, provided that
#' the datum of the two instances is equivalent.
#'
#' The order of the two parameters is significant. The resulting CFts will have
#' the offsets of both instances in the order that they are specified. There is
#' no reordering or removal of duplicates. This is because the time series are
#' usually associated with a dataset and the correspondence between the two is
#' thus preserved.
#'
#' @param e1,e2 CFts. Instances of the CFts class.
#'
#' @return A CFts object with a set of offsets equal to the offsets of the
#' instances of CFts that the operator operates on. If the datums of the CFts
#' instances are not equivalent, an error is thrown.
#' @export
#'
#' @examples
#' e1 <- CFts(CFdatum("days since 1850-01-01", "gregorian"), 0:364)
#' e2 <- CFts(CFdatum("days since 1850-01-01 00:00:00", "standard"), 0:364)
#' e1 + e2
setMethod("+", c("CFts", "CFts"), function(e1, e2) if (e1@origin == e2@origin) CFts(e1@origin, c(e1@offsets, e2@offsets)) else stop('datums not equivalent'))

#' Extend an CFts object with additional offsets
#'
#' A CFts instance can be extnded by adding additional offsets using this
#' operator.
#'
#' The resulting CFts instance will have its offsets in the order that they are added,
#' meaning that the offsets from the CFts instance come first and those from the
#' numeric vector follow. There is no reordering or removal of duplicates. This
#' is because the time series are usually associated with a dataset and the
#' correspondence between the two is thus preserved.
#'
#' Note that when adding multiple vectors of offsets to a CFts instance, it is
#' more efficient to first concatenate the vectors and then do a final addition
#' to the CFts instance. So avoid `e1 + e2 + e3 + ...` but rather do
#' `e1 + c(e2, e3, ...)`.
#'
#' @param e1 CFts. Instance of the CFts class.
#' @param e2 numeric. Vector of offsets to be added to the CFts instance.
#'
#' @return A CFts object with offsets composed of the CFts instance and the
#' numeric vector.
#' @export
#'
#' @examples
#' e1 <- CFts(CFdatum("days since 1850-01-01", "gregorian"), 0:364)
#' e2 <- 365:729
#' e1 + e2
setMethod("+", c("CFts", "numeric"), function(e1, e2) CFts(e1@origin, c(e1@offsets, e2)))

