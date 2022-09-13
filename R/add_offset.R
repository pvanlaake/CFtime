#' Add an offset, in units of the datum, to a timestamp
#'
#' @param datum CFdatum. The datum that defines the unit of the offsets and the
#' origin to add the offsets to.
#' @param offsets numeric. Vector of offsets to add to the datum.
#'
#' @return A tibble with columns year, month, day, hour, minute, second, offset.
#' @export
#'
#' @examples
#' datum <- CFdatum("days since 1850-01-01", "gregorian")
#' add_offset(datum, 0:364)
add_offset <- function(datum, offsets) {
  stopifnot(methods::is(datum, "CFdatum"), length(datum) == 1, is.numeric(offsets))
  if (any(offsets < 0)) stop("offsets must be positive")

  ymds <- .add_offset(offsets, datum)
  hour <- ymds[, 4] %/% 3600
  minute <- (ymds[, 4] %% 3600) %/% 60
  second <- ymds[, 4] %% 60
  dplyr::tibble(year = ymds[, 1], month = ymds[, 2], day = ymds[, 3], hour, minute, second, offset = offsets)
}

# Add an offset, in units of the datum, to a timestamp
#
# This function adds a specified amount of time to the origin of a CFts object.
#
# This is an internal function that should not be used outside of the CFtime package.
#
# @param offset numeric. Vector of offsets to add to the datum.
# @param datum CFdatum. The datum that defines the unit of the offsets and the
# origin to add the offsets to.
#
# @return A matrix with four column (year, month, day, second) and as many
# rows as there are offsets
.add_offset <- function(offset, datum) {
  origin <- datum@origin

  # First add time
  switch (datum@unit,
          secs <- (offset %% 1) * 86400,
          secs <- offset * 3600,
          secs <- offset * 60,
          secs <- offset)
  secs <- secs + origin[4]
  days <- secs %/% 86400  # overflow days
  secs <- secs %% 86400   # drop overflow days from time

  # Now add days using the calendar of the datum
  if (datum@unit == 1) days <- days + offset %/% 1
  if (any(days > 0)) {
    cal <- datum@cal_id
    if (cal == 1)      ymd <- .offset2date_standard(days, origin)
    else if (cal == 2) ymd <- .offset2date_julian(days, origin)
    else if (cal == 3) ymd <- .offset2date_360(days, origin)
    else if (cal == 4) ymd <- .offset2date_fixed(days, origin, c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 365)
    else if (cal == 5) ymd <- .offset2date_fixed(days, origin, c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31), 366)
  } else {
    len <- length(offset)
    ymd <- matrix(c(rep(origin[1], len), rep(origin[2], len), rep(origin[3], len)), nrow = len, ncol = 3)
  }
  return(cbind(ymd, secs))
}

# 360_day, use integer arithmetic
.offset2date_360 <- function(x, origin) {
  y <- origin[1] + x %/% 360
  m <- origin[2] + (x %% 360) %/% 30
  d <- origin[3] + x %% 30
  over <- which(d > 30)
  d[over] <- d[over] - 30
  m[over] <- m[over] + 1
  over <- which(m > 12)
  m[over] <- m[over] - 12
  y[over] <- y[over] + 1
  matrix(c(y, m, d), ncol = 3)
}

# Fixed year length, either 365_day or 366_day
.offset2date_fixed <- function(x, origin, month, ydays) {
  # First process full years over the vector
  yr <- origin[1] + (x %/% ydays)
  x <- x %% ydays

  # Remaining portion per datum
  x <- x + origin[3]
  ymd <- mapply(function(y, m, d) {
    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1
      if (m == 13) {
        y <- y + 1
        m <- 1
      }
    }
    return(c(y, m, d))
  }, yr, origin[2], x)
  return(t(ymd))
}

# Julian calendar offsetting
.offset2date_julian <- function(x, origin) {
  common_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leap_days   <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Is the leap day to consider ahead in the year from the base date (offset = 0) or in the next year (offset = 1)
  offset <- as.integer(origin[2] > 2)

  # First process 4-year cycles of 1,461 days over the vector
  yr <- origin[1] + (x %/% 1461) * 4
  x <- x %% 1461

  # Remaining portion per datum
  x <- x + origin[3]
  ymd <- mapply(function(y, m, d) {
    repeat {
      leap <- (y + offset) %% 4 == 0
      ydays <- 365 + as.integer(leap)

      if (d > ydays) {
        d <- d - ydays
        y <- y + 1
      } else break
    }

    if (leap) month <- leap_days else month <- common_days

    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1
      if (m == 13) {
        y <- y + 1
        m <- 1
      }
    }
    return(c(y, m, d))
  }, yr, origin[2], x)
  return(t(ymd))
}

# standard calendar offsetting
.offset2date_standard <- function(x, origin) {
  common_days <- c(31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)
  leap_days   <- c(31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31)

  # Is the leap day to consider ahead in the year from the base date (offset = 0) or in the next year (offset = 1)
  offset <- as.integer(origin[2] > 2)

  x <- x + origin[3]
  ymd <- mapply(function(y, m, d) {
    repeat {
      test <- y + offset
      leap <- (test %% 4 == 0 && test %% 100 > 0) || test %% 400 == 0
      ydays <- 365 + as.integer(leap)

      if (d > ydays) {
        d <- d - ydays
        y <- y + 1
      } else break
    }

    if (leap) month <- leap_days else month <- common_days

    while (d > month[m]) {
      d <- d - month[m]
      m <- m + 1
      if (m == 13) {
        y <- y + 1
        m <- 1
      }
    }
    return(c(y, m, d))
  }, origin[1], origin[2], x)
  return(t(ymd))
}
