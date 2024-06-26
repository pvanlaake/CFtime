% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CFtime.R
\name{bounds}
\alias{bounds}
\alias{bounds,CFtime-method}
\alias{bounds<-}
\alias{bounds<-,CFtime-method}
\title{Bounds of the time offsets}
\usage{
bounds(x, format)

\S4method{bounds}{CFtime}(x, format)

bounds(x) <- value

\S4method{bounds}{CFtime}(x) <- value
}
\arguments{
\item{x}{A \code{CFtime} instance}

\item{format}{Optional. A single string with format specifiers, see
\code{\link[=format]{format()}} for details.}

\item{value}{A \code{matrix} (or \code{array}) with dimensions (2, length(offsets))
giving the lower (first row) and higher (second row) bounds of each offset
(this is the format that the CF Metadata Conventions uses for storage in
NetCDF files). Use \code{FALSE} to unset any previously set bounds, \code{TRUE} to
set regular bounds at mid-points between the offsets (which must be regular
as well).}
}
\value{
If bounds have been set, an array of bounds values with dimensions
(2, length(offsets)). The first row gives the lower bound, the second row
the upper bound, with each column representing an offset of \code{x}. If the
\code{format} argument is specified, the bounds values are returned as strings
according to the format. \code{NULL} when no bounds have been set.
}
\description{
CF-compliant NetCDF files store time information as a single offset value for
each step along the dimension, typically centered on the valid interval of
the data (e.g. 12-noon for day data). Optionally, the lower and upper values
of the valid interval are stored in a so-called "bounds" variable, as an
array with two rows (lower and higher value) and a column for each offset.
With function \verb{bounds()<-} those bounds can be set for a CFtime instance. The
bounds can be retrieved with the \code{bounds()} function.
}
\examples{
cf <- CFtime("days since 2024-01-01", "standard", seq(0.5, by = 1, length.out = 366))
as_timestamp(cf)[1:3]
bounds(cf) <- rbind(0:365, 1:366)
bounds(cf)[, 1:3]
bounds(cf, "\%d-\%b-\%Y")[, 1:3]
}
