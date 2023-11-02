#' CFtime: working with CF Metadata Conventions "time" dimensions
#'
#' Support for all calendars as specified in the Climate and Forecast
#' (CF) Metadata Conventions for climate and forecasting data. The CF Metadata
#' Conventions is widely used for distributing files with climate observations
#' or projections, including the Coupled Model Intercomparison Project (CMIP)
#' data used by climate change scientists and the Intergovernmental Panel on
#' Climate Change (IPCC). This package specifically allows the user to work
#' with any of the CF-compliant calendars (many of which are not compliant with
#' POSIXt). The CF time coordinate is formally defined in the
#' [CF Metadata Conventions document](https://cfconventions.org/Data/cf-conventions/cf-conventions-1.10/cf-conventions.html#time-coordinate).
#'
#' The package can create a `CFtime` instance from scratch or, more commonly, it
#' can use the dimension attributes and dimension variable values from a NetCDF
#' resource. The package does not actually do any of the reading and the user is
#' free to use their NetCDF package of preference (with the two main options
#' being [RNetCDF](https://cran.r-project.org/package=RNetCDF) and
#' [ncdf4](https://cran.r-project.org/package=ncdf4)).
#'
#' **Create, modify, inquire**
#' * [CFtime()]: Create a CFtime instance
#' * [`Properties`][CFproperties] of the CFtime instance
#' * [CFparse()]: Parse a vector of character timestamps into CFtime elements
#' * [`Compare`][CFtime-equivalent] two CFtime instances
#' * [`Merge`][CFtime-merge] two CFtime instances
#' * [`Append`][CFtime-append] additional time steps to a CFtime instance
#' * [CFtimestamp()]: Generate a vector of character or `POSIXct` timestamps from a CFtime instance
#' * [CFrange()]: Timestamps of the two endpoints in the time series
#' * [CFcomplete()]: Does the CFtime instance have a complete time series between endpoints?
#' * [CFmonth_days()]: How many days are there in a month using the CFtime calendar?
#'
#' **Factors and coverage**
#' * [CFfactor()]: Create factors for different time periods
#' * [CFfactor_units()]: How many units of time are there in each factor level?
#' * [CFfactor_coverage()]: How much data is available for each level of the factor?
#' @keywords internal
#' @aliases CFtime-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
