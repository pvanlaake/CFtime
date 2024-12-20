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
#' [CF Metadata Conventions document](https://cfconventions.org/Data/cf-conventions/cf-conventions-1.12/cf-conventions.html#time-coordinate).
#'
#' The package can create a [CFTime] instance from scratch or, more commonly, it
#' can use the dimension attributes and dimension variable values from a netCDF
#' resource. The package does not actually do any of the reading and the user is
#' free to use their netCDF package of preference. The recommended package to
#' use (with any netCDF resources) is [ncdfCF](https://cran.r-project.org/package=ncdfCF).
#' `ncdfCF` will automatically use this package to manage the "time" dimension
#' of any netCDF resource. As with this package, it reads and interprets the
#' attributes of the resource to apply the CF Metadata Conventions, supporting
#' axes, auxiliary coordinate variables, coordinate reference systems, etc.
#' Alternatively, for more basic netCDF reading and writing, the two main options
#' are [RNetCDF](https://cran.r-project.org/package=RNetCDF) and
#' [ncdf4](https://cran.r-project.org/package=ncdf4)).
#'
#' **Create, modify, inquire**
#' * [CFtime()]: Create a [CFTime] instance
#' * [`Properties`][properties] of the `CFTime` instance
#' * [parse_timestamps()]: Parse a vector of character timestamps into `CFTime` elements
#' * [`Compare`][CFtime-equivalent] two `CFTime` instances
#' * [`Merge`][CFtime-merge] two `CFTime` instances or append additional time steps to a `CFTime` instance
#' * [as_timestamp()] and [format()]: Generate a vector of character or `POSIXct` timestamps from a `CFTime` instance
#' * [range()]: Timestamps of the two endpoints in the time series
#' * [is_complete()]: Does the `CFTime` instance have a complete time series between endpoints?
#' * [month_days()]: How many days are there in a month using the calendar of the `CFTime` instance?
#'
#' **Factors and coverage**
#' * [CFfactor()] and [cut()]: Create factors for different time periods
#' * [CFfactor_units()]: How many units of time are there in each factor level?
#' * [CFfactor_coverage()]: How much data is available for each level of the factor?
#'
#' **Filtering and selection**
#' * [slab()]: Logical vector of time steps between two extreme points.
#' * [indexOf()]: Index values in the time series of given timestamps, possibly with
#' fractional part for interpolation.
#' @keywords internal
#' @aliases CFtime-package
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
