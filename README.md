
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CFtime

<!-- badges: start -->

[![Lifecycle:
Stable](https://img.shields.io/badge/Lifecycle-Stable-green.svg)](https://lifecycle.r-lib.org/articles/stages.html)
[![CRAN
Status](https://www.r-pkg.org/badges/version/CFtime)](https://cran.r-project.org/package=CFtime)
[![CRAN
Downloads](https://cranlogs.r-pkg.org/badges/grand-total/CFtime)](https://cran.r-project.org/package=CFtime)
[![License: GPL
v3](https://img.shields.io/badge/License-MIT-blue.svg)](https://mit-license.org)
[![Commits since
release](https://img.shields.io/github/commits-since/R-CF/CFtime/latest.svg?color=green)](https://GitHub.com/R-CF/CFtime/commit/main/)
[![Last
commit](https://img.shields.io/github/last-commit/R-CF/CFtime)](https://github.com/R-CF/CFtime/commits/main)
[![R-CMD-check](https://github.com/R-CF/CFtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/R-CF/CFtime/actions/workflows/R-CMD-check.yaml)
[![codecov](https://codecov.io/gh/R-CF/CFtime/branch/main/graph/badge.svg)](https://app.codecov.io/gh/R-CF/CFtime)

<!-- badges: end -->

CFtime is an R package that supports working with [CF Metadata
Conventions](http://cfconventions.org) time coordinates, specifically
geared to time-referencing data sets of climate projections such as
those produced under the [World Climate Research
Programme](https://www.wcrp-climate.org) and re-analysis data such as
ERA5 from the European Centre for Medium-range Weather Forecasts
(ECMWF).

The data sets include in their metadata an epoch, or origin, a point in
time from which other points in time are calculated. This epoch takes
the form of `days since 1949-12-01`, with each data collection (Coupled
Model Intercomparison Project (CMIP) generation, model, etc) having its
own epoch. The data itself has a temporal dimension if a coordinate
variable in the netCDF file has an attribute `units` with a string value
describing an epoch. The coordinate variable, say “time”, has data
values such as 43289, which are offsets from the epoch in units of the
epoch string (“days” in this case). To convert this offset to a date,
using a specific calendar, is what this package does. Given that the
calendars supported by the CF Metadata Conventions are not compatible
with `POSIXt`, this conversion is not trivial because the standard R
date-time operations do not give correct results. That it is important
to account for these differences is easily demonstrated:

``` r
library(CFtime)

# POSIXt calculations on a standard calendar
as.Date("1949-12-01") + 43289
#> [1] "2068-06-08"

# CFtime calculation on a "360_day" calendar
as_timestamp(CFtime("days since 1949-12-01", "360_day", 43289))
#> [1] "2070-02-30"
```

That’s a difference of nearly 21 months! (And yes, 30 February is a
valid date on a `360_day` calendar.)

All defined calendars of the CF Metadata Conventions are supported:

- `standard` or `gregorian`: This calendar is valid for the Common Era
  only; it starts at 0001-01-01 00:00:00, i.e. 1 January of year 1. Time
  periods prior to the introduction of the Gregorian calendar
  (1582-10-15) use the `julian` calendar that was in common use then.
  The 10-day gap between the Julian and Gregorian calendars is observed,
  so dates in the range 5 to 14 October 1582 are invalid.
- `proleptic_gregorian`: This calendar uses the Gregorian calendar for
  periods prior to the introduction of that calendar as well, and it
  extends to periods before the Common Era, e.g. year 0 and negative
  years.
- `tai`: International Atomic Time, a global standard for linear time
  based on multiple atomic clocks: it counts seconds since its start at
  1958-01-01 00:00:00. For presentation it uses the Gregorian calendar.
  Timestamps prior to its start are not allowed.
- `utc`: Coordinated Universal Time, the standard for civil timekeeping
  all over the world. It is based on International Atomic Time but it
  uses occasional leap seconds to remain synchronous with Earth’s
  rotation around the Sun; at the end of 2024 it is 37 seconds behind
  `tai`. It uses the Gregorian calendar with a start at 1972-01-01
  00:00:00; earlier timestamps are not allowed. Future timestamps are
  also not allowed because the insertion of leap seconds is
  unpredictable. Most computer clocks synchronize against UTC but
  calculations of periods do not consider leap seconds.
- `julian`: The `julian` calendar has a leap year every four years,
  including centennial years. Otherwise it is the same as the `standard`
  calendar.
- `365_day` or `noleap`: This is a “model time” calendar in which no
  leap years occur. Year 0 exists, as well as years prior to that.
- `366_day` or `all_leap`: This is a “model time” calendar in which all
  years are leap years. Year 0 exists, as well as years prior to that.
- `360_day`: This is a “model time” calendar in which every year has 360
  days divided over 12 months of 30 days each. Year 0 exists, as well as
  years prior to that.
- `none`: Perpetual “calendar” for experiments that are simulated on a
  given instant during the year. All the elements in this calendar thus
  represent the same instant in time.

Use of custom calendars is currently not supported.

This package facilitates use of a suite of models of climate projections
that use different calendars in a consistent manner. This package is
particularly useful for working with climate projection data having a
daily or higher resolution, but it will work equally well on data with a
lower resolution.

Timestamps are generated using the [ISO8601
standard](https://en.wikipedia.org/wiki/ISO_8601).

Calendar-aware factors can be generated to support processing of data
using `tapply()` and similar functions. Merging of multiple data sets
and subsetting facilitate analysis while preserving computer resources.

## Installation

Get the latest stable version on CRAN:

``` r
install.packages("CFtime")
```

You can install the development version of CFtime from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("R-CF/CFtime")
```

## Basic operation

The package contains a class, `CFTime`, to describe the time coordinate
reference system, including its calendar and origin, and which holds the
time coordinate values that are offset from the origin to represent
instants in time. This class operates on the data in the file of
interest, here a Coordinated Regional Climate Downscaling Experiment
(CORDEX) file of precipitation for the Central America domain.

> In this vignette we are using the [`ncdfCF`
> package](https://cran.r-project.org/package=ncdfCF) as that provides
> the easiest interface to work with netCDF files. Package `CFtime` is
> integrated into `ncdfCF` which makes working with time dimensions in
> netCDF seamless.  
> Packages `RNetCDF` and `ncdf4` can work with `CFtime` as well but then
> the “intelligence” built into `ncdfCF` is not available, such as
> automatically identifying axes and data orientation. Other packages
> like `terra` and `stars` are not recommended because they do not
> provide access to the specifics of the time dimension of the data and
> do not properly consider any calendars other than “standard” and
> “proleptic_gregorian”.

``` r
# install.packages("ncdfCF")
library(ncdfCF)

# Opening a data set that is included with the package.
# Usually you would `list.files()` on a directory of your choice.
fn <- list.files(path = system.file("extdata", package = "CFtime"), full.names = TRUE)[1]
ds <- open_ncdf(fn)
ds$attribute("title")
#> [1] "NOAA GFDL GFDL-ESM4 model output prepared for CMIP6 update of RCP4.5 based on SSP2"
ds$attribute("license")
#> [1] "CMIP6 model data produced by NOAA-GFDL is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (https://creativecommons.org/licenses/). Consult https://pcmdi.llnl.gov/CMIP6/TermsOfUse for terms of use governing CMIP6 output, including citation requirements and proper acknowledgment. Further information about this data, including some limitations, can be found via the further_info_url (recorded as a global attribute in this file). The data producers and data providers make no warranty, either express or implied, including, but not limited to, warranties of merchantability and fitness for a particular purpose. All liabilities arising from the supply of the information (including any liability arising in negligence) are excluded to the fullest extent permitted by law."

# What axes are there in the data set?
dimnames(ds)
#> [1] "bnds" "lat"  "time" "lon"

# Get the CFTime instance from the "time" axis
(time <- ds[["time"]]$time())
#> CF calendar:
#>   Origin  : 1850-01-01T00:00:00
#>   Units   : days
#>   Type    : noleap
#> Time series:
#>   Elements: [2015-01-01T12:00:00 .. 2099-12-31T12:00:00] (average of 1.000000 days between 31025 elements)
#>   Bounds  : set
```

Note that the `ncdfCF` package reads the netCDF file and interprets its
contents on the basis of its attribute values. If an axis is found that
represents time, then a `CFTime` instance is created for it, which can
be accessed with the `time()` method.

##### Using RNetCDF or ncdf4

If you are using the `RNetCDF` or `ncdf4` package rather than `ncdfCF`,
creating a `CFTime` instance goes like this, assuming that the axis is
called “time”:

``` r
library(RNetCDF)
nc <- open.nc(fn)
time <- CFtime(att.get.nc(nc, "time", "units"), 
               att.get.nc(nc, "time", "calendar"), 
               var.get.nc(nc, "time"))

library(ncdf4)
nc <- nc_open(fn)
names(nc$var) # A mix of data variables, axes, and other objects
t <- CFtime(nc$dim$time$units, 
            nc$dim$time$calendar, 
            nc$dim$time$vals)
```

## Typical workflow

In a typical process, you would combine multiple data files into a
single data set to analyze a feature of interest. To continue the
previous example of precipitation in the Central America domain using
CORDEX data, you can calculate the precipitation per month for the
period 2041 - 2050 as follows:

``` r
# NOT RUN
library(ncdfCF)
library(abind)

# Open the files - one would typically do this in a loop
ds2041 <- open_ncdf("~/pr_CAM-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_GERICS-REMO2015_v1_day_20410101-20451230.nc")
ds2046 <- open_ncdf("~/pr_CAM-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_GERICS-REMO2015_v1_day_20460101-20501230.nc")

# Create the time object from the first file
# All files have an identical "time" axis as per the CORDEX specifications
time <- ds2041[["time"]]$time()

# Add the time values from the remaining files
time <- time + ds2046[["pr"]]$time()$offsets

# Grab the data from the files and merge the arrays into one, in the same order
# as the time values
pr <- abind(ds2041[["pr"]]$data()$array(), ds2046[["pr"]]$data()$array())

# Create the month factor from the time object
f_month <- CFfactor(time, "month")

# The result from applying this factor to a data set that it describes is a new
# data set with a different "time" dimension. The function result stores this
# new time object as an attribute.
pr_month_time <- attr(f_month, "CFTime")

# Now sum the daily data to monthly data
# Dimensions 1 and 2 are longitude and latitude, the third dimension is time
pr_month <- aperm(apply(pr, 1:2, tapply, f_month, sum), c(2, 3, 1))
dimnames(pr_month)[[3]] <- as_timestamp(pr_month_time)
```

## Coverage

This package has been tested with the following data sets:

- ERA5 (including multiple variables, levels, and mixed ERA5/ERA5T data)
- CMIP5
- CORDEX
- CMIP6
- ROMS

The package also operates on geographical and/or temporal subsets of
data sets so long as the subsetted data complies with the CF Metadata
Conventions. This includes subsetting in the [Climate Data
Store](https://cds.climate.copernicus.eu/#!/home). Subsetted data from
Climate4Impact is not automatically supported because the dimension
names are not compliant with the CF Metadata Conventions, use the
corresponding dimension names instead.
