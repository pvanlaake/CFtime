
<!-- README.md is generated from README.Rmd. Please edit that file -->

# CFtime

<!-- badges: start -->

[![R-CMD-check](https://github.com/pvanlaake/CFtime/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/pvanlaake/CFtime/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

CFtime is an R package that supports working with [CF Metadata
Conventions](http://cfconventions.org) time coordinates, specifically
geared to time-referencing data sets of climate projections such as
those produced under the [World Climate Research
Programme](https://www.wcrp-climate.org) and re-analysis data such as
ERA5 from the European Centre for Medium-range Weather Forecasts
(ECMWF).

The data sets include in their metadata a datum, a point in time from
which other points in time are calculated. This datum takes the form of
`days since 1949-12-01`, with each data source (Coupled Model
Intercomparison Project (CMIP) generation, model, etc) having its own
datum. The data itself has a dimension attribute of “time” with offset
values such as 43289. To convert this “offset” to a date, using a
specific calendar, is what this package does. Given that most calendars
supported by the CF Metadata Conventions are not compatible with
`POSIXt`, this conversion is not trivial. That it is important account
for these differences is easily demonstrated:

``` r
library(CFtime)

# POSIXt calculations on a standard calendar
as.Date("1949-12-01") + 43289
#> [1] "2068-06-08"

# CFtime calculation on a "360_day" calendar
CFtimestamp(CFtime("days since 1949-12-01", "360_day", 43289))
#> [1] "2070-02-30"
```

That’s a difference of nearly 21 months! (And yes, 30 February is a
valid date on a `360_day` calendar.)

All defined calendars of the CF Metadata Convention are supported:

- `standard` or `gregorian`
- `proleptic_gregorian`
- `julian`
- `365_day` or `noleap`
- `366_day` or `all_leap`
- `360_day`

Use of custom calendars is not supported. This package is also not
suitable for paleo-calendars. Time periods prior to the introduction of
the Gregorian calendar (1582-10-15) may be used but there are no special
provisions for it. Finally, there is no specific consideration for the
year 0 (which does not exist in any of the above calendars). Given that
climate change projections are typically made from 1850-01-01 onwards,
these limitations should not be of any practical concern.

This package IS NOT intended to support the full date and time
functionality of the CF Metadata Conventions. Instead, it facilitates
use of a suite of models of climate change projections that use
different calendars in a consistent manner.

This package is particularly useful for working with climate change
projections having a daily or higher resolution, but it will work
equally well on data with a lower resolution.

Timestamps are generated using the [ISO8601
standard](https://en.wikipedia.org/wiki/ISO_8601).

Calendar-aware factors can be generated to support processing of data
using `tapply()` and similar functions.

## Installation

Get the latest stable version on CRAN:

``` r
install.packages("CFtime")
```

You can install the development version of CFtime from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("pvanlaake/CFtime")
```

## Basic operation

The package contains a class, `CFtime` to describe the time coordinate
system, including its calendar, - a *datum* - and which holds the time
coordinate values that are offset from the datum to represent instants
in time. This class operates on the data in the file of interest, here a
CORDEX file of precipitation for the Central America domain:

``` r
library(ncdf4)

# Opening a data file that is included with the package.
# Usually you would `list.files()` on a directory of your choice.
nc <- nc_open(list.files(path = system.file("extdata", package = "CFtime"), full.names = TRUE)[1])
attrs <- ncatt_get(nc, "")
attrs$title
#> [1] "NOAA GFDL GFDL-ESM4 model output prepared for CMIP6 update of RCP4.5 based on SSP2"
attrs$license
#> [1] "CMIP6 model data produced by NOAA-GFDL is licensed under a Creative Commons Attribution-ShareAlike 4.0 International License (https://creativecommons.org/licenses/). Consult https://pcmdi.llnl.gov/CMIP6/TermsOfUse for terms of use governing CMIP6 output, including citation requirements and proper acknowledgment. Further information about this data, including some limitations, can be found via the further_info_url (recorded as a global attribute in this file). The data producers and data providers make no warranty, either express or implied, including, but not limited to, warranties of merchantability and fitness for a particular purpose. All liabilities arising from the supply of the information (including any liability arising in negligence) are excluded to the fullest extent permitted by law."

# Create the CFtime instance
cf <- CFtime(nc$dim$time$units, nc$dim$time$calendar, nc$dim$time$vals)
cf
#> CF datum of origin:
#>   Origin  : 1850-01-01 00:00:00
#>   Units   : days
#>   Calendar: noleap
#> CF time series:
#>   Elements: [2015-01-01T12:00:00 .. 2099-12-31T12:00:00] (average of 1.000000 days between 31025 elements)

# ... work with the data ...
nc_close(nc)
```

Note that the information of interest (`nc$dim$time$units`, etc) is read
out of the file “blindly”, without checking for available dimensions or
attributes. This can be done because the `time` dimension and its
attributes `units` and `calendar` are required by the CF Metadata
Conventions. Should this fail, then your data set is not compliant. You
could still use this package if the required information is contained in
your file but using a different dimension name or different attribute
names.

## Typical workflow

In a typical process, you would combine multiple data files into a
single data set to analyze a feature of interest. To continue the
previous example of precipitation in the Central America domain using
Coordinated Regional Climate Downscaling Experiment (CORDEX) data, you
can calculate the precipitation per month for the period 2041 - 2050 as
follows:

``` r
# NOT RUN
library(CFtime)
library(ncdf4)
library(abind)
# Open the files - one would typically do this in a loop
nc2041 <- nc_open("~/CC/CORDEX/CAM-22/RCP2.6/pr_CAM-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_GERICS-REMO2015_v1_day_20410101-20451230.nc")
nc2046 <- nc_open("~/CC/CORDEX/CAM-22/RCP2.6/pr_CAM-22_MOHC-HadGEM2-ES_rcp26_r1i1p1_GERICS-REMO2015_v1_day_20460101-20501230.nc")

# Create the time object from the first file
# All files have an identical datum as per the CORDEX specifications
time <- CFtime(nc2041$dim$time$units, nc2041$dim$time$calendar, nc2041$dim$time$vals)

# Add the time values from the remaining files
time <- time + as.vector(nc2046$dim$time$vals)

# Grab the data from the files and merge the arrays into one, in the same order
# as the time values
pr <- abind(ncvar_get(nc2041, "pr"), ncvar_get(nc2046, "pr"))
nc_close(nc2041)
nc_close(nc2046)

# Optionally - Set the time dimension to the timestamps from the time object
dimnames(pr)[[3]] <- CFtimestamp(time)

# Create the month factor from the time object
f_month <- CFfactor(time, "month")

# Now sum the daily data to monthly data
# Dimensions 1 and 2 are longitude and latitude, the third dimension is time
pr_month <- aperm(apply(pr, 1:2, tapply, f_month, sum), c(2, 3, 1))
dimnames(pr_month)[[3]] <- levels(f_month)
```

## Coverage

This package has been tested with the following data sets:

- ERA5 (including multiple variables, levels, and mixed ERA5/ERA5T data)
- CMIP5
- CORDEX
- CMIP6

The package also operates on geographical and/or temporal subsets of
data sets so long as the subsetted data complies with the CF Metadata
Conventions. This includes subsetting in the [Climate Data
Store](https://cds.climate.copernicus.eu/#!/home). Subsetted data from
Climate4Impact is not automatically supported because the dimension
names are not compliant with the CF Metadata Conventions, use the
corresponding dimension names instead.
