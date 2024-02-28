# CFtime 1.3.0

Changes since release 1.2.0:

* Two CFtime instances can be added if they have compatible calendars and units.
The earlier origin is preserved in the result and offsets from the later instance
are incremented by the appropriate amount. As before, in the result offsets are 
in the order of the specified CFtime instances.
* A new function CFsubset() can be used to retrieve a logical vector that indicates
which time steps in a CFtime instance fall between two timestamps. This is useful
to slice data during reading from file or analysis.
* Time zone information is managed at the level of the datum. If a vector of character
timestamps is parsed and found to have different time zones, a warning is generated.
* Much reduced memory footprint.
* Minor code fixes, see GitHub commits.
* Codecov.io test coverage monitoring added.
* Documentation updated, with description of new functions.

# CFtime 1.2.0

Changes since release 1.1.0:

* Datum units "years" and "months" added. While these units are discouraged by
the CF Metadata Conventions due to their problematic definition, there are quite
a few data sets out there that use these units nonetheless. For this reason,
reading existing files with such datum units is supported (converting offsets to
time elements is easy) but parsing timestamps is not (calculating offsets from
time elements is possible but tedious and slow). Should there be a definite need,
open an issue on GitHub and make a *very good* case why this functionality is
required.
* CFresolution() returns the average separation between elements in a time series,
in units of the datum.
* CFcomplete() indicates if the time series is complete, meaning that there are
no gaps in the time series. This also works for time series with a somewhat 
variable length such as monthly data with a "days" datum unit. This works for 
all but the most exotic time dimension constructions.
* CFtimestamp() produces a timestamp for all midnight values if the datum unit is
"hours", "minutes" or "seconds". The "time" format has been removed. For "standard",
"gregorian" and "proleptic_gregorian" calendars output can be generated as POSIXct
by specifying the new argument `asPOSIX = TRUE` -- defaults to `FALSE`, the
previous behaviour so the API is not broken.
* Minor documentation updates.
* Assorted minor code fixes, see GitHub commits.

# CFtime 1.1.0

Changes since release 1.0.0:

* CFtime() can now also be invoked with a vector of character timestamps as offsets, or
with a single timestamp to create a complete time series from the datum to the
indicated timestamp.
* CFtimestamp() can now automatically select the best format for the time series.
* New CFfactor_units() and CFfactor_coverage() functions. CFfactor_units() will 
tell you how many time units compose every level of a factor. CFfactor_coverage() 
computes the actual or relative number of time units in the factor levels from the 
time series in a CFtime instance with which the factor was created. This will 
enable you to assess the completeness of your time series (and perhaps filter out 
factor levels below a certain coverage threshold) and it can be useful in computing 
absolute values from average values, as is often useful when computing anomalies.
* Global constants are now defined in a package environment, CFt.
* Documentation expanded, updated and fixed.
* Assorted minor code fixes, see GitHub commits.

# CFtime 1.0.0

* This version supports all CF Metadata Conventions calendars for use with climate 
projection data.
* You can create timestamps from the offsets in the files and create factors that 
greatly simplify working with climate change data.
