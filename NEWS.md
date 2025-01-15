# CFtime 1.5.0

* Code is updated to align with the CF 1.12 release. Specifically, calendars 
`utc` and `tai` are added.
* `standard` calendar now uses mixed Gregorian/Julian calendar as defined in the
CF Metadata Conventions. `proleptic_gregorian` is now a separate calendar with
its own code base.
* Negative offsets from a calendar origin are allowed.
* Code is refactored to R6. R6 class CFTime replaces S4 class CFtime (note the
difference in case). S4 class CFdatum has been replaced by hierarchy of
R6 CFCalendar classes, with various non-exported functions converted into
methods of CFCalendar. The code is now much cleaner and easier to extend, such
as with the two new calendars `utc` and `tai`.
* ISO8601 timestamps can use a comma "," as decimal mark to separate a 
fractional part from a time element, in addition to the dot ".".
* Do not drop degenerate dimension on bounds when only 1 offset is included in
subsetting.

# CFtime 1.4.1

* Method `slab()` has an additional argument `rightmost.closed` to indicate if
the upper extreme value should be included in the result.
* Fixed bounds information on CFtime instances returned from methods `indexOf()`
and `slab()`.
* Several minor code improvements.

# CFtime 1.4.0

* Bounds that define intervals around offsets can be associated with a CFtime
instance and retrieved as raw offset values or as formatted timestamps.
* Methods that subset a CF time series (e.g. `CFfactor()`, `cut()`, `slab()`)
now have an attribute "CFtime" (among possible others) that describes the "time"
dimension of the analysis result applying the subset. In other words, if CFtime 
instance 'Acf' describes the temporal dimension of data set 'A' and a factor 'Af'
is generated from 'Acf', then `Bcf <- attr(Af, "CFtime")` describes the temporal
dimension of the result of, say, `B <- apply(A, 1:2, tapply, Af, FUN)`.
* New `indexOf()` method added that returns the indices of supplied timestamps
in a CFtime instance, optionally with a fractional part. This can be used to
extract specific time steps, or to interpolate between time steps using the
fractional part, from the time dimension of the data set associated with the
CFtime instance. A vector of indices (e.g. referring to slices of the data set)
can also be supplied, in which case valid indices are returned, with the new
CFtime instance.
* New `cut()` method added to generate a factor, similar to `cut.POSIXt()` but with
some differences in the arguments.
* `CFfactor()` now supports a period "quarter", for calendar quarters.
* `format()` method added that generates a character vector of timestamps for the
offsets in a CFtime instance. The format is specified using the flags used in
`strptime()`, with some limitations. In particular, locale-specific formatting is
limited to month names and no weekday information can be generated. The `range()`
method has a new "format" parameter to support the same functionality and timestamps
can also be generated for the extremes of the bounds, if set.
* `as_character()` and `length()` methods added that return a vector of timestamps 
or the number of offsets in a CFtime instance, respectively.
* Several functions have been renamed (most notably `CFtimestamp()` to
`as_timestamp()`, `CFcomplete()` to `is_complete()`, `CFrange()` to the standard 
generic method `range()`, and `CFsubset()` to `slab()`) to be more consistent 
with the R universe. The original functions are now flagged as being deprecated. 
Some datum functions (deep down where regular mortals do not dwell) have been 
deleted.
* Time zone designator "UTC" accepted when parsing timestamps to offsets.
* Minor code fixes, see GitHub commits.
* Documentation updated, with description of new functions.

# CFtime 1.3.0

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
