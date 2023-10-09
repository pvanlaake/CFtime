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
