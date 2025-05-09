% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CFCalendarStandard.R
\docType{class}
\name{CFCalendarStandard}
\alias{CFCalendarStandard}
\title{Standard CF calendar}
\description{
This class represents a standard calendar of 365 or 366 days per
year. This calendar is compatible with the standard POSIXt calendar for
periods after the introduction of the Gregorian calendar, 1582-10-15
00:00:00. The calendar starts at 0001-01-01 00:00:00, e.g. the start of the
Common Era.

Note that this calendar, despite its name, is not the same as that used in
ISO8601 or many computer systems for periods prior to the introduction of
the Gregorian calendar. Use of the "proleptic_gregorian" calendar is
recommended for periods before or straddling the introduction date, as that
calendar is compatible with POSIXt on most OSes.
}
\section{Super class}{
\code{\link[CFtime:CFCalendar]{CFtime::CFCalendar}} -> \code{CFCalendarStandard}
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-CFCalendarStandard-new}{\code{CFCalendarStandard$new()}}
\item \href{#method-CFCalendarStandard-valid_days}{\code{CFCalendarStandard$valid_days()}}
\item \href{#method-CFCalendarStandard-is_gregorian_date}{\code{CFCalendarStandard$is_gregorian_date()}}
\item \href{#method-CFCalendarStandard-POSIX_compatible}{\code{CFCalendarStandard$POSIX_compatible()}}
\item \href{#method-CFCalendarStandard-month_days}{\code{CFCalendarStandard$month_days()}}
\item \href{#method-CFCalendarStandard-leap_year}{\code{CFCalendarStandard$leap_year()}}
\item \href{#method-CFCalendarStandard-date2offset}{\code{CFCalendarStandard$date2offset()}}
\item \href{#method-CFCalendarStandard-offset2date}{\code{CFCalendarStandard$offset2date()}}
\item \href{#method-CFCalendarStandard-clone}{\code{CFCalendarStandard$clone()}}
}
}
\if{html}{\out{
<details><summary>Inherited methods</summary>
<ul>
<li><span class="pkg-link" data-pkg="CFtime" data-topic="CFCalendar" data-id="add_day"><a href='../../CFtime/html/CFCalendar.html#method-CFCalendar-add_day'><code>CFtime::CFCalendar$add_day()</code></a></span></li>
<li><span class="pkg-link" data-pkg="CFtime" data-topic="CFCalendar" data-id="is_compatible"><a href='../../CFtime/html/CFCalendar.html#method-CFCalendar-is_compatible'><code>CFtime::CFCalendar$is_compatible()</code></a></span></li>
<li><span class="pkg-link" data-pkg="CFtime" data-topic="CFCalendar" data-id="is_equivalent"><a href='../../CFtime/html/CFCalendar.html#method-CFCalendar-is_equivalent'><code>CFtime::CFCalendar$is_equivalent()</code></a></span></li>
<li><span class="pkg-link" data-pkg="CFtime" data-topic="CFCalendar" data-id="offsets2time"><a href='../../CFtime/html/CFCalendar.html#method-CFCalendar-offsets2time'><code>CFtime::CFCalendar$offsets2time()</code></a></span></li>
<li><span class="pkg-link" data-pkg="CFtime" data-topic="CFCalendar" data-id="parse"><a href='../../CFtime/html/CFCalendar.html#method-CFCalendar-parse'><code>CFtime::CFCalendar$parse()</code></a></span></li>
<li><span class="pkg-link" data-pkg="CFtime" data-topic="CFCalendar" data-id="print"><a href='../../CFtime/html/CFCalendar.html#method-CFCalendar-print'><code>CFtime::CFCalendar$print()</code></a></span></li>
</ul>
</details>
}}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-new"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-new}{}}}
\subsection{Method \code{new()}}{
Create a new CF calendar. When called with the deprecated
'gregorian' it is automatically converted to the equivalent 'standard'.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$new(nm, definition)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{nm}}{The name of the calendar. This must be "standard" or
"gregorian" (deprecated).}

\item{\code{definition}}{The string that defines the units and the origin, as
per the CF Metadata Conventions.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A new instance of this class.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-valid_days"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-valid_days}{}}}
\subsection{Method \code{valid_days()}}{
Indicate which of the supplied dates are valid.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$valid_days(ymd)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ymd}}{\code{data.frame} with dates parsed into their parts in columns
\code{year}, \code{month} and \code{day}. Any other columns are disregarded.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Logical vector with the same length as argument \code{ymd} has rows
with \code{TRUE} for valid days and \code{FALSE} for invalid days, or \code{NA} where
the row in argument \code{ymd} has \code{NA} values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-is_gregorian_date"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-is_gregorian_date}{}}}
\subsection{Method \code{is_gregorian_date()}}{
Indicate which of the supplied dates are in the Gregorian
part of the calendar, e.g. 1582-10-15 or after.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$is_gregorian_date(ymd)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ymd}}{\code{data.frame} with dates parsed into their parts in columns
\code{year}, \code{month} and \code{day}. Any other columns are disregarded.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Logical vector with the same length as argument \code{ymd} has rows
with \code{TRUE} for days in the Gregorian part of the calendar and \code{FALSE}
otherwise, or \code{NA} where the row in argument \code{ymd} has \code{NA} values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-POSIX_compatible"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-POSIX_compatible}{}}}
\subsection{Method \code{POSIX_compatible()}}{
Indicate if the time series described using this calendar
can be safely converted to a standard date-time type (\code{POSIXct},
\code{POSIXlt}, \code{Date}). This is only the case if all offsets are for
timestamps fall on or after the start of the Gregorian calendar,
1582-10-15 00:00:00.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$POSIX_compatible(offsets)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{offsets}}{The offsets from the CFtime instance.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
\code{TRUE}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-month_days"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-month_days}{}}}
\subsection{Method \code{month_days()}}{
Determine the number of days in the month of the calendar.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$month_days(ymd = NULL)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{ymd}}{\code{data.frame}, optional, with dates parsed into their parts.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A vector indicating the number of days in each month for the
dates supplied as argument \code{ymd}. If no dates are supplied, the number
of days per month for the calendar as a vector of length 12, for a
regular year without a leap day.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-leap_year"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-leap_year}{}}}
\subsection{Method \code{leap_year()}}{
Indicate which years are leap years.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$leap_year(yr)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{yr}}{Integer vector of years to test.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Logical vector with the same length as argument \code{yr}. \code{NA} is
returned where elements in argument \code{yr} are \code{NA}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-date2offset"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-date2offset}{}}}
\subsection{Method \code{date2offset()}}{
Calculate difference in days between a \code{data.frame} of time
parts and the origin.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$date2offset(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{\code{data.frame}. Dates to calculate the difference for.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
Integer vector of a length equal to the number of rows in
argument \code{x} indicating the number of days between \code{x} and the origin
of the calendar, or \code{NA} for rows in \code{x} with \code{NA} values.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-offset2date"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-offset2date}{}}}
\subsection{Method \code{offset2date()}}{
Calculate date parts from day differences from the origin. This
only deals with days as these are impacted by the calendar.
Hour-minute-second timestamp parts are handled in \link{CFCalendar}.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$offset2date(x)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{x}}{Integer vector of days to add to the origin.}
}
\if{html}{\out{</div>}}
}
\subsection{Returns}{
A \code{data.frame} with columns 'year', 'month' and 'day' and as many
rows as the length of vector \code{x}.
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-CFCalendarStandard-clone"></a>}}
\if{latex}{\out{\hypertarget{method-CFCalendarStandard-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{CFCalendarStandard$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
