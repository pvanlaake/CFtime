% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CFts.R
\docType{class}
\name{CFts-class}
\alias{CFts-class}
\title{CF-convention time series}
\value{
An object of class CFts.
}
\description{
CF-convention time series
}
\section{Slots}{

\describe{
\item{\code{origin}}{CFdatum. The atomic origin upon which the \code{offsets} are based.}

\item{\code{offsets}}{numeric. Vector of offsets in units and from a timestamp}

\item{\code{ymds}}{matrix. Matrix of numeric date elements year, month, day and seconds from offsets.
defined by the \code{origin}.}
}}

