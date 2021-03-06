% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/classes.R
\docType{class}
\name{DGVMUnit-class}
\alias{DGVMUnit-class}
\title{An S4 class holding the unit information.}
\description{
Each slot holds the same number of elements, which are used to construct a unit string. 
short and long name, numeric unit prefix (multiplier), exponent (the nominator is positive and the denominator negative)
and a reference (if not required it should be NA).
}
\section{Slots}{

\describe{
\item{\code{shortname}}{hold sthe units short name (e.g. "g", "m", ...)}

\item{\code{longname}}{can hold the long name (e.g. "gram", "meter", ...)}

\item{\code{scale}}{if given with a prefix, e.g. k (kilo) it will become 1000}

\item{\code{offset}}{if a value needs to be added or substracted, e.g. for temperatures degree C -> K}

\item{\code{exponent}}{the exponent of the unit}

\item{\code{reference}}{can hold a name to what the unit relates (if it differes for the same unit, they don't calcel out)}
}}

