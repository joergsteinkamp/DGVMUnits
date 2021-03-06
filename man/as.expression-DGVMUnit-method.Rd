% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/methods.R
\docType{methods}
\name{as.expression,DGVMUnit-method}
\alias{as.expression,DGVMUnit-method}
\title{Expression for labels.}
\usage{
\S4method{as.expression}{DGVMUnit}(x, ...)
}
\arguments{
\item{x}{An DGVMUnit object.}

\item{...}{Ignored further arguments.}
}
\value{
an expression
}
\description{
Returns an expression from a DGVMUnit object, which can be used e.g. as axis label.
}
\examples{
x <- as.DGVMUnit("kW h m-2")
as.expression(x)
units <- c("m2 s", "m2/s", "m^2*s", "C d")
x <- as.DGVMUnit(units)
sapply(x, as.expression)
}
