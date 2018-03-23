# DGVMUnits

[![Build Status](https://travis-ci.org/joergsteinkamp/DGVMUnits.svg?branch=master)](https://travis-ci.org/joergsteinkamp/DGVMUnits)
[![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/DGVMUnits)](https://cran.r-project.org/package=DGVMUnits)
[![Downloads](https://cranlogs.r-pkg.org/badges/DGVMUnits)](https://cran.r-project.org/package=DGVMUnits)

Simple unit conversion and parsing for Vegetation and (not yet) Climate modelling.

```{r}
if("package:DGVMUnits" %in% search()) detach(name = "package:DGVMUnits", unload = TRUE)
library(DGVMUnits)

x <- as.DGVMUnit("kW h m-2")
## returns a simple string
as.character(x)
## returns an expression for usage as e.g. axis label in plots
as.expression(x)

## convert several strings at once (not implrmented by purpose) 
test.str <- c("m2 s", "m2/s", "m^2*s", "W m^-2 s^-1")
x <- as.DGVMUnit(test.str)

## unit division and cancelation
x <- as.DGVMUnit("kg/m2")
y <- as.DGVMUnit("kg m^-2 yr^-1")
ret <- divide(x,y)
as.character(ret)

## multiplication
x <- as.DGVMUnit("kg/m2")
y <- as.DGVMUnit("ha")
ret <- multiply(x,y)
as.character(ret)
```
