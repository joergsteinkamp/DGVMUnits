# RVCUnits

Simple unit conversion and parsing for Vegetation and (not yet) Climate modelling.

```{r}
if("package:RVCUnits" %in% search()) detach(name = "package:RVCUnits", unload = TRUE)
library(RVCUnits)

x <- as.RVCUnit("kW h m-2")
## returns a simple string
as.character(x)
## returns an expression for usage as e.g. axis label in plots
as.expression(x)

## convert several strings at once (not implrmented by purpose) 
test.str <- c("m2 s", "m2/s", "m^2*s", "W m^-2 s^-1")
x <- as.RVCUnit(test.str)

## unit division and cancelation
x <- as.RVCUnit("kg/m2")
y <- as.RVCUnit("kg m^-2 yr^-1")
ret <- divide(x,y)
as.character(ret)

## multiplication
x <- as.RVCUnit("kg/m2")
y <- as.RVCUnit("ha")
ret <- multiply(x,y)
as.character(ret)
```
