## Includes those base units currently:
##
## g: gramm
## m: meter
## s: seconds
## W: Watts
##
## each unit has two parts:
## 1.) the multiplier (from k(ilo), n(ano), h(our), yr, ...)
## 2.) exponents if it is multiplied
##     and if it is in the nominator (positive) or denominator (negative)

setClass("RVCUnit",
         representation(g="numeric",
                        m="numeric",
                        W="numeric",
                        s="numeric"),
         prototype(g=c(1, 0),
                   m=c(1, 0),
                   W=c(1, 0),
                   s=c(1, 0)
                   ))
