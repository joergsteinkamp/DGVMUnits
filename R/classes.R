#' An S4 class holding the unit information.
#'
#' Each slot holds the same number of elements, which are used to construct a unit string. 
#' short and long name, numeric unit prefix (multiplier), exponent (the nominator is positive and the denominator negative)
#' and a reference (if not required it should be NA).
#'
#' @slot shortname hold sthe units short name (e.g. "g", "m", ...)
#' @slot longname can hold the long name (e.g. "gram", "meter", ...)
#' @slot scale if given with a prefix, e.g. k (kilo) it will become 1000
#' @slot offset if a value needs to be added or substracted, e.g. for temperatures degree C -> K
#' @slot exponent the exponent of the unit
#' @slot reference can hold a name to what the unit relates (if it differes for the same unit, they don't calcel out)
#' @import methods
setClass("DGVMUnit",
         representation(shortname="character",
                        longname="character",
                        scale="numeric",
                        offset="numeric",
                        exponent="numeric",
                        reference="character"))
