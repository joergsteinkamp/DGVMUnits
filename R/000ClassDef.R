#' An S4 class holding the unit information.
#'
#'Each slot consists of a two element vector, holding the numeric unit prefix (multiplier) and exponent (the nominator is positive and the denominator negative).
#'
#' @slot g for gram
#' @slot m for meter
#' @slot W for watts
#' @slot s for seconds
#' @slot K for Kelvin
#' @import methods
setClass("RVCUnit",
         representation(g="numeric",
                        m="numeric",
                        W="numeric",
                        s="numeric",
                        K="numeric"),
         prototype(g=c(1, 0),
                   m=c(1, 0),
                   W=c(1, 0),
                   s=c(1, 0),
                   K=c(1, 0)
                   ))
