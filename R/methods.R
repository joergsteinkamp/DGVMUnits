#' CF-conform unit string.
#'
#' Returns a String from a DGVMUnit object, which can be used e.g. in ud.convert of package udunits2.
#'
#' @param x An DGVMUnit object.
#' @param ... Ignored further arguments.
#' @return a character string
#'
#' @examples
#' x <- as.DGVMUnit("kW h m-2")
#' as.character(x)
#' units <- c("m2 s", "m2/s", "m^2*s", "C d")
#' x <- as.DGVMUnit(units)
#' sapply(x, as.character)
#' @export
#' @include internal.R
setMethod("as.character", signature("DGVMUnit"), function(x, ...) {
  return(.as.char(x))
})

#' Expression for labels.
#'
#' Returns an expression from a DGVMUnit object, which can be used e.g. as axis label.
#'
#' @param x An DGVMUnit object.
#' @param ... Ignored further arguments.
#' @return an expression
#'
#' @include internal.R
#'
#' @examples
#' x <- as.DGVMUnit("kW h m-2")
#' as.expression(x)
#' units <- c("m2 s", "m2/s", "m^2*s", "C d")
#' x <- as.DGVMUnit(units)
#' sapply(x, as.expression)
#' @export
#' @include internal.R
setMethod("as.expression", signature("DGVMUnit"), function(x, ...) {
  x <- .as.char(x)
  x <- gsub(" ", "\u007E", x)
  return(as.expression(x))
  ## return(parse(text=x))
})
