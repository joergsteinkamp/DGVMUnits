#' CF-conform unit string.
#'
#' Returns a String from a RVCUnit object, which can be used e.g. in ud.convert of package udunits2.
#'
#' @param x An RVCUnit object.
#' @param ... Ignored further arguments.
#' @return a character string
#'
#' @examples
#' x <- as.RVCUnit("kW h m-2")
#' as.character(x)
#' units <- c("m2 s", "m2/s", "m^2*s", "C d")
#' x <- as.RVCUnit(units)
#' sapply(x, as.character)
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
setMethod("as.character", signature("RVCUnit"), function(x, ...) {
  return(.as.char(x))
})

#' Expression for labels.
#'
#' Returns an expression from a RVCUnit object, which can be used e.g. as axis label.
#'
#' @param x An RVCUnit object.
#' @param ... Ignored further arguments.
#' @return an expression
#'
#' @examples
#' x <- as.RVCUnit("kW h m-2")
#' as.expression(x)
#' units <- c("m2 s", "m2/s", "m^2*s", "C d")
#' x <- as.RVCUnit(units)
#' sapply(x, as.expression)
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
setMethod("as.expression", signature("RVCUnit"), function(x, ...) {
  x <- .as.char(x)
  x <- gsub(" ", "\u007E", x)
  return(parse(text=x))
})
