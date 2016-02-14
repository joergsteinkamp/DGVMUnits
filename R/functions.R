#' Division of two unit strings
#' 
#' Divides two RVCUnit objects.
#'
#' @param a An RVCUnit object.
#' @param b An RVCUnit object.
#' @return A new RVCUnit object.
#'
#' @examples
#' x <- as.RVCUnit("kg/m2")
#' y <- as.RVCUnit("ha")
#' ret <- multiply(x,y)
#' as.character(ret)
#' @export
divide <- function(a,b) {
  class.def <- class(a)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "RVCUnit" && attr(class.def, "package") != "RVCUnits")
    stop("Input not class RVCUnits:RVCUnit")
  class.def <- class(b)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "RVCUnit" && attr(class.def, "package") != "RVCUnits")
    stop("Input not class RVCUnits:RVCUnit")

  rt <- new("RVCUnit")
  
  rt@g[2] = a@g[2] - b@g[2]
  rt@m[2] = a@m[2] - b@m[2]
  rt@W[2] = a@W[2] - b@W[2]
  rt@s[2] = a@s[2] - b@s[2]

  rt@g[1] = a@g[1]^a@g[2] / b@g[1]^b@g[2]
  rt@m[1] = a@m[1]^a@m[2] / b@m[1]^b@m[2]
  rt@W[1] = a@W[1]^a@W[2] / b@W[1]^b@W[2]
  rt@s[1] = a@s[1]^a@s[2] / b@s[1]^b@s[2]

  return(rt)
}

#' Multiplication of two RVCUnit objects
#' 
#' Multiplies two RVCUnit objects.
#' 
#' @param a An RVCUnit object.
#' @param b An RVCUnit object.
#' @return A new RVCUnit object.
#'
#' @examples
#' x <- as.RVCUnit("kg/m2")
#' y <- as.RVCUnit("kg m^-2 yr^-1")
#' ret <- divide(x,y)
#' as.character(ret)
#' @export
multiply <- function(a,b) {
  class.def <- class(a)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "RVCUnit" && attr(class.def, "package") != "RVCUnits")
    stop("Input not class RVCUnits:RVCUnit")
  class.def <- class(b)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "RVCUnit" && attr(class.def, "package") != "RVCUnits")
    stop("Input not class RVCUnits:RVCUnit")

  rt <- new("RVCUnit")
  
  rt@g[2] = a@g[2] + b@g[2]
  rt@m[2] = a@m[2] + b@m[2]
  rt@W[2] = a@W[2] + b@W[2]
  rt@s[2] = a@s[2] + b@s[2]

  rt@g[1] = a@g[1]^a@g[2] * b@g[1]^b@g[2]
  rt@m[1] = a@m[1]^a@m[2] * b@m[1]^b@m[2]
  rt@W[1] = a@W[1]^a@W[2] * b@W[1]^b@W[2]
  rt@s[1] = a@s[1]^a@s[2] * b@s[1]^b@s[2]

  return(rt)
}
