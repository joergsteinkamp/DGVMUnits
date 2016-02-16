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
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
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

  if (a@K[1]!=b@K[1])
    stop("Cannot divide Kelvin and Celsius!")
  
  rt <- new("RVCUnit")
  
  rt@g[2] = a@g[2] - b@g[2]
  rt@m[2] = a@m[2] - b@m[2]
  rt@W[2] = a@W[2] - b@W[2]
  rt@s[2] = a@s[2] - b@s[2]
  rt@K[2] = a@K[2] - b@K[2]
  
  rt@g[1] = a@g[1]^a@g[2] / b@g[1]^b@g[2]
  rt@m[1] = a@m[1]^a@m[2] / b@m[1]^b@m[2]
  rt@W[1] = a@W[1]^a@W[2] / b@W[1]^b@W[2]
  rt@s[1] = a@s[1]^a@s[2] / b@s[1]^b@s[2]
  rt@K[1] = a@K[1]
  
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
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
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

  if (a@K[1]!=b@K[1])
    stop("Cannot divide Kelvin and Celsius!")
  
  rt <- new("RVCUnit")
  
  rt@g[2] = a@g[2] + b@g[2]
  rt@m[2] = a@m[2] + b@m[2]
  rt@W[2] = a@W[2] + b@W[2]
  rt@s[2] = a@s[2] + b@s[2]
  rt@K[2] = a@K[2] + b@K[2]

  rt@g[1] = a@g[1]^a@g[2] * b@g[1]^b@g[2]
  rt@m[1] = a@m[1]^a@m[2] * b@m[1]^b@m[2]
  rt@W[1] = a@W[1]^a@W[2] * b@W[1]^b@W[2]
  rt@s[1] = a@s[1]^a@s[2] * b@s[1]^b@s[2]
  rt@K[1] = a@K[1]
  
  return(rt)
}
#' Comarison two RVCUnit objects
#' 
#' Checks if two RVCUnit objects are identical.
#' 
#' @param a An RVCUnit object.
#' @param b An RVCUnit object.
#' @return boolean
#'
#' @examples
#' x <- as.RVCUnit("kg/m2")
#' y <- as.RVCUnit("kg ha^-2")
#' ret <- equal(x,y)
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
equal <- function(a,b) {
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
  
  if (a@g[1]==b@g[1] && a@g[2]==b@g[2] &&
      a@m[1]==b@m[1] && a@m[2]==b@m[2] &&
      a@W[1]==b@W[1] && a@W[2]==b@W[2] &&
      a@s[1]==b@s[1] && a@s[2]==b@s[2] &&
      a@K[1]==b@K[1] && a@K[2]==b@K[2])
    return(TRUE)
  return(FALSE)
}

#' Comarison two RVCUnit objects
#' 
#' Checks if two RVCUnit objects are comparable (only different in unit prefixes).
#' 
#' @param a An RVCUnit object.
#' @param b An RVCUnit object.
#' @return boolean
#'
#' @examples
#' x <- as.RVCUnit("kg/m2")
#' y <- as.RVCUnit("kg ha^-2")
#' ret <- comparable(x,y)
#' @export
#' @author Joerg Steinkamp \email{joerg.steinkamp@@senckenberg.de}
comparable <- function(a,b) {
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
  
  if (a@g[2]==b@g[2] &&
      a@m[2]==b@m[2] &&
      a@W[2]==b@W[2] &&
      a@s[2]==b@s[2] &&
      a@K[2]==b@K[2])
    return(TRUE)
  return(FALSE)
}