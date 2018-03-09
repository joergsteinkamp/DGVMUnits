#' Division of two unit strings
#' 
#' Divides two DGVMUnit objects.
#'
#' @param a An DGVMUnit object.
#' @param b An DGVMUnit object.
#' @return A new DGVMUnit object.
#'
#' @examples
#' x <- as.DGVMUnit("kg/m2")
#' y <- as.DGVMUnit("ha")
#' ret <- multiply(x,y)
#' as.character(ret)
#' @export
#' @include 500Methods.R
divide <- function(a,b) {
  class.def <- class(a)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  class.def <- class(b)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")

  if (a@K[1] != b@K[1])
    stop("Cannot divide Kelvin and Celsius!")
  
  rt <- new("DGVMUnit")
  
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

#' Multiplication of two DGVMUnit objects
#' 
#' Multiplies two DGVMUnit objects.
#' 
#' @param a An DGVMUnit object.
#' @param b An DGVMUnit object.
#' @return A new DGVMUnit object.
#'
#' @examples
#' x <- as.DGVMUnit("kg/m2")
#' y <- as.DGVMUnit("kg m^-2 yr^-1")
#' ret <- divide(x,y)
#' as.character(ret)
#' @export
#' @include 500Methods.R
multiply <- function(a,b) {
  class.def <- class(a)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  class.def <- class(b)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")

  if (a@K[1] != b@K[1])
    stop("Cannot divide Kelvin and Celsius!")
  
  rt <- new("DGVMUnit")
  
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
#' Comarison two DGVMUnit objects
#' 
#' Checks if two DGVMUnit objects are identical.
#' 
#' @param a An DGVMUnit object.
#' @param b An DGVMUnit object.
#' @return boolean
#'
#' @examples
#' x <- as.DGVMUnit("kg/m2")
#' y <- as.DGVMUnit("kg ha^-2")
#' ret <- equal(x,y)
#' @export
#' @include 500Methods.R
equal <- function(a,b) {
  class.def <- class(a)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  class.def <- class(b)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  
  if (a@g[1] == b@g[1] && a@g[2] == b@g[2] &&
      a@m[1] == b@m[1] && a@m[2] == b@m[2] &&
      a@W[1] == b@W[1] && a@W[2] == b@W[2] &&
      a@s[1] == b@s[1] && a@s[2] == b@s[2] &&
      a@K[1] == b@K[1] && a@K[2] == b@K[2])
    return(TRUE)
  return(FALSE)
}

#' Comarison two DGVMUnit objects
#' 
#' Checks if two DGVMUnit objects are comparable (only different in unit prefixes).
#' 
#' @param a An DGVMUnit object.
#' @param b An DGVMUnit object.
#' @return boolean
#'
#' @examples
#' x <- as.DGVMUnit("kg/m2")
#' y <- as.DGVMUnit("kg ha^-2")
#' ret <- comparable(x,y)
#' @export
#' @include 500Methods.R
comparable <- function(a,b) {
  class.def <- class(a)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  class.def <- class(b)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  
  if (a@g[2] == b@g[2] &&
      a@m[2] == b@m[2] &&
      a@W[2] == b@W[2] &&
      a@s[2] == b@s[2] &&
      a@K[2] == b@K[2])
    return(TRUE)
  return(FALSE)
}
