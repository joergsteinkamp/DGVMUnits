#######################################################################
## Function to create a new unit element from strings #################
#######################################################################
#' Creates a DGVMUnit object from a unit string.
#' 
#' @param x A string or a vector of strings parsable as units.
#' @return An DGVMUnit object.
#' @examples
#' x <- as.DGVMUnit("kW h m-2")
#' units <- c("m2 s", "m2/s", "m^2*s", "C d")
#' x <- as.DGVMUnit(units)
#' @export
#' @importFrom udunits ud.is.parseable
#' @include classes.R 
#' @include internal.R
as.DGVMUnit <- function(x=NA) {
  if (all(is.na(x)))
    return(new("DGVMUnit"))
  
  if (!is.character(x))
    stop("Need characters/strings as input!")
  
  if (any(grepl('[()]', x)))
    stop("Bracket parsing not implemented!")
  
  ret <- sapply(x, function(x) {
    ## parse
    x <- .char.split(x)
    
    return(new('DGVMUnit',
               shortname  = x[['units']],
               longname   = rep(NA_character_, length(x)),
               scale      = x[['scale']],
               offset     = x[['offset']],
               exponent   = x[['exponent']],
               reference  = rep(NA_character_, length(x))
    ))
  })
  
  lapply(ret, function(x) {
    if (!ud.is.parseable(.as.char(x)))
      warning(paste0("Unit '", .as.char(x), "' is not parseable by udunits2!"))
    invisible(NULL)
  })
  
  if (length(ret) == 1)
    ret = ret[[1]]
  return(ret)
}

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
#' y <- as.DGVMUnit("kg m^-2 yr^-1")
#' ret <- divide(x,y)
#' as.character(ret)
#' @export
#' @include methods.R
divide <- function(a, b) {
  stop("Due to restructuring not yet working!")
  
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

  ## TODO: there is a problem here, if a unit is canceled out but was at different magnitudes
  sapply(slotNames(rt), function(x) {
    vals = slot(rt, x)
    if (vals[2] == 0 && vals[1] != 1)
      warning(paste0("slot '", x, "': ", vals[1]))
    return(invisible(NULL))
  })
  
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
#' y <- as.DGVMUnit("ha")
#' ret <- multiply(x,y)
#' as.character(ret)
#' @export
#' @include methods.R
multiply <- function(a, b) {
  stop("Due to restructuring not yet working!")
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

  ## TODO: there is a problem here, if a unit is canceled out but was at different magnitudes
  sapply(slotNames(rt), function(x) {
    vals = slot(rt, x)
    if (vals[2] == 0 && vals[1] != 1)
      warning(paste0("slot '", x, "': ", vals[1]))
    return(invisible(NULL))
    })
  
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
#' @importFrom udunits2 ud.are.convertible ud.is.parseable
#' @include methods.R
equal <- function(a, b) {
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

  if (!ud.is.parseable(.as.char(x)))
    stop(paste0("'", .as.char(x), "' not paeseable by udunits2!"))
  if (!ud.is.parseable(.as.char(y)))
    stop(paste0("'", .as.char(y), "' not paeseable by udunits2!"))
  
  if (ud.are.convertible(.as.char(x), .as.char(y)))
    if (ud.convert(1, .as.char(x), .as.char(y)) == 1)
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
#' @importFrom udunits2 ud.are.convertible
#' @include methods.R
comparable <- function(a, b) {
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
 
  if (ud.are.convertible(.as.char(x), .as.char(y)))
    return(TRUE)
  return(FALSE)
}
