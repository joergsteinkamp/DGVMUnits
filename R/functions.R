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
#' @importFrom udunits2 ud.is.parseable
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

##############################
## Cancel equal components ###
##############################
## helper function, to handle NAs in the reference slot like values
.equal.reference <- function(a, b) {
  if (is.na(a) && !is.na(b)) {
    return(FALSE)
  } else if (!is.na(a) && is.na(b)) {
    return(FALSE)
  } else if (all(!is.na(c(a, b)))) {
    if (a != b)
      return(FALSE)
  }
  return(TRUE)
}

#' Cancel equal unit parts
#'
#' @param x a \code{\linkS4class{DGVMUnit}} object
#'
#' @return a \code{\linkS4class{DGVMUnit}} object
#' @export
#'
#' @examples
#' x <- as.DGVMUnit("kg m s^-1 km^-2")
#' as.character(cancel(x))
cancel <- function(x) {
  class.def <- class(x)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  
  if (length(x@shortname) == 1)
    return(x)
  
  canceled <- 0
  ret <- new("DGVMUnit")
  for (i in 1:(length(x@shortname) - 1)) {
    for (j in (i+1):length(x@shortname)) {
      if (x@shortname[i] == x@shortname[j] && 
          .equal.reference(x@reference[i], x@reference[j])) {
        canceled = append(canceled, c(i, j))
        print(paste("canceling",i, j))
        ret@shortname = append(ret@shortname, x@shortname[j])
        ret@longname  = append(ret@longname,  x@longname[j])
        ret@scale     = append(ret@scale,     x@scale[i]^x@exponent[i] * x@scale[j]^x@exponent[i])
        ## TODO: find out how to handle offsets
        if (x@offset[i] != x@offset[j])
          stop("Don't know how to handle different offsets!")
        ret@offset    = append(ret@offset,    x@offset[j])
        ret@exponent  = append(ret@exponent,  x@exponent [i] + x@exponent[j])
        ret@reference = append(ret@reference, x@reference[j])
      } else if (!any(canceled == j) && i == j - 1 && j == length(x@shortname)) {
        ret@shortname = append(ret@shortname, x@shortname[j])
        ret@longname  = append(ret@longname,  x@longname[j])
        ret@scale     = append(ret@scale,     x@scale[j])
        ret@offset    = append(ret@offset,    x@offset[j])
        ret@exponent  = append(ret@exponent,  x@exponent[j])
        ret@reference = append(ret@reference, x@reference[j])
      }
    }
    if (!any(canceled == i)) {
      ret@shortname = append(ret@shortname, x@shortname[i])
      ret@longname  = append(ret@longname,  x@longname[i])
      ret@scale     = append(ret@scale,     x@scale[i])
      ret@offset    = append(ret@offset,    x@offset[i])
      ret@exponent  = append(ret@exponent,  x@exponent[i])
      ret@reference = append(ret@reference, x@reference[i])
    }
  }
  return(ret)
}

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
  
  a <- cancel(a)
  b <- cancel(b)
  for (i in 1:length(b@shortname)) 
    b@exponent[i] = -b@exponent[i]
  cancel(as.DGVMUnit(paste(.as.char(a), .as.char(b))))
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
  
  a <- cancel(a)
  b <- cancel(b)
  cancel(as.DGVMUnit(paste(.as.char(a), .as.char(b))))
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

  if (!ud.is.parseable(.as.char(a)))
    stop(paste0("'", .as.char(a), "' not paeseable by udunits2!"))
  if (!ud.is.parseable(.as.char(b)))
    stop(paste0("'", .as.char(b), "' not paeseable by udunits2!"))
  
  if (ud.are.convertible(.as.char(a), .as.char(b)))
    if (ud.convert(1, .as.char(a), .as.char(b)) == 1)
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
 
  if (ud.are.convertible(.as.char(a), .as.char(b)))
    return(TRUE)
  return(FALSE)
}
