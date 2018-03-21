#######################################################################
## Internal function to split a string into its unit components #######
#######################################################################
#' @include classes.R
.char.split <- function(x) {
  ## remove the hat
  x <- gsub('\\\u005E', '', x)
  ## replace the multiplication sign by a space
  x <- gsub('\\\u002A', ' ', x)
  units <- strsplit(x, '[ /]')

  ## get the exponents and unit prefixes
  exponents <- list()
  prefix    <- list() 
  for (i in 1:length(units)) {
    exponents[[i]] <- as.numeric(gsub('[\u00B5\u00B0a-zA-Z]', '', units[[i]]))
    exponents[[i]][is.na(exponents[[i]])] = 1
    units[[i]] <- gsub('[0-9\\-]', '', units[[i]])
    prefix[[i]] <- rep(NA, length(units[[i]]))
    for (j in 1:length(units[[i]])) {
      baseunit <- .baseunit(units[[i]][j])
      units[[i]][j] = baseunit[[1]]
      prefix[[i]][j] = baseunit[[2]]
      exponents[[i]][j] = exponents[[i]][j] * baseunit[[3]]
      ## TODO: check this!
      ##prefix[[i]][j] = prefix[[i]][j]^exponents[[i]][j]
    }
  }

  ## get the division sign and multiply exponent by -1
  operators <- gsub('[\u00B5\u00B0a-zA-Z0-9\\-]*', '', x)
  operators <- strsplit(operators, '', perl=TRUE)

  for (i in 1:length(units)) {
    if (!length(operators[[i]]))
      break
    for (j in 1:length(operators[[i]])) {
      if (operators[[i]][j] == "/")
        exponents[[i]][j+1] = -1 * exponents[[i]][j+1]
    }
  }

  return(list(units=units, prefix=prefix, exponents=exponents))
}

#######################################################################
## split a baseunit part into its components: #########################
## baseunit, scale factor, exponent           #########################
#######################################################################
#' @importFrom udunits2 ud.convert
#' @include classes.R
.baseunit <- function(x) {
  if (length(x) > 1 && !is.character(x))
    stop(paste("'x' is no character of has a length > 1:", x))

  ## parse units of charlength 1
  if (x == "h") {
    return(list("s", 3600, 1))
  } else if (x == "d") {
    return(list("s", 86400, 1))
  } else if (x == "l") {
    return(list("m", 1.e-3, 3))
  } else if (x == "t") {
    return(list("g", 1.e6, 1))
  } else if (x == "C" || x == "\u00B0C") {
    return(list("K", 273.15, 1))
  } else if (nchar(x) == 1) {
    return(list(x, 1, 1))
  }

  ## parse special strings
  if (x == "ha") {
    return(list("m", 100, 2))
  } else if (x == "sec") {
    return(list("s", 1, 1))
  } else if (x == "min") {
    return(list("s", 60, 1))
  } else if (x == "day") {
    return(list("s", 86400, 1))
  } else if (x == "yr") {
    return(list("s", ud.convert(1, "yr", "s"), 1))
  }

  if (grepl('^[kMGTP]s', x))
    stop(paste0("The temporal unit '", x, "' does not make any sense. Use min, hour, day or yr instead!"))

  ## pico, nano, ..., Peta
  if (grepl("^p", x)) {
    return(list(sub('^p', '', x), 1.e-12, 1))
  } else if (grepl("^n", x)) {
    return(list(sub('^n', '', x), 1.e-9, 1))
  } else if (grepl("^\u00B5", x)) {
    return(list(sub('^\u00B5', '', x), 1.e-6, 1))
  } else if (grepl("^m", x)) {
    return(list(sub('^m', '', x), 1.e-3, 1))
##  } else if (grepl("^c", x)) {
##    return(list(sub('^c', '', x), 1.e-2, 1))
##  } else if (grepl("^da", x)) {
##    return(list(sub('^da', '', x), 1.e1, 1))
##  } else if (grepl("^d", x)) {
##    return(list(sub('^d', '', x), 1.e-1, 1))
##  } else if (grepl("^h", x)) {
##    return(list(sub('^h', '', x), 1.e2, 1))
  } else if (grepl("^k", x)) {
    return(list(sub('^k', '', x), 1.e3, 1))
  } else if (grepl("^M", x)) {
    return(list(sub('^M', '', x), 1.e6, 1))
  } else if (grepl("^G", x)) {
    return(list(sub('^G', '', x), 1.e9, 1))
  } else if (grepl("^T", x)) {
    return(list(sub('^T', '', x), 1.e12, 1))
  } else if (grepl("^P", x)) {
    return(list(sub('^P', '', x), 1.e15, 1))
  } else {
    stop(paste("Sh..! That should not have happened:", x))
  }
}

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
#' @include classes.R
as.DGVMUnit <- function(x=NA) {
  if (all(is.na(x)))
    return(new("DGVMUnit"))

  if (!is.character(x))
    stop("Need characters/strings as input!")

  if (any(grepl('[()]', x)))
    stop("Bracket parsing not implemented!")

  ## parse
  x <- .char.split(x)

  if (length(x[[1]]) == 1) {
    pstr <- paste(x[[1]][[1]], "=c(", x[[2]][[1]], ", ", x[[3]][[1]],")", sep="", collapse=", ")
    return(eval(parse(text=paste("new('DGVMUnit',", pstr, ")", sep=""))))
  }

  rt <- list()
  for (i in 1:length(x[[1]])) {
    pstr <- paste(x[[1]][[i]], "=c(", x[[2]][[i]], ", ", x[[3]][[i]],")", sep="", collapse=", ")
    rt[[i]] <- eval(parse(text=paste("new('DGVMUnit',", pstr, ")", sep="")))
  }

  return(rt)
}

#######################################################################
## return a string, which is udunits/CF conform #######################
#######################################################################
#' @importFrom udunits2 ud.convert
#' @include classes.R
.as.char <- function(x) {
  class.def <- class(x)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits:DGVMUnit")
  
  y = list(g=x@g, m=x@m, K=x@K, W=x@W, s=x@s)
  ## remove unused unit parts
  if (y[['g']][2] == 0)
    y[['g']] <- NULL
  if (y[['m']][2] == 0)
    y[['m']] <- NULL
  if (y[['s']][2] == 0)
    y[['s']] <- NULL
  if (y[['W']][2] == 0)
    y[['W']] <- NULL
  if (y[['K']][2] == 0)
    y[['K']] <- NULL

  for (i in 1:length(names(y))) {
    if (!is.null(y[[i]])) {
      if (names(y)[i] == "s") {
        if (y[[i]][1] == 60) {
          names(y)[i] <- "min"
        } else if (y[[i]][1] == 3600) {
          names(y)[i] <- "h"
        } else if (y[[i]][1] == 86400) {
          names(y)[i] <- "d"
        } else if (y[[i]][1] == ud.convert(1, "yr", "s")) {
          names(y)[i] <- "yr"
        }
      } else if (names(y)[i] == "K" && y[[i]][1] == 273.15) {
        names(y)[i] <- "\u00B0C"
      }

      if (y[[i]][1] == 1.e-12) {
        names(y)[i] <- paste("p", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e-9) {
        names(y)[i] <- paste("n", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e-6) {
        names(y)[i] <- paste("\u00B5", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e-3) {
        names(y)[i] <- paste("m", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e3) {
        names(y)[i] <- paste("k", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e6) {
        names(y)[i] <- paste("M", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e9) {
        names(y)[i] <- paste("G", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e12) {
        names(y)[i] <- paste("T", names(y)[i], sep="")
      } else if (y[[i]][1] == 1.e15) {
        names(y)[i] <- paste("P", names(y)[i], sep="")
      } 
    }
  }
  ## sort with decreasing exponents
  exponents <- sapply(y ,"[[", 2)
  exponents <- sort(exponents, decreasing=TRUE)

  rt <- paste(names(exponents), exponents, sep="\u005E", collapse=" ")
  return(gsub("\\\u005E1", "", rt))
}

## sprintf("%X", as.integer(charToRaw("^"))) => 5E
## paste0("\u005E") => "^"
## sprintf("%X", as.integer(charToRaw("*"))) => 2A
## paste0("\u002A") => "*"
## sprintf("%X", as.integer(charToRaw("°"))) => C2 B0
## paste0("\u00B0")
