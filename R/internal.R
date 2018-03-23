#######################################################################
## split a baseunit part into its components: #########################
## baseunit, scale factor, exponent           #########################
#######################################################################
## TODO: This needs to be extended by units not yet covered
##       as well as a reference e.g. kgC m^2(leaf)
#' @importFrom udunits2 ud.convert
#' @include classes.R
.baseunit <- function(x) {
  if (length(x) > 1 && !is.character(x))
    stop(paste("'x' is no character of has a length > 1:", x))
  
  ## parse units of charlength 1
  if (x == "h") {
    return(list("s", 3600, 0, 1))
  } else if (x == "d") {
    return(list("s", 86400, 0, 1))
  } else if (x == "l") {
    return(list("m", 1.e-3, 0, 3))
  } else if (x == "t") {
    return(list("g", 1.e6, 0, 1))
  } else if (x == "C" || x == "\u00B0C") {
    return(list("K", 1, 273.15, 1))
  } else if (nchar(x) == 1) {
    return(list(x, 1, 0, 1))
  }
  
  ## parse special strings
  if (x == "ha") {
    return(list("m", 100, 0, 2))
  } else if (x == "sec") {
    return(list("s", 1, 0, 1))
  } else if (x == "min") {
    return(list("s", 60, 0, 1))
  } else if (x == "day") {
    return(list("s", 86400, 0, 1))
  } else if (x == "yr") {
    return(list("s", ud.convert(1, "yr", "s"), 0, 1))
  }
  
  if (grepl('^[kMGTP]s', x))
    stop(paste0("The temporal unit '", x, "' does not make any sense. Use min, hour, day or yr instead!"))
  
  ## pico, nano, ..., Peta
  if (grepl("^p", x)) {
    return(list(sub('^p', '', x), 1.e-12, 0, 1))
  } else if (grepl("^n", x)) {
    return(list(sub('^n', '', x), 1.e-9, 0, 1))
  } else if (grepl("^\u00B5", x)) {
    return(list(sub('^\u00B5', '', x), 1.e-6, 0, 1))
  } else if (grepl("^m", x)) {
    return(list(sub('^m', '', x), 1.e-3, 0, 1))
    ##  } else if (grepl("^c", x)) {
    ##    return(list(sub('^c', '', x), 1.e-2, 1))
    ##  } else if (grepl("^da", x)) {
    ##    return(list(sub('^da', '', x), 1.e1, 1))
    ##  } else if (grepl("^d", x)) {
    ##    return(list(sub('^d', '', x), 1.e-1, 1))
    ##  } else if (grepl("^h", x)) {
    ##    return(list(sub('^h', '', x), 1.e2, 1))
  } else if (grepl("^k", x)) {
    return(list(sub('^k', '', x), 1.e3, 0, 1))
  } else if (grepl("^M", x)) {
    return(list(sub('^M', '', x), 1.e6, 0, 1))
  } else if (grepl("^G", x)) {
    return(list(sub('^G', '', x), 1.e9, 0, 1))
  } else if (grepl("^T", x)) {
    return(list(sub('^T', '', x), 1.e12, 0, 1))
  } else if (grepl("^P", x)) {
    return(list(sub('^P', '', x), 1.e15, 0, 1))
  } else {
    stop(paste("Sh..! That unit is not yet implemented:", x))
  }
}

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
  exponent <- list()
  scale    <- list()
  offset   <- list()
  for (i in 1:length(units)) {
    exponent[[i]] <- as.numeric(gsub('[\u00B5\u00B0a-zA-Z]', '', units[[i]]))
    exponent[[i]][is.na(exponent[[i]])] = 1
    units[[i]]  <- gsub('[0-9\\-]', '', units[[i]])
    scale[[i]]  <- rep(NA, length(units[[i]]))
    offset[[i]] <- rep(NA, length(units[[i]]))
    for (j in 1:length(units[[i]])) {
      baseunit <- .baseunit(units[[i]][j])
      units[[i]][j] = baseunit[[1]]
      scale[[i]][j] = baseunit[[2]]
      offset[[i]][j] = baseunit[[3]]
      exponent[[i]][j] = exponent[[i]][j] * baseunit[[4]]
      ## TODO: check this!
      ##prefix[[i]][j] = prefix[[i]][j]^exponent[[i]][j]
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
        exponent[[i]][j+1] = -1 * exponent[[i]][j+1]
    }
  }

  return(list(units=unlist(units), offset=unlist(offset), scale=unlist(scale), exponent=unlist(exponent)))
}

#######################################################################
## return a string, which is udunits/CF conform #######################
#######################################################################
#' @importFrom udunits2 ud.convert
#' @include classes.R
## TODO: make use of slot reference as index or in brackets for pretty printing
.as.char <- function(x) {
  class.def <- class(x)
  if (is.null(attr(class.def, "package")))
    stop("Input seems not to be a class.")
  if (class.def[1] != "DGVMUnit" && attr(class.def, "package") != "DGVMUnits")
    stop("Input not class DGVMUnits::DGVMUnit")
  
  # TODO: handle a list of DGVMUnits
  
  for (i in 1:length(x@shortname)) {
    if (x@shortname[i] == "s") {
      if (x@scale[i] == 60) {
        x@shortname[i] <- "min"
      } else if (x@scale[i] == 3600) {
        x@shortname[i] <- "h"
      } else if (x@scale[i] == 86400) {
        x@shortname[i] <- "d"
      } else if (x@scale[i] == ud.convert(1, "yr", "s")) {
        x@shortname[i] <- "yr"
      }
    } else if (x@shortname[i] == "K" && x@offset[i] == 273.15) {
      x@shortname[i] <- "\u00B0C"
    }

    if (x@scale[i] == 1.e-12) {
      x@shortname[i] <- paste0("p", x@shortname[i])
    } else if (x@scale[i] == 1.e-9) {
      x@shortname[i] <- paste0("n", x@shortname[i])
    } else if (x@scale[i] == 1.e-6) {
      x@shortname[i] <- paste0("\u00B5", x@shortname[i])
    } else if (x@scale[i] == 1.e-3) {
      x@shortname[i] <- paste0("m", x@shortname[i])
    } else if (x@scale[i] == 1.e3) {
      x@shortname[i] <- paste0("k", x@shortname[i])
    } else if (x@scale[i] == 1.e6) {
      x@shortname[i] <- paste0("M", x@shortname[i])
    } else if (x@scale[i] == 1.e9) {
      x@shortname[i] <- paste0("G", x@shortname[i])
    } else if (x@scale[i] == 1.e12) {
      x@shortname[i] <- paste0("T", x@shortname[i])
    } else if (x@scale[i] == 1.e15) {
      x@shortname[i] <- paste0("P", x@shortname[i])
    }
  }

  ## TODO: make this optional, or with a predefined order
  ## sort with decreasing exponents
  ordered <- order(x@exponent, decreasing=TRUE)

  ret <- paste(x@shortname[ordered], x@exponent[ordered], sep="\u005E", collapse=" ")
  return(gsub("\\\u005E1", "", ret))
}

## sprintf("%X", as.integer(charToRaw("^"))) => 5E
## paste0("\u005E") => "^"
## sprintf("%X", as.integer(charToRaw("*"))) => 2A
## paste0("\u002A") => "*"
## sprintf("%X", as.integer(charToRaw("°"))) => C2 B0
## paste0("\u00B0")
