setMethod("as.character", signature("RVCUnit"), function(x, ...) {
  return(.as.char(x))
})

setMethod("as.expression", signature("RVCUnit"), function(x, ...) {
  x <- .as.char(x)
  x <- gsub(" ", "\u007E", x)
  return(parse(text=x))
})
