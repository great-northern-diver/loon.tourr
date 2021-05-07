l_path_name <- function(x, child) {

  if(length(x) == 0) return(character(0L))

  if(inherits(x, "l_tour") || inherits(x, "l_tour_compound")) {
    attrName <- attributes(x)$names
    return(attrName[!attrName %in% "projection"])
  }

  if (loon::l_isLoonWidget(x))
    return(as.character(x))


  if (inherits(x, "list") && l_isCompound(x)) {

    # maybe a compound?
    isSameParent <- vapply(x,
                           function(p) {
                             grepl(as.character(child), as.character(p))
                           }, logical(1L))
    if(all(isSameParent)) return(child)
  }

  character(0L)
}
