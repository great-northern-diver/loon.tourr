l_isTour <- function(target) {
  UseMethod("l_isTour", target)
}

l_isTour.default <- function(target) {
  FALSE
}

l_isTour.l_tour <- function(target) {
  plot_name <- setdiff(names(unclass(target)), "projection")
  plot <- target[[plot_name]]
  loon::l_isLoonWidget(plot)
}

l_isTour.l_tour_compound <- function(target) {
  plot_name <- setdiff(names(unclass(target)), "projection")
  plot <- target[[plot_name]]
  l_isCompound(plot)

}

l_isCompound <- function(target) {

  if(is.atomic(target))
    target <- tryCatch(l_getFromPath(target),
                       error = function(e) character(0L))

  if(length(target) == 0L)
    return(FALSE)

  all(vapply(target,
         function(p) {
           loon::l_isLoonWidget(p)
         },
         logical(1L)))
}

