#' @title Query a loon widget
#' @description A generic function to query the \code{loon} (tcl) widget from the given target
#' @param target a \code{loon} object
#' @examples
#' if(interactive()) {
#'   p <- l_tour(iris[, -5])
#'   l_isLoonWidget(p) # FALSE
#'   q <- l_getPlots(p)
#'   l_isLoonWidget(q) # TRUE
#'
#'   # `l_compound` widget
#'   p <- l_tour_pairs(tourr::flea[, -7])
#'   l_isLoonWidget(p) # FALSE
#'   q <- l_getPlots(p)
#'   l_isLoonWidget(q) # FALSE
#'   is(q, "l_compound") # TRUE
#' }
#' @export
l_getPlots.l_tour <- function(target) {

  target <- unclass(target)
  plot_name <- setdiff(names(target), "projection")
  widget <- target[[plot_name]]
  loon::l_throwErrorIfNotLoonWidget(widget)

  return(widget)
}

#' @export
l_getPlots.l_tour_compound <- function(target) {

  target <- unclass(target)
  plot_name <- setdiff(names(target), "projection")
  target[[plot_name]]
}


#' @export
l_getPlots.l_compound <- function(target) {

  lapply(target,
         function(w) {
           loon::l_throwErrorIfNotLoonWidget(w)
         })
  target
}


