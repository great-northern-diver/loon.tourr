#' Display tour path with trails
#' @description A 2D tour path with trails
#' @inheritParams l_layer_hull
#' @param x The coordinates of x representing the current state
#' @param y The coordinates of y representing the current state
#' @param xpre the same length of \code{x} representing the last state
#' @param ypre the same length of \code{y} representing the last state
#' @param color the color of the trail
#' @export
#' @return an \code{l_layer} widget
#' @examples
#' if(interactive()) {
#' p <- l_tour(iris[, -5], color = iris$Species)
#' l <- l_layer_trails(p, color = "grey50")
#' }
l_layer_trails <- function(widget, x, y, xpre, ypre,
                           color = "black", linewidth = 1,
                           label = "trails", parent = "root", index = 0,
                           active = TRUE, ...) {

  widget <- l_getPlots(widget)

  loon::l_throwErrorIfNotLoonWidget(widget)

  if(label != "trails") {
    warning("The label ",
            deparse(substitute(label)),
            " is not `trails` so that this layer may not be interactive",
            call. = FALSE)
  }

  # inherits coords from widget
  if(missing(x)) x <- widget['x']
  if(missing(y)) y <- widget['y']

  if(missing(xpre)) xpre <- x
  if(missing(ypre)) ypre <- y

  stopifnot(
    exprs = {
      length(x) == length(xpre)
      length(y) == length(ypre)
      length(y) == length(x)
    }
  )

  len <- length(x)

  loon::l_layer_lines(
    widget,
    x = lapply(seq(len), function(i) c(xpre[i], x[i])),
    y = lapply(seq(len), function(i) c(ypre[i], y[i])),
    color = color,
    linewidth = linewidth,
    index = index,
    label = label,
    parent = parent,
    ...
  )
}
