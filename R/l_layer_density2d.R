#' 2D density layer
#' @import MASS
#' @description Two-dimensional kernel density estimation with an axis-aligned bivariate normal kernel
#' @inheritParams l_layer_hull
#' @inheritParams MASS::kde2d
#' @inheritParams loon::l_layer_contourLines
#' @export
#' @examples
#' if(interactive()) {
#' p <- l_plot(iris, color = iris$Species)
#' l <- l_layer_density2d(p)
#' }
l_layer_density2d <- function(widget, x, y, h, n = 25L, lims = NULL,
                              color = "black", linewidth = 1, nlevels = 10,
                              levels = NULL,
                              label = "density2d", parent = "root", index = 0,
                              group = NULL, active = TRUE, ...) {

  widget <- l_getPlots(widget)

  loon::l_throwErrorIfNotLoonWidget(widget)

  if(label != "density2d") {
    warning("The label ",
            deparse(substitute(label)),
            " is not `density2d` so that this layer may not be interactive",
            call. = FALSE)
  }

  # inherits coords from widget
  if(missing(x)) x <- widget['x']
  if(missing(y)) y <- widget['y']

  if(!is.list(x)) {
    if(is.null(group)) group <- rep(1, length(x))
    unigroup <- unique(group)
    x <- lapply(unigroup,
                function(g) {
                  x[group == g]
                })
  }

  if(!is.list(y)) {
    if(is.null(group)) group <- rep(1, length(y))
    unigroup <- unique(group)
    y <- lapply(unique(group),
                function(g) {
                  y[group == g]
                })
  }

  len <- length(x)
  if(length(color) != len)
    color <- rep(color, len)

  if(length(linewidth) != len)
    linewidth <- rep(linewidth, len)

  # density 2D
  den2d <- density2d_xyz(x, y, h, n, lims)

  den <- loon::l_layer_group(widget, label = label,
                             index = index, parent = parent)

  if(len == 1) {

    loon::l_layer_contourLines(
      widget,
      x = den2d$x,
      y = den2d$y,
      z = den2d$z,
      color = color,
      linewidth = linewidth,
      nlevels = nlevels,
      levels = levels %||% pretty(range(den2d$z, na.rm = TRUE), nlevels),
      parent = den,
      ...
    )
  } else {

    lapply(seq(len),
           function(i) {
             loon::l_layer_contourLines(
               widget,
               x = den2d[[i]]$x,
               y = den2d[[i]]$y,
               z = den2d[[i]]$z,
               color = color[i],
               linewidth = linewidth[i],
               nlevels = nlevels,
               levels = levels %||% pretty(range(den2d[[i]]$z, na.rm = TRUE), nlevels),
               parent = den,
               ...
             )
           })
  }

  den
}

density2d_xyz <- function(x, y, h, n = 25L, lims = NULL) {

  stopifnot(
    exprs = {
      class(x) == class(y)
      length(x) == length(y)
    }
  )

  default_binwidth <- function(h, x, y) {
    if(missing(h)) h <- NULL

    h <- h %||% c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
    if(any(h <= 0)) h[which(h <= 0)] <- 1e-6

    h
  }

  if(is.list(x)) {

    density2d <- c()
    len <- length(x)
    for(i in seq(len)) {

      density2d[[i]] <- MASS::kde2d(x[[i]], y[[i]],
                                    default_binwidth(h, x[[i]], y[[i]]), n,
                                    lims = lims %||% c(grDevices::extendrange(x[[i]]),
                                                       grDevices::extendrange(y[[i]])))
    }

    if(len == 1)
      density2d <- density2d[[1]]

  } else {
    density2d <- MASS::kde2d(x, y,
                             default_binwidth(h, x, y), n,
                             lims = lims %||% c(grDevices::extendrange(x),
                                                grDevices::extendrange(y)))
  }

  density2d
}
