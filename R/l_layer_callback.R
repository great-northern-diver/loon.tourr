#' @title Tour Layer Configuration
#' @description Mainly used in the 2D (or 1D) tour interactive layer configuration
#' @param target either a `l_tour` object or a loon widget
#' @param layer the layer need to be modified
#' @param ... some useful info for the layer configuration (i.e. tours, projections, etc)
#' @details It is a \code{S3} method. The object class is determined by the layer **label**
#' @export
#'
#' @return this callback function does not return any object. As the
#' slider bar is scrolled, for the specified layer, the callback function
#' will be fired and the layer will be configured.
#'
#' @examples
#' if(interactive() && requireNamespace("tourr")) {
#'   # 1D tour
#'   p <- l_tour(iris[, -5], tour = tourr::grand_tour(1L))
#'   # add layer density
#'   l <- l_layer(l_getPlots(p),
#'                stats::density(p['x']),
#'                label = "density")
#'
#'   # as we scroll the bar, the density curve does not change
#'   # unless the following function is executed
#'   l_layer_callback.density <- function(target, layer, ...) {
#'
#'       layer <- loon::l_create_handle(c(l_getPlots(target), layer))
#'       den <- stats::density(target['x'])
#'
#'       loon::l_configure(layer,
#'                         x = den$x,
#'                         y = den$y)
#'
#'       invisible()
#'   }
#' }
#'
l_layer_callback <- function(target, layer, ...) {
  widget <- l_getPlots(target)
  layer <- loon::l_create_handle(c(widget, layer))
  label <- loon::l_layer_getLabel(widget, layer)
  class(label) <- label
  UseMethod("l_layer_callback", label)
}

#' @export
l_layer_callback.default <- function(target, layer, ...) {
  NULL
}

#' @export
l_layer_callback.guides <- function(target, layer, ...) {
  NULL
}

#' @export
l_layer_callback.density2d <- function(target, layer, ...) {

  widget <- l_getPlots(target)
  layer <- loon::l_create_handle(c(widget, layer))
  # is layer visible?
  isVisible <- loon::l_layer_isVisible(widget, layer)
  x <- widget['x']
  y <- widget['y']

  group <- list(...)$group

  density2d_configure <- function(widget, layer, x, y, parent, label = "density2d", isVisible = TRUE) {

    if(is.null(x) || is.null(y)) return(NULL)

    den2d <- density2d_xyz(x, y)
    lines <- grDevices::contourLines(x=den2d$x, y=den2d$y, z=den2d$z)
    len <- length(lines)

    # in density 2d configuration,
    # we remove the target (`l_layer_lines`) and create new target
    # the reason is that as the points change
    # the number of lines vary, if we configure, error occur
    # i.e. Suppose we had 5 density 2d lines, after a random tour, the new density 2d only contains 3 lines.
    # Then, l_configure will configure the three lines rather than the 5. Error will occur.

    # record the color and line width
    color <- layer['color']
    linewidth <- layer['linewidth']
    # remove the old density layer

    loon::l_layer_expunge(widget, layer)

    rep_len2 <- function(x, length.out) {
      if(length(x) == 0) return(NULL)
      lenx <- length(x)
      if(lenx == length.out) return(x)
      if(lenx > length.out) return(x[seq(length.out)])
      c(x, rep(x[lenx], length.out - lenx))
    }

    l <- do.call(
      loon::l_layer_lines,
      remove_null(widget = widget,
                  x = lapply(lines, FUN=function(line) line$x),
                  y = lapply(lines, FUN=function(line) line$y),
                  color = rep_len2(color, length.out = len),
                  linewidth = rep_len2(linewidth, length.out = len),
                  label = label,
                  parent = parent)
    )

    if(!isVisible)
      loon::l_layer_hide(widget, l)
  }


  # grouped density
  children <- rev(loon::l_layer_getChildren(widget, layer))

  if(length(children) == 1) {
    density2d_configure(widget,
                        layer = loon::l_create_handle(c(widget, children[1])),
                        x,
                        y,
                        parent = layer,
                        label = "density2d",
                        isVisible = isVisible)
  } else {
    x <- split(x, f = group)
    y <- split(y, f = group)

    lapply(seq(length(children)),
           function(i) {
             density2d_configure(widget,
                                 layer = loon::l_create_handle(c(widget, children[i])),
                                 x[[i]],
                                 y[[i]],
                                 parent = layer,
                                 label = paste("density2d", i),
                                 isVisible = isVisible)
           })
  }

  invisible()
}

#' @export
l_layer_callback.hull <- function(target, layer, ...) {

  widget <- l_getPlots(target)
  layer <- loon::l_create_handle(c(widget, layer))
  x <- widget['x']
  y <- widget['y']

  group <- list(...)$group

  if(inherits(layer, "l_layer_lines")) {
    # grouped hull
    x <- split(x, f = group)
    y <- split(y, f = group)
    xy <- hull_xy(x, y)
  } else {
    # hull
    xy <- hull_xy(x, y)
  }
  do.call(loon::l_configure, c(list(target = layer), xy))

  invisible()
}

#' @export
l_layer_callback.trails <- function(target, layer, ...) {
  widget <- l_getPlots(target)
  layer <- loon::l_create_handle(c(widget, layer))

  args <- list(...)
  tours <- args$tours
  var <- args$var
  varOld <- args$varOld
  start <- args$start
  data <-  args$data

  if(var == 0 || varOld == 0) return(NULL)

  xnew <- tours[[var]][, 1]
  ynew <- tours[[var]][, 2]

  xpre <- tours[[varOld]][, 1]
  ypre <- tours[[varOld]][, 2]

  stopifnot(
    exprs = {
      length(xpre) == length(xnew)
    }
  )

  len <- length(xnew)
  loon::l_configure(
    layer,
    x = lapply(seq(len), function(i) c(xpre[i], xnew[i])),
    y = lapply(seq(len), function(i) c(ypre[i], ynew[i]))
  )

  invisible()
}
