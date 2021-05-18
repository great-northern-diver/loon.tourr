#' Layer a hull for \code{loon}
#' @description Creates a layer which is the subset of points lying on the hull (convex or alpha) of the set of points specified.
#' @param widget `loon` widget path name as a string
#' @param x The coordinates of \code{x}. See details
#' @param y The coordinates of \code{y}. See details
#' @param color the line color of each hull
#' @param linewidth the line width
#' @param label label used in the layers inspector
#' @param parent parent group layer
#' @param index of the newly added layer in its parent group
#' @param group separate x vector or y vector into a list by group
#' @param active a logical determining whether points appear or not (default is \code{TRUE} for all points).
#' If a logical vector is given of length equal to the number of points,
#' then it identifies which points appear (\code{TRUE}) and which do not (\code{FALSE}).
#' @param ... other arguments to modify \code{l_layer_line}.
#' @details Coordinates: the \code{x} or \code{y} can be a list or a vector.
#' \itemize{
#'   \item {If they are vectors, the argument \code{group} will be used to set the groups.}
#'   \item {If they are not provided, the \code{x} will be inherited from the widget}
#' }
#'
#' @return an \code{l_layer} widget
#'
#' @importFrom grDevices chull
#' @export
#' @examples
#' if(interactive()) {
#' p <- l_plot(iris, color = iris$Species)
#' l <- l_layer_hull(p, group = iris$Species)
#' }
#'
l_layer_hull <- function(widget, x, y, color = "black", linewidth = 1,
                         label = "hull", parent = "root", index = 0,
                         group = NULL, active = TRUE, ...) {

  widget <- l_getPlots(widget)

  loon::l_throwErrorIfNotLoonWidget(widget)

  if(label != "hull") {
    warning("The label ",
            deparse(substitute(label)),
            " is not `hull` so that this layer may not be interactive",
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

  # hull x and y
  xy <- hull_xy(x, y)

  hull <- if(len == 1) {
    loon::l_layer_line(
      widget,
      x = xy$x,
      y = xy$y,
      color = color,
      linewidth = linewidth,
      parent = parent,
      index = index,
      label = label,
      ...
    )
  } else {
    loon::l_layer_lines(
      widget,
      x = xy$x,
      y = xy$y,
      color = color,
      linewidth = linewidth,
      parent = parent,
      index = index,
      label = label,
      ...
    )
  }

  hull
}

hull_xy <- function(x, y) {

  stopifnot(
    exprs = {
      class(x) == class(y)
      length(x) == length(y)
    }
  )

  if(is.list(x)) {

    len <- length(x)

    xx <- c()
    yy <- c()

    for(i in seq(len)) {
      ch <- grDevices::chull(x[[i]], y[[i]])
      xx[[i]] <- x[[i]][c(ch, ch[1])]
      yy[[i]] <- y[[i]][c(ch, ch[1])]
    }

    if(len == 1) {
      xx <- xx[[1]]
      yy <- yy[[1]]
    }

  } else {
    ch <- grDevices::chull(x, y)
    xx <- x[c(ch, ch[1])]
    yy <- y[c(ch, ch[1])]
  }

  list(
    x = xx,
    y = yy
  )
}

## convex hull
## TODO: alpha hull needs other dependency (package `alphahull`)
# switch (hull,
#         "convex" = {
#           lapply(seq(len),
#                  function(i) {
#                    ch <- grDevices::chull(x[[i]], y[[i]])
#                    loon::l_layer_line(
#                      widget,
#                      x = x[[i]][c(ch, ch[1])],
#                      y = y[[i]][c(ch, ch[1])],
#                      color = color[i],
#                      linewidth = linewidth[i],
#                      parent = hull_group,
#                      label = paste0("convexHull", i),
#                      ...
#                    )
#                  })
#         },
#         "alpha" = {
#
#           if(length(active) != len)
#             active <- rep(active, len)
#
#           lapply(seq(len),
#                  function(i) {
#
#                    # In `ashape`, remove duplicated points
#                    not_duplicated <- !duplicated(data.frame(x = x[[i]], y = y[[i]]))
#
#                    ah <- alphahull::ashape(x[[i]][not_duplicated], y[[i]][not_duplicated], alpha = alpha)
#                    loon::l_layer_lines(
#                      widget,
#                      x = c(ah$edges[, "x1"], ah$edges[, "x2"]),
#                      y = c(ah$edges[, "y1"], ah$edges[, "y2"]),
#                      color = color[i],
#                      group = rep(1:length(ah$edges[, "x1"]), 2),
#                      linewidth = linewidth[i],
#                      parent = hull_group,
#                      label = paste0("alphaHull", i),
#                      active = active[i],
#                      ...
#                    )
#                  })
#         }
# )
