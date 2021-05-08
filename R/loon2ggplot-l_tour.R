#' @export
loon2ggplot.l_tour <- function(target, asAes = TRUE, ...) {
  widget <- l_getPlots(target)
  loon.ggplot::loon2ggplot(widget, asAes = asAes, ...)
}
