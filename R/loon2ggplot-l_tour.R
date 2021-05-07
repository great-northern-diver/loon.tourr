#' @export
#' @importFrom loon.ggplot loon.ggplot
loon2ggplot.l_tour <- function(target, ...) {
  widget <- l_getPlots(target)
  loon.ggplot::loon2ggplot(widget, ...)
}
