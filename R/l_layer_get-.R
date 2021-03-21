l_layer_getAxes <- function(widget) {
  UseMethod("l_layer_getAxes", widget)
}

l_layer_getAxes.l_hist <- function(widget) {
  NULL
}

l_layer_getAxes.l_plot <- function(widget) {
  layer <- setdiff(loon::l_layer_getChildren(widget), "model")
  children <- loon::l_layer_getChildren(widget, layer)
  for(child in children) {
    l <- loon::l_layer_getLabel(widget, child)
    if(l == "lines") return(loon::l_create_handle(c(widget, child)))
  }
}

l_layer_getAxes.l_serialaxes <- function(widget) {
  NULL
}

l_layer_getAxes.l_compound <- function(widget) {
  lapply(widget, function(w) l_layer_getAxes(w))
}

l_layer_getLabels <- function(widget) {
  UseMethod("l_layer_getLabels", widget)
}

l_layer_getLabels.l_hist <- function(widget) {
  NULL
}

l_layer_getLabels.l_plot <- function(widget) {
  layer <- setdiff(loon::l_layer_getChildren(widget), "model")
  children <- loon::l_layer_getChildren(widget, layer)
  for(child in children) {
    l <- loon::l_layer_getLabel(widget, child)
    if(l == "texts") return(loon::l_create_handle(c(widget, child)))
  }
}

l_layer_getLabels.l_serialaxes <- function(widget) {
  NULL
}

l_layer_getLabels.l_compound <- function(widget) {
  lapply(widget, function(w) l_layer_getLabels(w))
}
