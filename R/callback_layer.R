callback_layer <- function(target, ...) {

  widget <- l_getPlots(target)

  layers <- loon::l_layer_getChildren(widget)
  # model layer
  if(length(layers) == 1) return(NULL)

  lapply(setdiff(layers, "model"),
         function(layer) {
           l_layer_callback(widget, layer, ...)
         })
}
