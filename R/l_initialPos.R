# Save the x and y values from widget
# the benefit of such operation:
# NAs will be filtered
l_initialTour <- function(widget) {
  UseMethod("l_initialTour", widget)
}

l_initialTour.l_hist <- function(widget) {
  widget["x"]
}

l_initialTour.l_plot <- function(widget) {
  data.frame(x = widget["x"], y = widget["y"], stringsAsFactors = FALSE)
}

l_initialTour.l_serialaxes <- function(widget) {
  char2num.data.frame(widget['data'])
}

l_initialTour.l_compound <- function(widget) {
  lapply(widget, function(w){l_initialTour(w)})
}
