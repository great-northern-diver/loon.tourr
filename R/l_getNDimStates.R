l_getNDimStates <- function(widget) {
  UseMethod("l_getNDimStates", widget)
}

l_getNDimStates.loon <- function(widget) {
  statesNames <- loon::l_nDimStateNames(widget)
  states <- stats::setNames(
    lapply(statesNames,
           function(s) {
             if(length(widget[s]) == 0) return(NULL)
             widget[s]
           }),
    statesNames)
  # remove NULL
  remove_null(states, as_list = FALSE)
}

l_getNDimStates.l_compound <- function(widget) {
  lapply(widget, function(x) l_getNDimStates(x))
}
