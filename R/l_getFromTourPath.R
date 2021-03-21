# #' @title Create a loon tour object from path name
# #' @description This function can be used to create the loon tour object from a valid widget path name.
# #' @param loon object specification (e.g. ".l0.plot")
# #' @seealso \link{\code{l_getFromPath}}
# l_getFromTourPath <- function(target) {
#
#   widgets <- loon::l_getFromPath(target)
#
#   x <- list(target = widgets,
#             "projection" = NULL)
#
#   class(x) <- c(ifelse(length(widgets) > 1,
#                        "l_tour_compound",
#                        "l_tour"),
#                 "loon")
#   x
# }
