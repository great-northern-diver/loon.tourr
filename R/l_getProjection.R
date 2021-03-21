#' @title Query the matrix of projection vectors
#' @param target A object returned by \code{l_tour}
#' @param data Original data set
#' @return a matrix of projection vectors
#' @export
#' @examples
#' if(interactive()) {
#'   dat <- iris[,-5]
#'   p <- l_tour(dat, color = iris$Species,
#'               as.l_tour = FALSE)
#'   # scroll the bar
#'   proj <- l_getProjection(p, dat)
#'   projected_object <- as.matrix(dat) %*% proj
#'   # it will not be exactly the same
#'   plot(projected_object[,1], projected_object[,2],
#'        col = hex12tohex6(p['color']))
#' }
l_getProjection <- function(target, data) {
  UseMethod("l_getProjection", target)
}

#' @export
l_getProjection.l_hist <- function(target, data) {

  inv_data <- MASS::ginv(as.matrix(data))
  proj <- inv_data %*% matrix(c(target['x']), ncol = 1)

  # divide the norm
  apply(proj, MARGIN = 2, function(x) {x/sqrt(sum(x^2))})
}

#' @export
l_getProjection.l_plot <- function(target, data) {

  inv_data <- MASS::ginv(as.matrix(data))
  proj <- inv_data %*% matrix(c(target['x'], target['y']), ncol = 2)

  # divide the norm
  apply(proj, MARGIN = 2, function(x) {x/sqrt(sum(x^2))})
}

#' @export
l_getProjection.l_serialaxes <- function(target, data) {

  inv_data <- MASS::ginv(as.matrix(data))
  proj <- inv_data %*% as.matrix(char2num.data.frame(target['data']))

  # divide the norm
  apply(proj, MARGIN = 2, function(x) {x/sqrt(sum(x^2))})
}

#' @export
l_getProjection.l_tour <- function(target, data) {target['projection']}

#' @export
l_getProjection.l_tour_compound <- function(target, data) {target['projection']}

#' @export
l_getProjection.l_compound <- function(target, data) {

  lapply(target,
         function(t) {
           l_getProjection(t, data)
         })
}
