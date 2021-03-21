interpolate_list <- function(data, start = NULL,
                             tour_path = tourr::grand_tour(), numOfTours = 30L,
                             interpolation = 10L) {

  if(is.null(start))
    start <- tour_path(NULL, data)
  else {

    stopifnot(
      exprs = {
        nrow(start) == ncol(data)
      }
    )

    start <- as.matrix(start)
  }

  projection <- list()

  for(i in seq(numOfTours)) {
    end <- tour_path(start, data)
    if(is.null(end)) break
    R <- lapply(seq(0, 1, length.out = interpolation),
                function(i) {
                  end$interpolate(i)
                })
    projection <- c(projection, R)
    start <- end$interpolate(1L)
  }

  return(projection)
}

tour_list <- function(data, projection) {

  if(!is.matrix(data)) data <- as.matrix(data)

  lapply(projection,
         function(P) {
           apply(data %*% P, MARGIN = 2, scale01)
         })
}

# avoid calculating p duplicated in the interactive process
creat_start <- function(data, start, tour_path, d) {

  if(is.null(start))
    start <- tour_path(NULL, data)
  else {

    stopifnot(
      exprs = {
        nrow(start) == d
      }
    )

    start <- as.matrix(start)
  }

  start
}
