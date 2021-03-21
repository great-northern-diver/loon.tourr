callback_scaling <- function(scalingVar = "data", scalingOld = "data",
                             tour_path = tourr::grand_tour(),
                             originalData, originalStart = NULL,
                             d, k = 2, start = NULL,
                             numOfTours = 30L, interpolation = 40L, isPairs = FALSE,
                             by = NULL, env = parent.frame()) {

  if(scalingVar == scalingOld) return(NULL)

  data <- get_scaledData(originalData, scaling = scalingVar)
  assign("data", data, envir = env)

  start <- creat_start(data, originalStart, tour_path, d)
  assign("start", start, envir = env)
  # proj is n * d * d * k
  proj <- as.matrix(data) %*% start

  if(isPairs) {
    initialTour <- as.data.frame(na.omit(proj))
  } else {
    initialTour <- switch(as.character(k),
                         "1" = {
                           na.omit(scale01(proj[, 1]))
                         },
                         "2" = {
                           na.omit(data.frame(x = scale01(proj[, 1]),
                                              y = scale01(proj[, 2])))
                         }, {
                           as.data.frame(na.omit(proj))
                         })
  }

  # update random tours
  projections <- interpolate_list(data, start = start,
                                  tour_path = tour_path, numOfTours = numOfTours,
                                  interpolation = interpolation)

  tours <- tour_list(data, projections)
  scalingOld <- scalingVar

  if(!is.null(by)) {
    initialTour <- split(initialTour, f = as.list(by), drop = FALSE)
    tours <- lapply(tours,
                    function(tour) {
                      split(as.data.frame(tour),
                            f = as.list(by),
                            drop = FALSE)
                    })
  }

  assign("initialTour", initialTour, envir = env)
  assign("projections", projections, envir = env)
  assign("tours", tours, envir = env)
  assign("scalingOld", scalingOld, envir = env)

  return(invisible())
}

callback_refresh <- function(count = 0L, countOld = 0L,
                             data, start = NULL, tour_path = tourr::grand_tour(),
                             numOfTours = 30L, interpolation = 40L, by = NULL, env = parent.frame()) {

  if(count == countOld) return(NULL)
  # update random tours
  projections <- interpolate_list(data, start = start,
                                  tour_path = tour_path, numOfTours = numOfTours,
                                  interpolation = interpolation)
  tours <- tour_list(data, projections)
  countOld <- count

  if(!is.null(by)) {
    tours <- lapply(tours,
                    function(tour) {
                      split(as.data.frame(tour),
                            f = as.list(by),
                            drop = FALSE)
                    })
  }

  assign("tours", tours, envir = env)
  assign("projections", projections, envir = env)
  assign("countOld", countOld, envir = env)

  return(invisible())
}
