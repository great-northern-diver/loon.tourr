callback_pairs <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)
  start <- args$start
  color <- args$color
  group <- args$group
  varOld <- args$varOld
  data <- args$data
  projections <- args$projections
  dataNames <- args$dataNames

  len <- length(widget)

  lapply(seq(len),
         function(i) {

           w <- widget[[i]]

           if(inherits(w, "l_plot")) {

             # the original data set is n * d
             xth <- which(dataNames == w['xlabel'])
             yth <- which(dataNames == w['ylabel'])
             # initialTour is n * k
             initialTour_ <- initialTour[, c(xth, yth)]
             # start is d * k
             start_ <- start[, c(xth, yth)]
             # tours are l * n * k
             tours_ <- lapply(tours, function(tour) tour[, c(xth, yth)])
             # projections are l * d * k
             projections_ <- lapply(projections, function(projection) projection[, c(xth, yth)])

           } else if(inherits(w, "l_hist")) {

             xth <- which(dataNames == w['xlabel'])
             # initialTour is n * k
             initialTour_ <- initialTour[, c(xth)]
             # start is d * k
             start_ <- start[, c(xth)]
             # tours are l * n * k
             tours_ <- lapply(tours, function(tour) tour[, c(xth)])
             # projections are l * d * k
             projections_ <- lapply(projections, function(projection) projection[, c(xth)])

           } else if(inherits(w, "l_serialaxes")) {

             # initialTour is n * k
             initialTour_ <- initialTour
             # start is d * k
             start_ <- start
             # tours are l * n * k
             tours_ <- tours
             # projections are l * d * k
             projections_ <- projections

           } else {
             stop("Unknown widgets", call. = FALSE)
           }

           callback_plot(w, initialTour = initialTour_, tours = tours_, var = var,
                         data = data, start = start_,
                         color = color, group = group,
                         varOld = varOld, projections = projections_,
                         l_compound = widget,
                         allTours = tours,
                         allColor = color,
                         allProjections = projections,
                         allInitialTour = initialTour)

           if(inherits(w, "l_plot") || inherits(w, "l_hist")) {
             loon::l_scaleto_world(w)
           }

         })

  return(invisible())
}

