callback_plot <- function(widget, initialTour, tours, var = 0L, ...) {
  UseMethod("callback_plot", widget)
}

callback_plot.l_hist <- function(widget, initialTour, tours, var = 0L, ...) {
  # histogram update
  if(var == 0) {
    initialTour <- unlist(initialTour)
    # start position
    # update hist

    # for l_hist widget, as the x is modified, the binwidth and origin will be modified as default
    loon::l_configure(widget,
                      x = initialTour,
                      binwidth = widget['binwidth'],
                      origin = widget['origin']
    )

    callback_layer(widget,
                   tours = tours,
                   var = var,
                   initialTour = initialTour,
                   ...)
  } else {
    proj <- tours[[var]]
    loon::l_configure(widget,
                      x = proj,
                      binwidth = widget['binwidth'],
                      origin = widget['origin']
    )

    callback_layer(widget,
                   tours = tours,
                   var = var,
                   initialTour = initialTour,
                   ...)
  }
}

callback_plot.l_plot <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)
  axesLength <- args$axesLength %||% 0.2
  axes <- args$axes
  labels <- args$labels
  start <- args$start
  projections <- args$projections

  # scatter plot update
  if(var == 0) {
    # start position
    # update plot
    loon::l_configure(widget,
                      x = initialTour[, 1],
                      y = initialTour[, 2]
    )

    if(!is.null(axes))
      loon::l_configure(axes,
                        x = lapply(start[, 1], function(x) c(0.5, 0.5 + x * axesLength)),
                        y = lapply(start[, 2], function(y) c(0.5, 0.5 + y * axesLength))
      )

    if(!is.null(labels))
      loon::l_configure(labels,
                        x = start[, 1] * axesLength + 0.5,
                        y = start[, 2] * axesLength + 0.5
      )

    callback_layer(widget,
                   tours = tours,
                   var = var,
                   initialTour = initialTour,
                   ...)

  } else {
    proj <- tours[[var]]
    loon::l_configure(widget,
                      x = proj[, 1],
                      y = proj[, 2])

    rotation <- projections[[var]]

    if(!is.null(axes))
      loon::l_configure(axes,
                        x = lapply(rotation[, 1], function(x) c(0.5, 0.5 + x * (axesLength - 0.05))),
                        y = lapply(rotation[, 2], function(y) c(0.5, 0.5 + y * (axesLength - 0.05)))
      )

    if(!is.null(labels))
      loon::l_configure(labels,
                        x = rotation[, 1] * axesLength + 0.5,
                        y = rotation[, 2] * axesLength + 0.5
      )

    callback_layer(widget,
                   tours = tours,
                   var = var,
                   initialTour = initialTour,
                   ...)
  }
}

callback_plot.l_serialaxes <- function(widget, initialTour, tours, var = 0L, ...) {

  statesNames <- loon::l_nDimStateNames(widget)
  states <- stats::setNames(
    lapply(statesNames,
           function(s) {
             widget[s]
           }),
    statesNames
  )
  # data is set by tour
  states$data <- NULL

  # serial axes update
  if(var == 0) {
    # start position
    # update plot
    do.call(loon::l_configure,
            c(
              list(target = widget,
                   data = initialTour),
              states
            ))
  } else {
    do.call(loon::l_configure,
            c(
              list(target = widget,
                   data = as.data.frame(tours[[var]])),
              states
            ))
  }
}

callback_plot.l_facet <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)

  axesLength <- args$axesLength %||% 0.2
  axes <- args$axes
  labels <- args$labels
  color <- args$color
  group <- args$group

  canvasRange <- grDevices::extendrange(c(0, 1))

  lapply(seq(length(widget)),
         function(i) {

           dotArgs <- args
           dotArgs$color <- NULL
           dotArgs$axes <- NULL
           dotArgs$labels <- NULL
           dotArgs$group <- NULL

           w <- widget[[i]]

           do.call(callback_plot,
                   c(
                     list(
                       widget = w,
                       initialTour = initialTour[[i]],
                       tours = lapply(tours, function(tour) tour[[i]]),
                       var = var,
                       color = color[[i]],
                       axes = axes[[i]],
                       labels = labels[[i]],
                       group = group[[i]],
                       l_compound = widget,
                       allTours = tours,
                       allColor = color,
                       allInitialTour = initialTour
                     ),
                     dotArgs
                   )
           )

           # canvas scale to 0 1
           loon::l_configure(
             w,
             panX = canvasRange[1],
             panY = canvasRange[1],
             zoomX = w['deltaX']/diff(canvasRange),
             zoomY = w['deltaY']/diff(canvasRange)
           )
         })

  return(invisible())
}
