callback_plot <- function(widget, initialTour, tours, var = 0L, ...) {
  UseMethod("callback_plot", widget)
}

callback_plot.l_hist <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)
  slicing <- args$slicing %||% FALSE

  # histogram update
  if(var == 0) {

    initialTour <- unlist(initialTour)

    if(slicing) {

      slicingIn <- get_slicingIn(args$start,
                                 args$data,
                                 args$slicingDistance)

      newStates <- lapply(args$states,
                          function(s) {
                            s[slicingIn]
                          })
      newStates$target <- widget
      newStates$x <- initialTour[slicingIn]
      newStates$sync <- "push"
      newStates$binwidth <- widget['binwidth']
      newStates$origin <- widget['origin']
      do.call(loon::l_configure, newStates)

    } else {
      # start position
      # update hist

      # for l_hist widget, as the x is modified, the binwidth and origin will be modified as default
      loon::l_configure(widget,
                        x = initialTour,
                        binwidth = widget['binwidth'],
                        origin = widget['origin']
      )
    }

  } else {

    tour <- tours[[var]]

    if(slicing) {

      slicingIn <- get_slicingIn(args$projections[[var]],
                                 args$data,
                                 args$slicingDistance)

      newStates <- lapply(args$states,
                          function(s) {
                            s[slicingIn]
                          })
      newStates$target <- widget
      newStates$x <- tour[slicingIn]
      newStates$sync <- "push"
      newStates$binwidth <- widget['binwidth']
      newStates$origin <- widget['origin']

      do.call(loon::l_configure, newStates)

    } else {

      loon::l_configure(widget,
                        x = tour,
                        binwidth = widget['binwidth'],
                        origin = widget['origin']
      )
    }
  }

  callback_layer(widget,
                 tours = tours,
                 var = var,
                 initialTour = initialTour,
                 ...)

  loon::l_scaleto_plot(widget)
}

callback_plot.l_plot <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)
  axesLength <- args$axesLength %||% 0.2
  axes <- args$axes
  labels <- args$labels
  slicing <- args$slicing %||% FALSE

  # scatter plot update
  if(var == 0) {
    # start position
    # update plot
    if(slicing) {

      slicingIn <- get_slicingIn(args$start,
                                 args$data,
                                 args$slicingDistance)

      newStates <- lapply(args$states,
                          function(s) {
                            s[slicingIn]
                          })
      newStates$target <- widget
      newStates$x <- initialTour[slicingIn, 1]
      newStates$y <- initialTour[slicingIn, 2]
      newStates$sync <- "push"
      do.call(loon::l_configure, newStates)

    } else {
      loon::l_configure(widget,
                        x = initialTour[, 1],
                        y = initialTour[, 2])
    }

    if(!is.null(axes))
      loon::l_configure(
        axes,
        x = lapply(start[, 1],
                   function(x) c(0.5, 0.5 + x * axesLength)),
        y = lapply(start[, 2],
                   function(y) c(0.5, 0.5 + y * axesLength))
      )

    if(!is.null(labels))
      loon::l_configure(
        labels,
        x = start[, 1] * axesLength + 0.5,
        y = start[, 2] * axesLength + 0.5
      )

  } else {

    projections <- args$projections
    tour <- tours[[var]]
    proj <- projections[[var]]

    if(slicing) {

      slicingIn <- get_slicingIn(proj,
                                 args$data,
                                 args$slicingDistance)

      newStates <- lapply(args$states,
                          function(s) {
                            s[slicingIn]
                          })
      newStates$target <- widget
      newStates$x <- tour[slicingIn, 1]
      newStates$y <- tour[slicingIn, 2]
      newStates$sync <- "push"
      do.call(loon::l_configure, newStates)

    } else {
      loon::l_configure(widget,
                        x = tour[, 1],
                        y = tour[, 2])
    }

    if(!is.null(axes))
      loon::l_configure(
        axes,
        x = lapply(proj[, 1],
                   function(x)
                     c(0.5, 0.5 + x * (axesLength - 0.05))),
        y = lapply(proj[, 2],
                   function(y)
                     c(0.5, 0.5 + y * (axesLength - 0.05)))
      )

    if(!is.null(labels))
      loon::l_configure(
        labels,
        x = proj[, 1] * axesLength + 0.5,
        y = proj[, 2] * axesLength + 0.5
      )
  }

  callback_layer(widget,
                 tours = tours,
                 var = var,
                 initialTour = initialTour,
                 ...)

  loon::l_scaleto_plot(widget)
}

callback_plot.l_serialaxes <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)
  slicing <- args$slicing %||% FALSE
  states <- args$states

  # serial axes update
  if(var == 0) {
    # start position
    # update plot
    if(slicing) {

      slicingIn <- get_slicingIn(args$start,
                                 args$data,
                                 args$slicingDistance)
      states$data <- NULL
      newStates <- lapply(states,
                          function(s) {
                            s[slicingIn]
                          })
      newStates$target <- widget
      newStates$data <- initialTour[slicingIn, ]
      newStates$sync <- "push"
      do.call(loon::l_configure, newStates)

    } else {

      states$data <- NULL
      states$target <- NULL
      do.call(loon::l_configure,
              c(
                list(target = widget,
                     data = initialTour),
                states
              ))
    }

  } else {

    tour <- as.data.frame(tours[[var]])
    proj <- args$projections[[var]]

    if(slicing) {

      slicingIn <- get_slicingIn(proj,
                                 args$data,
                                 args$slicingDistance)
      states$data <- NULL
      newStates <- lapply(states,
                          function(s) {
                            s[slicingIn]
                          })
      newStates$target <- widget
      newStates$data <- tour[slicingIn, ]
      newStates$sync <- "push"
      do.call(loon::l_configure, newStates)

    } else {
      do.call(loon::l_configure,
              c(
                list(target = widget,
                     data = tour),
                states
              ))
    }
  }
}

callback_plot.l_facet <- function(widget, initialTour, tours, var = 0L, ...) {

  args <- list(...)

  axesLength <- args$axesLength %||% 0.2
  axes <- args$axes
  labels <- args$labels
  color <- args$color
  group <- args$group
  lenGroup <- length(group)

  canvasRange <- grDevices::extendrange(c(0, 1))
  lenWidgets <- length(widget)

  lapply(seq(lenWidgets),
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
                       group = if(lenGroup == lenWidgets) group[[i]] else group,
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
