#' @title Tour Pairs Plot
#' @name tour_pairs
#' @description A nD tour path with a scatterplot matrix (the default tour is a 4D tour;
#' by setting `tour_path` to modify the dimension)
#' @inheritParams l_tour
#' @inheritParams loon::l_pairs
#' @export
#' @return an \code{l_tour_compound} object that one can query the \code{loon} states
#' and a matrix projection vectors
#' @seealso \code{\link{l_pairs}}, \code{\link{l_tour}}
#' @examples
#' if(interactive() && requireNamespace('tourr')) {
#'   # q is a `l_pairs` object
#'   q <- l_tour_pairs(olive[, -c(1:2)],
#'                     color = olive$region)
#'   # query the matrix of projection vectors
#'   proj <- q["projection"]
#'
#'   # query the `l_compound` widget
#'   lc <- l_getPlots(q)
#'   # pack the `density2d` layers
#'   layer_pack <- lapply(lc, function(w) l_layer_density2d(w))
#'
#'   #### set `as.l_tour = FALSE`
#'   # q is a `l_pairs` object
#'   q <- l_tour_pairs(tourr::flea[, 1:6],
#'                     as.l_tour = FALSE,
#'                     color = tourr::flea$species,
#'                     showHistogram = TRUE,
#'                     showSerialAxes = TRUE)
#'
#'   # proj <- q["projection"] # Return a list of `NA`
#'   # query estimated matrix of projection vectors
#'   proj <- l_getProjection(q, tourr::flea[, 1:6])
#' }
l_tour_pairs <- function(data, scaling = c('data', 'variable', 'observation', 'sphere'),
                         tour_path = tourr::grand_tour(4L), numOfTours = 30L, interpolation = 40L,
                         as.l_tour = TRUE, connectedScales = c("none", "cross"),
                         linkingGroup, linkingKey, showItemLabels = TRUE, itemLabel,
                         showHistograms = FALSE, histLocation = c("edge", "diag"),
                         histHeightProp = 1, histArgs = list(),
                         showSerialAxes = FALSE, serialAxesArgs = list(),
                         color = "grey60", group = "color",
                         start = NULL, parent = NULL, span = 10L, envir = parent.frame(), ...) {
  stopifnot(
    exprs = {
      is.numeric(numOfTours)
      is.finite(numOfTours)
      is.numeric(interpolation)
      is.finite(interpolation)
    }
  )

  scaling <- match.arg(scaling)
  # data is n * d
  originalData <- data
  originalStart <- start

  data <- get_scaledData(originalData, scaling = scaling)

  # start is d * k
  d <- ncol(data)
  start <- creat_start(data, originalStart, tour_path, d)
  k <- ncol(start)

  if(k <= 2) {
    # call l_tour
    p <- do.call(
      l_tour,
      remove_null(
        data = originalData,
        scaling = scaling,
        as.l_tour = as.l_tour,
        color = color, tour_path = tour_path, group = group,
        start = start, numOfTours = numOfTours, interpolation = interpolation,
        parent = parent,
        envir = envir,
        ...
      )
    )

    return(p)
  }

  env <- environment()

  projections <- interpolate_list(data, start = start,
                                  tour_path = tour_path, numOfTours = numOfTours,
                                  interpolation = interpolation)
  tours <- tour_list(data, projections)

  new.toplevel <- FALSE
  if(is.null(parent)) {
    new.toplevel <- TRUE
    # create parent
    parent <- l_toplevel()
  }

  child <- paste0(loon::l_subwin(parent, ""), "pairs")
  tcltk::tktitle(parent) <- paste("Tour Pairs", "--path:", child)

  dataNames <- paste0("V", seq(k))
  proj <- stats::setNames(as.data.frame(as.matrix(data) %*% start), dataNames)
  histLocation <- match.arg(histLocation)
  histspan <- span
  histAdjust <- 1
  if(showHistograms && histLocation == "edge") {
    histspan <- round(histHeightProp * span)
  }
  if(showHistograms && histLocation == "diag") {
    histAdjust <- 0
  }
  p <- l_pairs(proj, connectedScales = match.arg(connectedScales),
               linkingGroup, linkingKey,
               showItemLabels = showItemLabels, itemLabel,
               showHistograms = showHistograms, histLocation = histLocation,
               histHeightProp = histHeightProp, histArgs = histArgs,
               showSerialAxes = showSerialAxes, serialAxesArgs = c(serialAxesArgs, list(scaling = "none")),
               parent = parent, span = span, color = color, ...)

  # Set up position
  from <- 0
  var <- 0
  varTcl <- tcltk::tclVar(var)
  # `length(projections)` is less and equal than numOfTours * interpolation
  to <- length(projections)
  # tour slider bar
  tourBar <- as.character(
    tcltk::tcl('scale',
               as.character(loon::l_subwin(child, 'scale')),
               orient = 'vertical', variable = varTcl,
               showvalue = 0, from = from, to = to,
               resolution = 1)
  )

  # refresh button
  refreshButton <- as.character(
    tcltk::tcl('button',
               as.character(loon::l_subwin(child,'refresh')),
               text = "refresh",
               bg = "grey80",
               fg = "black",
               borderwidth = 2,
               relief = "raised"))

  # scale radio button
  scalingVar <- tcltk::tclVar(scaling)
  scaleRadioButtons <- sapply(as.character(formals(l_tour_pairs)[["scaling"]])[-1],
                                function(scale) {
                                  as.character(tcltk::tcl('radiobutton',
                                                          as.character(loon::l_subwin(child, 'radiobutton')),
                                                          text = scale,
                                                          variable = scalingVar,
                                                          value = scale))
                                })

  path <- file.path(find.package(package = 'loon.tourr'), "images")

  upbutton <- tryCatch(
    expr = {
      tcltk::tkimage.create("photo",  tcltk::tclVar(),
                            file=paste0(path, "/up.png"))
    },
    error = function(e) {
      assign("path",
             file.path(find.package(package = 'loon.tourr'), "inst/images"),
             envir = env)
      tcltk::tkimage.create("photo",  tcltk::tclVar(),
                            file=paste0(path, "/up.png"))
    }
  )


  # one step up
  onStepUpButton <-  as.character(
    tcltk::tcl('button',
               as.character(loon::l_subwin(child, 'one step up button')),
               image = upbutton,
               bg = "grey92",
               borderwidth = 1,
               relief = "raised"))

  downbutton <- tcltk::tkimage.create("photo",  tcltk::tclVar(),
                                      file=paste0(path, "/down.png"))
  # one step down
  onStepDownButton <-  as.character(
    tcltk::tcl('button',
               as.character(loon::l_subwin(child, 'one step down button')),
               image = downbutton,
               bg = "grey92",
               borderwidth = 1,
               relief = "raised"))

  rowcols <- tk_get_row_and_columns(widget = p, span = span,
                                    histspan = histspan,
                                    histAdjust = histAdjust)

  tk_grid_pack_tools(tourBar = tourBar, refreshButton = refreshButton,
                     scaleRadioButtons = scaleRadioButtons,
                     onStepUpButton = onStepUpButton,
                     onStepDownButton = onStepDownButton,
                     parent = child,
                     pack = new.toplevel,
                     row = rowcols$row,
                     column = rowcols$column)

  # initial settings
  axesLength <- 0.2
  count <- 0
  countOld <- 0
  # `varOld` is only used in trail layer
  varOld <- 0
  scalingOld <- scaling
  # step <- 0
  toOld <- to

  group <- tryCatch(
    expr = {
      # all points are linked
      # we suppose they share the same state (`group` obj)
      p[[1]][group]
    },
    error = function(e) ""
  )
  initialTour <- na.omit(proj)

  tour <- if(as.l_tour) {
    structure(
      stats::setNames(list(p, start),
                      c(as.character(child), "projection")),
      class = c("l_tour_compound", "loon"))
  } else p


  l_object_name <- character(0L)

  update <- function(...) {

    scalingVar <- tcltk::tclvalue(scalingVar)
    callback_scaling(scalingVar, scalingOld, tour_path,
                     originalData, originalStart, d, k, start,
                     numOfTours, interpolation, isPairs = TRUE, env = env)

    callback_refresh(count, countOld, data, start,
                     tour_path, numOfTours, interpolation, env = env)

    # as the slider bar is moved,
    # the `varTcl` is updated automatically
    # then, update the `var` manually
    var <<- as.numeric(tcltk::tclvalue(varTcl))

    if(as.l_tour) {
      # A compromised way to pack a projection object in loon
      # Create a list "list(loon, projection)", then modify the class of the list (force it to be a loon widget)
      # Advantage:
      #   1. No need to touch the tcl or change the main structure of loon
      #   2. The matrix of projection vectors can be updated as the interface changes.
      # Drawbacks:
      #   1. Have to create a bunch of functions, like `[.l_tour`, `[<-.l_tour`, ... to cover
      #   2. A `l_tour` must be assigned to a real object.
      #      ########### Example
      #      # in your console, if you call
      #      l_tour_pairs(iris)
      #      # Instead of
      #      p <- l_tour_pairs(iris)
      #      `projection` matrix would not be updated
      #   3. Several objects may point to one single loon widget.

      # update l_object_name
      # find the loon object name in `envir`
      ls_objects <- ls(envir = envir)
      obj_name <- lapply(ls_objects,
                         function(obj) {
                           tourobj <- get(obj, envir = envir)
                           pname <- l_path_name(tourobj, child)
                           if(length(pname) == 0) return(NULL)
                           if(grepl(as.character(child), pname) && (l_isTour(tourobj) || l_isCompound(tourobj))) return(obj)

                           else return(NULL)
                         })

      assign("l_object_name", unlist(obj_name), envir = env)

      if(var > 0 && !is.null(l_object_name)) {
        tour <- l_configure(
          tour,
          projection = projections[[var]]
        )
        # This loop is very dangerous
        # Reason: more than one objects would point to one single loon widget
        # So far, I do not have any better idea
        # FIXME
        # Find Better ways to extract the matrix of projection vectors (side effect is not suggested)
        lapply(l_object_name,
               function(x) {
                 tourobj <- get(x, envir = envir)
                 pname <- l_path_name(tourobj, child)
                 if(grepl(as.character(child), pname) && (l_isTour(tourobj) || l_isCompound(tourobj)))
                   assign(x, tour, envir = envir)
                 else {
                   warning("The object `",
                           x,
                           "` is assigned to other object ",
                           "so that the `projections` will not be updated",
                           call. = FALSE,
                           immediate. = TRUE)}
               })
      }
    }

    callback_pairs(widget = p, initialTour = initialTour, start = start,
                   color = color, group = group, tours = tours, var = var,
                   data = originalData,
                   varOld = varOld, projections = projections, dataNames = dataNames)

    # step <<- step + 1
    varOld <<- var
    to <<- length(projections)

    if(to != toOld) {
      toOld <<- to
      tcltk::tkconfigure(tourBar, to = to)
    }
  }
  # as tourBar or refresh button is modified, update function will be executed
  tcltk::tkconfigure(tourBar, command = update)
  tcltk::tkconfigure(refreshButton,
                     command = function(...) {
                       count <<- count + 1
                       update(...)
                     })

  tcltk::tkconfigure(onStepUpButton,
                     command = function(...) {

                       if(var == 0) return(NULL)

                       varOld <<- var
                       var <<- var - 1
                       varTcl <<- tcltk::tclVar(var)
                       tcltk::tkconfigure(tourBar,
                                          variable = varTcl)
                       update(...)
                     })


  tcltk::tkconfigure(onStepDownButton,
                     command = function(...) {

                       if(var == to) return(NULL)

                       varOld <<- var
                       var <<- var + 1
                       varTcl <<- tcltk::tclVar(var)
                       tcltk::tkconfigure(tourBar,
                                          variable = varTcl)
                       update(...)
                     })

  lapply(scaleRadioButtons,
         function(scale_radio_button) {
           tcltk::tkconfigure(scale_radio_button,
                              command = update)
         })

  return(tour)
}
