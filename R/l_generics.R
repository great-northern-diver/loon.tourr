# `l_tour` is not a real tcl widget
# the projection is achieved via the manipulation of the environment
#' @export
l_cget.l_tour <- function(target, state) {

  widget <- l_getPlots(target)

  target <- unclass(target)
  if(is.list(target) && "projection" %in% state) {
    return(target[["projection"]])
  }

  loon::l_cget(widget, state)
}

#' @export
l_cget.l_tour_compound <- function(target, state) {

  widget <- l_getPlots(target)
  names <- names(target)
  target <- unclass(target)

  if(is.list(target) && (state %in% names)) {
    if(state == "projection")
      return(target[[state]])
    else
      return(widget[[state]])
  }

  lapply(widget,
         function(w) {
           loon::l_cget(w, state)
         })
}

#' @export
l_configure.l_tour <- function(target, ...) {

  # the loon widget
  widget <- l_getPlots(target)

  # to list
  target <- unclass(target)

  # the args
  args <- list(...)
  name <- names(args)

  if(is.list(target)) {

    #### configure projection
    if("projection" %in% name) {
      target[["projection"]] <- args[["projection"]]
      args[["projection"]] <- NULL
    }

    if(length(args) == 0)
      return(invisible(
        structure(
          target,
          class = c("l_tour", "loon")
        )
      ))

    ### configure plots
    widget_name <- setdiff(names(target), "projection")

    do.call(loon::l_configure, c(list(target = widget), args))
    target[[widget_name]] <- widget

    #### return target
    return(invisible(
      structure(
        target,
        class = c("l_tour", "loon")
      )
    ))

  } else {
    if("projection" %in% name) {
      args[["projection"]] <- NULL
    }
    if(length(args) == 0) return(invisible(target))
    do.call(l_configure, c(list(target = widget), args))

    return(invisible(target))
  }
}

#' @export
l_configure.l_tour_compound <- function(target, ...) {

  # the loon widget
  widget <- l_getPlots(target)

  # to list
  target <- unclass(target)

  # the args
  args <- list(...)
  name <- names(args)

  if(is.list(target)) {

    #### configure projection
    if("projection" %in% name) {
      target[["projection"]] <- args[["projection"]]
      args[["projection"]] <- NULL
    }

    if(length(args) == 0)
      return(
        invisible(
          structure(
            target,
            class = c("l_tour_compound", "loon")
          )
        ))

    ### configure plots
    lapply(widget,
           function(w) {
             do.call(loon::l_configure, c(list(target = w), args))
           })

    target[[setdiff(names(target), "projection")]] <- widget

    #### return target
    return(invisible(
      structure(
        target,
        class = c("l_tour_compound", "loon")
      )
    ))

  } else {
    if("projection" %in% name) {
      args[["projection"]] <- NULL
    }
    if(length(args) == 0) return(invisible(target))
    do.call(l_configure, c(list(target = widget), args))

    return(invisible(target))
  }
}


#' @export
names.l_tour <- function(x) {

  plot_name <- setdiff(names(unclass(x)), "projection")
  plot <- x[[plot_name]]

  c(names(plot), "projection")
}

#' @export
names.l_tour_compound <- function(x) {

  plot_name <- setdiff(names(unclass(x)), "projection")
  plot <- x[[plot_name]]

  c(names(plot), "projection")
}

#' @export
`names<-.l_tour` <- function(x, value) {
  stop("`l_tour` object is not allowed to set names")
}

#' @export
`names<-.l_tour_compound` <- function(x, value) {
  stop("`l_tour_compound` object is not allowed to set names")
}

#' @export
print.l_tour <- function(x, ...) {
  plot_name <- setdiff(names(unclass(x)), "projection")
  plot <- x[[plot_name]]
  print.default(
    structure(
      as.character(plot),
      class = class(x)
    )
  )
}

#' @export
print.l_tour_compound <- function(x, ...) {
  plot_name <- setdiff(names(unclass(x)), "projection")
  print.default(
    structure(
      as.character(plot_name),
      class = class(x)
    )
  )
}

#' @export
loonGrob.l_tour <- function(target, name = NULL, gp = NULL, vp = NULL) {
  plot_name <- setdiff(names(unclass(target)), "projection")
  plot <- target[[plot_name]]
  loonGrob(plot, name = name, gp = gp, vp = vp)
}

#' @export
loonGrob.l_tour_compound <- function(target, name = NULL, gp = NULL, vp = NULL) {
  plot_name <- setdiff(names(unclass(target)), "projection")
  plot <- target[[plot_name]]
  loonGrob(plot, name = name, gp = gp, vp = vp)
}
