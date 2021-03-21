l_creat_tour_widget <- function(data, scaling = 'data', color = "grey60", tour_path = tourr::grand_tour(),
                                start = NULL, parent = NULL, by = NULL, on, ...) {

  data <- get_scaledData(data, scaling = scaling)
  start <- creat_start(data, start, tour_path, ncol(data))

  # project on a k-d space
  k <- ncol(start)
  args <- list(...)
  proj <- as.matrix(data) %*% start
  connectedScales <- args$connectedScales %||% "none"
  args$connectedScales <- NULL

  if(missing(on)) on <- NULL

  switch(as.character(k),
         "0" = {stop("`l_tour` would not project data onto a 0-D space")},
         "1" = {
           args$xlabel <- args$xlabel %||% ""
           x <- scale01(proj[, 1])

           # initial projection
           p <- do.call(
             loon::l_hist,
             c(
               list(
                 x = x,
                 color = color,
                 yshows = "density",
                 parent = parent,
                 by = by,
                 on = on,
                 connectedScales = connectedScales
               ),
               args
             )
           )
         },
         "2" = {
           # scatterplot
           args$xlabel <- args$xlabel %||% ""
           args$ylabel <- args$ylabel %||% ""

           x <- scale01(proj[, 1])
           y <- scale01(proj[, 2])

           # initial projection
           p <- do.call(
             loon::l_plot,
             c(
               list(
                 x = x,
                 y = y,
                 color = color,
                 parent = parent,
                 by = by,
                 on = on,
                 connectedScales = connectedScales
               ),
               args
             )
           )

           l_layer_guideAxes <- function(widget, x, y, text, axesLength = 0.2) {

             tourGuides <- loon::l_layer_group(
               widget,
               label = "guides"
             )
             # axes
             axes <- loon::l_layer_lines(widget,
                                         x = lapply(x, function(xx) c(0.5, 0.5 + xx * (axesLength - 0.05))),
                                         y = lapply(y, function(yy) c(0.5, 0.5 + yy * (axesLength - 0.05))),
                                         color = "gray50",
                                         parent = tourGuides)

             # labels
             labels <- loon::l_layer_texts(widget,
                                           x = x * axesLength + 0.5,
                                           y = y * axesLength + 0.5,
                                           text = text,
                                           size = 12,
                                           color = "black",
                                           parent = tourGuides)

             l_layer_lower(widget, tourGuides)
           }

           ############# tour guides
           axesLength <- 0.2
           if(is(p, "l_plot")) {
             l_layer_guideAxes(p, x = start[, 1], y = start[, 2],
                               text = colnames(data), axesLength = axesLength)
           } else if(is(p, "l_facet")) {
             lapply(p,
                    function(w) {
                      if(is(w, "l_plot"))
                        l_layer_guideAxes(w, x = start[, 1], y = start[, 2],
                                          text = colnames(data), axesLength = axesLength)
                    })
           } else stop("Unknown widget", call. = FALSE)
         }, {
           p <- loon::l_serialaxes(
             data = proj,
             color = color,
             scaling = if(scaling == "sphere") "data" else "none", # scaling has already been set at the beginning
             parent = parent,
             by = by,
             on = on,
             ...
           )
         }
  )

  if(!is(p, "l_facet")) {

    tcltk::tkconfigure(paste(p,'.canvas',sep=''),
                       width = 500,
                       height = 500)

    # pack the plot
    tcltk::tkgrid(p,
                  row = 0,
                  column = 0,
                  rowspan = 20,
                  columnspan = 20,
                  sticky="nesw")
  }

  return(p)
}
