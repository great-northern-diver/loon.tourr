context("test callback functions")
library(loon.tourr)
library(tourr)

test_that("test callback l_plot", {

  # call back l_plot
  ir <- iris[, -5]
  widget <- l_plot(ir)
  initialTour <- data.frame(x = widget['x'], y = widget['y'])

  start <- grand_tour()(NULL, ir)
  color <- widget['color']
  group <- ""
  projections <- interpolate_list(ir, start = start,
                                  tour_path = grand_tour(), numOfTours = 30L,
                                  interpolation = 40L)
  var <- sample(seq(length(projections)), 1)

  tours <- tour_list(ir, projections)

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = var, projections = projections)

  expect_true(l_isLoonWidget(widget))

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = 0L, projections = projections)
  expect_equal(initialTour$x, widget['x'])
})

test_that("test callback l_plot", {

  # call back l_plot
  ir <- iris[, -5]
  widget <- l_plot(ir)
  initialTour <- data.frame(x = widget['x'], y = widget['y'])

  start <- grand_tour()(NULL, ir)
  color <- widget['color']
  group <- ""
  projections <- interpolate_list(ir, start = start,
                                  tour_path = grand_tour(), numOfTours = 30L,
                                  interpolation = 40L)
  var <- sample(seq(length(projections)), 1)

  tours <- tour_list(ir, projections)

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = var, projections = projections)

  expect_true(l_isLoonWidget(widget))

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = 0L, projections = projections)
  expect_equal(initialTour$x, widget['x'])
})

test_that("test callback l_hist", {

  # call back l_plot
  ir <- iris[, -5]
  widget <- l_hist(ir)
  initialTour <- data.frame(x = widget['x'])

  start <- grand_tour(1L)(NULL, ir)
  color <- widget['color']
  group <- ""
  projections <- interpolate_list(ir, start = start,
                                  tour_path = grand_tour(1L), numOfTours = 30L,
                                  interpolation = 40L)

  var <- sample(seq(length(projections)), 1)

  tours <- tour_list(ir, projections)

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = var, projections = projections)

  expect_true(l_isLoonWidget(widget))

  callback_plot(widget = widget, initialTour = unlist(initialTour),
                start = start, color = color, group = group,
                tours = tours, var = 0L, projections = projections)
  expect_equal(initialTour$x, widget['x'])
})

test_that("test callback l_serialaxes", {

  # call back l_plot
  ir <- iris[, -5]
  widget <- l_serialaxes(ir)
  initialTour <- char2num.data.frame(widget['data'])

  start <- grand_tour(3L)(NULL, ir)
  color <- widget['color']
  group <- ""
  projections <- interpolate_list(ir, start = start,
                                  tour_path = grand_tour(3L), numOfTours = 30L,
                                  interpolation = 40L)

  var <- sample(seq(length(projections)), 1)

  tours <- tour_list(ir, projections)

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = var, projections = projections)

  expect_true(l_isLoonWidget(widget))

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = 0L, projections = projections)
  expect_equal(initialTour, char2num.data.frame(widget['data']))
})

test_that("test callback l_facet", {

  # call back l_plot
  ir <- iris[, -5]
  by <- data.frame(Species = iris$Species)
  widget <- l_plot(ir, by = by)
  initialTour <- split(ir[, 1:2],
                 f = as.list(by),
                 drop = FALSE)

  start <- grand_tour()(NULL, ir)
  color <- widget['color']
  group <- ""
  projections <- interpolate_list(ir, start = start,
                                  tour_path = grand_tour(), numOfTours = 30L,
                                  interpolation = 40L)
  var <- sample(seq(length(projections)), 1)

  tours <- tour_list(ir, projections)

  tours <- lapply(tours,
                  function(tour) {
                    split(as.data.frame(tour),
                          f = as.list(by),
                          drop = FALSE)
                  })

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = var, projections = projections,
                by = by)

  expect_true(is(widget, "l_compound"))

  callback_plot(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = 0L, projections = projections)
  expect_equal(initialTour[[1]]$Sepal.Length, widget[[1]]["x"])
})

test_that("test callback l_pairs", {

  # call back l_plot
  ir <- iris[, -5]
  widget <- l_pairs(ir)
  initialTour <- ir

  start <- grand_tour(4L)(NULL, ir)
  color <- widget[[1]]['color']
  group <- ""
  projections <- interpolate_list(ir, start = start,
                                  tour_path = grand_tour(4L), numOfTours = 30L,
                                  interpolation = 40L)

  var <- sample(seq(length(projections)), 1)

  tours <- tour_list(ir, projections)
  dataNames <- colnames(ir)

  callback_pairs(widget = widget, initialTour = initialTour,
                 start = start, color = color, group = group,
                 tours = tours, var = var, projections = projections, dataNames = dataNames)

  expect_true(is(widget, "l_compound"))
  expect_equal(tours[[var]][, 1], widget$x2y1['y'])

  callback_pairs(widget = widget, initialTour = initialTour,
                start = start, color = color, group = group,
                tours = tours, var = 0L, projections = projections, dataNames = dataNames)
  expect_equal(initialTour$Sepal.Length, widget$x2y1["y"])
})


test_that("test callback scaling and refresh", {
  callback_scaling(originalData = iris[, -5], scalingVar = "data",
                   scalingOld = "var", d = 4)
  expect_equal(length(projections), 1200L)
  callback_refresh(count = 1L, data = get_scaledData(iris[, -5], "data"))
  expect_equal(length(projections), 1200L)
})
