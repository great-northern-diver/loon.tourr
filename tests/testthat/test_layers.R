context("test layers")
library(loon.tourr)

test_that("test layers", {
  ir <- iris[, -5]
  scaledData <- get_scaledData(ir, "variable")

  ############# hull layer
  p <- l_plot(ir, color = iris$Species)
  l1 <- l_layer_hull(p, group = iris$Species)
  expect_true(inherits(l1, "l_layer_lines"))
  l2 <- l_layer_hull(p)
  expect_true(inherits(l2, "l_layer_line"))

  ### layer callback
  l_configure(p, x = scaledData[,1], y = scaledData[,2])
  l_layer_callback(p, l1, x = scaledData[,1], y = scaledData[,2], group = p['color'])
  l_layer_callback(p, l2, x = scaledData[,1], y = scaledData[,2])

  expect_equal(l2['x'][1:9], scaledData[,1][chull(scaledData[,1], scaledData[,2])])
  ############# trails layer
  p <- l_plot(ir)
  l3 <- l_layer_trails(p,
                       x = iris$Sepal.Length,
                       y = iris$Sepal.Width,
                       xpre = jitter(iris$Sepal.Length, factor = 5),
                       ypre = jitter(iris$Sepal.Width, factor = 5))
  expect_true(inherits(l3, "l_layer_lines"))

  start <- grand_tour()(NULL, scaledData)
  projections <- interpolate_list(scaledData, start = start,
                                  tour_path = grand_tour(), numOfTours = 30L,
                                  interpolation = 40L)
  tours <- tour_list(scaledData, projections)
  l_configure(p, x = scaledData[,1], y = scaledData[,2])
  l_layer_callback(p, l3, tours = tours, var = 20L, varOld = 21L, start = start)
  expect_true(inherits(l3, "l_layer_lines"))

  l4 <- l_layer_trails(p)
  expect_true(inherits(l4, "l_layer_lines"))

  ############# density2d layer
  p <- l_plot(ir)
  l5 <- l_layer_density2d(p, color = "red", linewidth = 4)
  expect_true(inherits(l5 , "l_layer_group"))

  l_configure(p, x = scaledData[,1], y = scaledData[,2])
  l_layer_callback(p, l5, x = scaledData[,1], y = scaledData[,2], group = "")

  expect_equal(length(l_layer_getChildren(p)), 2)

  p <- l_plot(ir)
  l6 <- l_layer_density2d(p, group = iris$Species, linewidth = 4)
  l_configure(p, x = scaledData[,1], y = scaledData[,2])
  l_layer_callback(p, l6, x = scaledData[,1], y = scaledData[,2], group = iris$Species)

  children <- l_layer_getChildren(p)
  expect_equal(length(children), 2)

  expect_true(inherits(l_create_handle(c(p, children[1])), "l_layer_group"))
})
