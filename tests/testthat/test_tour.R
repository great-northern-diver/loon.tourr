context("test tour")
library(loon.tourr)
library(tourr)

test_that("test tour", {
  # 1D
  fl <- flea[, 1:6]
  p <- l_tour(fl, tour_path = tourr::grand_tour(1))
  p
  w <- l_getPlots(p)
  proj <- l_getProjection(p)
  expect_true(inherits(w, "l_hist"))
  expect_true(inherits(proj, "matrix"))
  # scaling 2D
  p <- l_tour(fl, scaling = "sphere", color = flea$species)
  w <- l_getPlots(p)
  proj <- p['projection']
  g <- loonGrob(p)

  expect_equal(length(l_path_name(p)), 1)
  expect_equal(proj, l_getProjection(p))
  expect_equal(length(unique(p['color'])), 3)
  expect_true(inherits(proj, "matrix"))
  expect_true(inherits(w, "l_plot"))
  expect_true(inherits(g, "gTree"))
  expect_true(l_isTour(p))
  expect_true("projection" %in% names(p))

  p['color'] <- "red"
  expect_equal(length(unique(p['color'])), 1)

  p <- l_tour(fl, as.l_tour = FALSE)
  expect_equal(length(l_path_name(p)), 1)
  expect_true(inherits(p, "l_plot"))
  proj <- l_getProjection(p, fl)
  expect_true(inherits(proj, "matrix"))

  # k-D space (k >= 3)
  p <- l_tour(fl, scaling = "sphere",
              tour_path = tourr::grand_tour(5))
  proj <- l_getProjection(p)
  w <- l_getPlots(p)
  expect_true(inherits(w, "l_serialaxes"))
  expect_true(inherits(proj, "matrix"))

  ########## facets
  ol <- olive[, -c(1,2)]
  p <- l_tour(ol,
              by = olive$region,
              tour_path = tourr::grand_tour(1))
  proj <- l_getProjection(p)
  expect_true(inherits(p, "l_tour_compound"))
  w <- l_getPlots(p)
  expect_true(inherits(w, "l_compound"))
  expect_true(inherits(w[[1]], "l_hist"))
  expect_true(inherits(proj, "matrix"))

  p <- l_tour(ol,
              by = olive$region,
              tour_path = tourr::grand_tour(2L))
  expect_true(inherits(p, "l_tour_compound"))
  w <- l_getPlots(p)
  expect_true(inherits(w, "l_compound"))
  expect_true(inherits(w[[1]], "l_plot"))

  p <- l_tour(ol,
              by = olive$region,
              tour_path = tourr::grand_tour(3L))
  expect_true(inherits(p, "l_tour_compound"))
  w <- l_getPlots(p)
  expect_true(inherits(w, "l_compound"))
  expect_true(inherits(w[[1]], "l_serialaxes"))
})

test_that("test tour pairs", {
  # scaling
  fl <- flea[, 1:6]
  p <- l_tour_pairs(fl, color = flea$species, showSerialAxes = TRUE)
  expect_equal(length(p['color']), 7) # test `l_cget.l_tour_compound`
  expect_true(inherits(p['projection'], "matrix"))
  expect_true(inherits(p['color'], "list"))
  expect_true(inherits(p['x2y1'], "l_plot"))
  p['color'] <- "red" # test `l_configure.l_tour_compound`
  # expect_equal(unique(unlist(p['color'])), loon::l_hexcolor("red"))

  p <- l_tour_pairs(fl, tour_path = tourr::grand_tour(3L),
                    as.l_tour = FALSE)
  p
  g <- loonGrob(p)
  proj <- l_getProjection(p, fl)
  expect_equal(length(proj), 3)
  expect_true(inherits(p, "l_pairs"))
  expect_true(is.na(all(unlist(p['projection']))))
  expect_false(l_isLoonWidget(p))
  expect_true(inherits(g, "gTree"))
})
