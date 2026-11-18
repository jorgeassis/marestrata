test_that("an ordinary extent is clamped to the global grid", {
  windows <- marestrata:::.extent_windows(c(-190, 10, -100, 20))
  expect_length(windows, 1)
  expect_equal(windows[[1]]$xmin, -180)
  expect_equal(windows[[1]]$xmax, 10)
  expect_equal(windows[[1]]$ymin, -90)
  expect_equal(windows[[1]]$ymax, 20)
  expect_equal(windows[[1]]$shift, 0)
})

test_that("an extent that crosses the antimeridian becomes two windows", {
  windows <- marestrata:::.extent_windows(c(170, -170, -10, 10))
  expect_length(windows, 2)
  expect_equal(windows[[1]]$xmin, 170)
  expect_equal(windows[[1]]$xmax, 180)
  expect_equal(windows[[1]]$shift, 0)
  expect_equal(windows[[2]]$xmin, -180)
  expect_equal(windows[[2]]$xmax, -170)
  expect_equal(windows[[2]]$shift, 360)
  expect_equal(windows[[2]]$ymin, -10)
  expect_equal(windows[[2]]$ymax, 10)
})

test_that("extents outside the grid are rejected", {
  expect_error(marestrata:::.extent_windows(c(-200, -190, 0, 1)), "overlap")
  expect_error(marestrata:::.extent_windows(c(0, 10, 91, 95)), "overlap")
  expect_error(marestrata:::.extent_windows(c(0, 10, 10, 0)), "ymin")
})

test_that("a SpatExtent is accepted", {
  windows <- marestrata:::.extent_windows(terra::ext(-15, -5, 35, 45))
  expect_length(windows, 1)
  expect_equal(windows[[1]]$xmin, -15)
  expect_equal(windows[[1]]$xmax, -5)
  expect_equal(windows[[1]]$ymin, 35)
  expect_equal(windows[[1]]$ymax, 45)
})
