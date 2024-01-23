testthat::context("Test geographic coordinates")

testthat::test_that("Uniform geographic interval", {
  testthat::expect_equal (eneRgymaps::uniform.geographic.interval (100), list ("longitude.min" = 100, "longitude.max" = 100,
                                                                               "latitude.min" = 100, "latitude.max" = 100))

  testthat::expect_equal (eneRgymaps::uniform.geographic.interval (0), list ("longitude.min" = 0, "longitude.max" = 0,
                                                                               "latitude.min" = 0, "latitude.max" = 0))

  testthat::expect_equal (eneRgymaps::uniform.geographic.interval (-159.314), list ("longitude.min" = -159.314, "longitude.max" = -159.314,
                                                                               "latitude.min" = -159.314, "latitude.max" = -159.314))

  testthat::expect_equal (eneRgymaps::uniform.geographic.interval (NA), list ("longitude.min" = NA, "longitude.max" = NA,
                                                                               "latitude.min" = NA, "latitude.max" = NA))


  testthat::expect_error (eneRgymaps::uniform.geographic.interval ("zero"))

})

testthat::test_that("Geographic coordinates check", {
  goodCoordinates1 <- list ("latitude.min" = 4663033.31031558, "latitude.max" = 5837927.18190134,
                            "longitude.min" = -483833.833770226, "longitude.max" = 352007.374878592)

  goodCoordinates2 <- list ("latitude.min" = 4663033, "latitude.max" = 5837927,
                            "longitude.min" = 0, "longitude.max" = 352007)

  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = goodCoordinates1, check.bounds.consistency = TRUE), TRUE)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = goodCoordinates1, check.bounds.consistency = FALSE), TRUE)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = goodCoordinates2, check.bounds.consistency = TRUE), TRUE)

  # error : one forbidden key
  badCoordinate1 <- list ("latitude.min" = 4663033, "latitude.max" = 5837927,
                         "longitude.min" = 0, "longitude.max" = 352007, "altitude" = 500)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = badCoordinate1, check.bounds.consistency = TRUE), FALSE)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = badCoordinate1, check.bounds.consistency = FALSE), FALSE)

  # error : missing required key
  badCoordinate2 <- list ("latitude.min" = 4663033, "latitude.max" = 5837927,
                          "longitude.min" = 0)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (badCoordinate2), FALSE)

  # error : NA in values
  badCoordinate3 <- list ("latitude.min" = 4663033, "latitude.max" = 5837927,
                          "longitude.min" = 0, "longitude.max" = NA)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (badCoordinate3), FALSE)

  # error : non-numeric values
  badCoordinate4 <- list ("latitude.min" = 4663033, "latitude.max" = 5837927,
                          "longitude.min" = 0, "longitude.max" = "thousand")
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (badCoordinate4), FALSE)

  # error : min > max latitude
  badCoordinate5 <- list ("latitude.min" = 5837927, "latitude.max" = 4663033,
                          "longitude.min" = 0, "longitude.max" = 5000)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = badCoordinate5, check.bounds.consistency = TRUE), FALSE)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = badCoordinate5, check.bounds.consistency = FALSE), TRUE)

  # error : min > max longitude
  badCoordinate6 <- list ("latitude.min" = 4663033, "latitude.max" = 5837927,
                          "longitude.min" = 352007, "longitude.max" = 0)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = badCoordinate6, check.bounds.consistency = TRUE), FALSE)
  testthat::expect_equal(eneRgymaps::geographic.interval.well.defined (coordinates = badCoordinate6, check.bounds.consistency = FALSE), TRUE)
})


testthat::test_that("Aspect ratio correction", {
  aspectMin <- 0.75
  aspectMax <- 1.5
  coordinatesNothingToDo1 <- list ("latitude.min" = 0, "latitude.max" = 1000,
                                  "longitude.min" = 0, "longitude.max" = 1250)


  coordinatesNothingToDo2 <- list ("latitude.min" = 0, "latitude.max" = 1000,
                                   "longitude.min" = 0, "longitude.max" = 800)

  testthat::expect_equal(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesNothingToDo1, aspect.ratio.min = aspectMin, aspect.ratio.max = aspectMax),
                         coordinatesNothingToDo1)
  testthat::expect_equal(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesNothingToDo2, aspect.ratio.min = aspectMin, aspect.ratio.max = aspectMax),
                         coordinatesNothingToDo2)

  coordinatesToIncreaseWidth <- list ("latitude.min" = 0, "latitude.max" = 1000,
                                      "longitude.min" = 0, "longitude.max" = 100)
  testthat::expect_equal(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesToIncreaseWidth, aspect.ratio.min = aspectMin, aspect.ratio.max = aspectMax),
                         list ("latitude.min" = 0, "latitude.max" = 1000,
                               "longitude.min" = -325, "longitude.max" = 425))

  coordinatesToIncreaseHeight <- list ("latitude.min" = 0, "latitude.max" = 100,
                                       "longitude.min" = 0, "longitude.max" = 1500)
  testthat::expect_equal(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesToIncreaseHeight, aspect.ratio.min = aspectMin, aspect.ratio.max = aspectMax),
                         list ("latitude.min" = -450, "latitude.max" = 550,
                               "longitude.min" = 0, "longitude.max" = 1500))

  # aspect.ratio.min = aspect.ratio.max
  testthat::expect_equal(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesToIncreaseHeight, aspect.ratio.min = aspectMax, aspect.ratio.max = aspectMax),
                         list ("latitude.min" = -450, "latitude.max" = 550,
                               "longitude.min" = 0, "longitude.max" = 1500))

  # error: aspect.ratio.min is not numeric
  testthat::expect_error(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesNothingToDo1, aspect.ratio.min = "zero", aspect.ratio.max = aspectMax))

  # error: aspect.ratio.max is not numeric
  testthat::expect_error(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesNothingToDo1, aspect.ratio.min = aspectMin, aspect.ratio.max = "three"))

  # error: aspect.ratio.min > aspect.ratio.max
  testthat::expect_error(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesNothingToDo1, aspect.ratio.min = aspectMax, aspect.ratio.max = aspectMin))

  # error: aspect.ratio.min < 0
  testthat::expect_error(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesNothingToDo1, aspect.ratio.min = -0.1, aspect.ratio.max = aspectMax))

  # error: coordinates [["latitude.max"]] = coordinates [["latitude.min"]]
  coordinatesSameLatitude <- list ("latitude.min" = 50, "latitude.max" = 50,
                                   "longitude.min" = 0, "longitude.max" = 1500)
  testthat::expect_error(eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates = coordinatesSameLatitude, aspect.ratio.min = aspectMin, aspect.ratio.max = aspectMax))
})
