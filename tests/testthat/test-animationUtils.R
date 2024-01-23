testthat::context("Test geographic maps - animation utilities")

testthat::test_that("Animation formats", {

  testthat::expect_equal(eneRgymaps::valid.animation.formats(), c("gif", "mp4"))

  # animation formats allowed
  testthat::expect_equal(eneRgymaps::animation.format.is.valid("gif"), TRUE)
  testthat::expect_equal(eneRgymaps::animation.format.is.valid("mp4"), TRUE)
  testthat::expect_equal(eneRgymaps::animation.format.is.valid("mov"), FALSE)
  testthat::expect_equal(eneRgymaps::animation.format.is.valid(c("gif", "mp4", "tmp")), c(TRUE, TRUE, FALSE))
})
