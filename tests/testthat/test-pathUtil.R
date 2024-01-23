testthat::context("Test path utilities")

testthat::test_that("Current package directories", {
  testthat::expect_true (dir.exists (eneRgymaps::maps.coordinates.dir()))
  testthat::expect_equal (basename  (eneRgymaps::maps.coordinates.dir()), "geographic")
  testthat::expect_true (dir.exists (eneRgymaps::example.data.dir()))
  testthat::expect_equal (basename  (eneRgymaps::example.data.dir()), "examples")
  testthat::expect_true (dir.exists (eneRgymaps::example.code.dir()))
  testthat::expect_equal (basename  (eneRgymaps::example.code.dir()), "examples")
})
