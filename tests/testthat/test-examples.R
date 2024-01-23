testthat::context("Test examples")

testthat::test_that("Testing static map examples", {

  examplesDir <- eneRgymaps::example.code.dir()
  testthat::expect_true ({base::suppressWarnings(base::source (file = file.path(examplesDir, "maps.R"), local = TRUE),
                                                 classes = c("message", "warning")); TRUE})
  # need to set examplesDir again due to R issues...
  examplesDir <- eneRgymaps::example.code.dir()
  testthat::expect_true ({base::suppressWarnings(base::source (file = file.path(examplesDir, "maps_border.R"), local = TRUE),
                                                 classes = c("message", "warning")); TRUE})
  examplesDir <- eneRgymaps::example.code.dir()
  testthat::expect_true ({base::suppressWarnings(base::source (file = file.path(examplesDir, "maps_fill.R"), local = TRUE),
                                                 classes = c("message", "warning")); TRUE})
  examplesDir <- eneRgymaps::example.code.dir()
  testthat::expect_true ({base::suppressWarnings(base::source (file = file.path(examplesDir, "maps_combined.R"), local = TRUE),
                                                 classes = c("message", "warning")); TRUE})
  examplesDir <- eneRgymaps::example.code.dir()
  testthat::expect_true ({base::suppressWarnings(base::source (file = file.path(examplesDir, "maps_custom.R"), local = TRUE),
                                                 classes = c("message", "warning")); TRUE})
})

testthat::test_that("Testing animated map examples", {

  examplesDir <- eneRgymaps::example.code.dir()
  testthat::expect_true ({base::suppressWarnings(base::source (file = file.path(examplesDir, "animations.R"), local = TRUE),
                                                 classes = c("message", "warning")); TRUE})
})
