testthat::context("Test help")

testthat::test_that("Help functions", {
  testthat::expect_true({base::suppressMessages(eneRgymaps::help.path.linetypes()); TRUE})

  testthat::expect_true({eneRgymaps::help.shapes(); TRUE})
})
