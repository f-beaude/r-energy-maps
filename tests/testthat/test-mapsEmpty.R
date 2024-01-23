testthat::context("Test geographic maps - empty")

testthat::test_that("Empty maps", {
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = NULL,
                                    border.data = NULL,

                                    save.plot = FALSE,
                                    save.data = FALSE))

  testthat::expect_error (eneRgymaps::mapControlArea(fill.data = NULL,
                                                     border.data = NULL,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  testthat::expect_error (eneRgymaps::mapCountry(fill.data = NULL,
                                                     border.data = NULL,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))
})
