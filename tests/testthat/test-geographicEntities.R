testthat::context("Test geographic entities")

testthat::test_that("Checking geographic entity name", {
  testthat::expect_true (eneRgymaps::is.relevant.geographic.entity.name("Bidding zone"))
  testthat::expect_true (eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name = "Control area"))
  testthat::expect_true (eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name = "Country"))
  testthat::expect_false (eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name = "My bed"))
})

testthat::test_that("Testing reverse border name", {
  testthat::expect_equal (eneRgymaps::reverse.border.name (border.name = "BE-FR", separator = "-"), "FR-BE")
  testthat::expect_equal (eneRgymaps::reverse.border.name (border.name = "BE - FR", separator = " - "), "FR - BE")
  testthat::expect_equal (eneRgymaps::reverse.border.name (border.name = "BE - FR", separator = "-"), " FR-BE ")
  testthat::expect_equal (eneRgymaps::reverse.border.name (border.name = "DE > NL", separator = " > "), "NL > DE")

  # vector of border names
  testthat::expect_equal (eneRgymaps::reverse.border.name (border.name = c("DE > NL", "DE > BE"), separator = " > "), c("NL > DE", "BE > DE"))

  # vector of both border names and separators
  testthat::expect_equal (eneRgymaps::reverse.border.name (border.name = c("DE > NL", "DE - BE"), separator = c(" > ", " - ")), c("NL > DE", "BE - DE"))

  # error : the separator is not in the border.name
  testthat::expect_error (eneRgymaps::reverse.border.name (border.name = c("DE > NL"), separator = " - "))
  testthat::expect_error (eneRgymaps::reverse.border.name (border.name = c("DE > NL", "DE - BE"), separator = " > "))
})

testthat::test_that("Testing predefined EIC codes", {
  testthat::expect_equal (eneRgymaps::eic.code.AT(), "10YAT-APG------L")
  testthat::expect_equal (eneRgymaps::eic.code.DELU(), "10Y1001A1001A82H")
  testthat::expect_equal (eneRgymaps::eic.code.merged.AT.DE(), "10Y1001A1001A63L")
  testthat::expect_equal (eneRgymaps::eic.code.LU.TSO(), "10YLU-CEGEDEL-NQ")
  testthat::expect_equal (sort(eneRgymaps::eic.codes.split.AT.DE()), sort(c(eneRgymaps::eic.code.AT(), eneRgymaps::eic.code.DELU())))
  testthat::expect_equal (sort(eneRgymaps::eic.codes.DE.TSOs()), sort(c("10YDE-VE-------2", "10YDE-RWENET---I", "10YDE-EON------1", "10YDE-ENBW-----N")))
})

testthat::test_that("Testing whether the split or merged AT DE bidding-zones are included in a data set", {

  # neither merged nor split bidding-zones declared
  none <- c ("10YFR-RTE------C", "10YES-REE------0", "10YPT-REN------W")
  testNone <- eneRgymaps::merged.split.AT.DE.in.data (none)
  testthat::expect_equal(testNone, list (split = FALSE, merged = FALSE))

  # merged AT-DE bidding-zone declared
  merged1 <- c (eneRgymaps::eic.code.merged.AT.DE(), "10YGB----------A")
  testMerged1 <- eneRgymaps::merged.split.AT.DE.in.data (merged1)
  testthat::expect_equal(testMerged1, list (split = FALSE, merged = TRUE))

  # split AT bidding-zone declared
  splitAT <- c ("10YPT-REN------W", eneRgymaps::eic.code.AT())
  testSplitAT <- eneRgymaps::merged.split.AT.DE.in.data (splitAT)
  testthat::expect_equal(testSplitAT, list (split = TRUE, merged = FALSE))

  # split DE bidding-zone declared
  splitDEBZ <- c ("10YES-REE------0", "10YPT-REN------W", eneRgymaps::eic.code.DELU())
  testSplitDEBZ <- eneRgymaps::merged.split.AT.DE.in.data (splitDEBZ)
  testthat::expect_equal(testSplitDEBZ, list (split = TRUE, merged = FALSE))

  # both split AT and DE declared
  splitATDE <- c ("10YES-REE------0", "10YPT-REN------W",
                  eneRgymaps::eic.code.DELU(), eneRgymaps::eic.code.AT())
  testSplitATDE <- eneRgymaps::merged.split.AT.DE.in.data (splitATDE)
  testthat::expect_equal(testSplitATDE, list (split = TRUE, merged = FALSE))

  # both split and merged bidding-zones declared
  splitMerge <- c ("10YES-REE------0", "10YPT-REN------W",
                   eneRgymaps::eic.code.DELU(), eneRgymaps::eic.code.AT(), eneRgymaps::eic.code.merged.AT.DE())
  testSplitMerge <- eneRgymaps::merged.split.AT.DE.in.data (splitMerge)
  testthat::expect_equal(testSplitMerge, list (split = TRUE, merged = TRUE))
})
