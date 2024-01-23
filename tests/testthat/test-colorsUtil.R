testthat::context("Test palette utility functions")

testthat::test_that("Testing palette size", {
  colorPalettesDF <- RColorBrewer::brewer.pal.info
  for (i in 1:length(colorPalettesDF)) {
    paletteName <- rownames (colorPalettesDF) [i]
    expectedSize <- colorPalettesDF$maxcolors [i]
    testthat::expect_equal (eneRgymaps::palette.length (palette = paletteName), expectedSize)
  }

  # size of palette as list of colours
  myUserDefinedPalette <- c ("#b30011", "#2db9be", "#fdf893", "#f67d5b")
  testthat::expect_equal (eneRgymaps::palette.length (palette = myUserDefinedPalette), length (myUserDefinedPalette))


  # error : unknown palette
  testthat::expect_error (eneRgymaps::palette.length (palette = "MyOwnPalette"))

})


testthat::test_that("Testing palette resizing", {
  colorPalettesDF <- RColorBrewer::brewer.pal.info
  for (i in 1:length(colorPalettesDF)) {
    paletteName <- rownames (colorPalettesDF) [i]
    paletteSize <- colorPalettesDF$maxcolors [i]

    expectedPalette <- RColorBrewer::brewer.pal(n = paletteSize, name = paletteName)

    testthat::expect_equal (eneRgymaps::palette.resize (palette = paletteName, length = paletteSize), expectedPalette)

    largerPaletteSize <- paletteSize + 15
    largerPalette <- eneRgymaps::palette.resize (palette = paletteName, length = largerPaletteSize)
    testthat::expect_equal (eneRgymaps::palette.length (palette = largerPalette), largerPaletteSize)

    smallerPaletteSize <- paletteSize - 1
    smallerPalette <- eneRgymaps::palette.resize (palette = paletteName, length = smallerPaletteSize)
    testthat::expect_equal (eneRgymaps::palette.length (palette = smallerPalette), smallerPaletteSize)
  }

  myUserDefinedPalette <- c ("#b30011", "#2db9be", "#fdf893", "#f67d5b")
  updatedUDPaletteSize <- 10
  updatedUDPalette <- eneRgymaps::palette.resize (palette = myUserDefinedPalette, length = updatedUDPaletteSize)
  testthat::expect_equal (eneRgymaps::palette.length (palette = updatedUDPalette), updatedUDPaletteSize)

})


testthat::test_that("Testing color palette", {

  myLength <- 5
  myPal <- eneRgymaps::palette.colors.sample (length = myLength)
  testthat::expect_equal (eneRgymaps::palette.length (palette = myPal), myLength)

  testthat::expect_true ({myPalette <- eneRgymaps::palette.colors.sample (length = 0); myPalette; TRUE;})
  testthat::expect_error (eneRgymaps::palette.colors.sample (length = -1))

  rog <- eneRgymaps::red.orange.green.palette(add.grey = FALSE)
  testthat::expect_equal (eneRgymaps::palette.length (palette = rog), 3)
  rogg <- eneRgymaps::red.orange.green.palette(add.grey = TRUE)
  testthat::expect_equal (eneRgymaps::palette.length (palette = rogg), 4)

  gor <- eneRgymaps::green.orange.red.palette(add.grey = FALSE)
  testthat::expect_equal (eneRgymaps::palette.length (palette = gor), 3)
  gorg <- eneRgymaps::green.orange.red.palette(add.grey = TRUE)
  testthat::expect_equal (eneRgymaps::palette.length (palette = gorg), 4)

  testthat::expect_equal (gor, rev (rog))
})


testthat::test_that("Testing ACER colors palette", {

  myACERPal1 <- eneRgymaps::acer.colors.palette (add.greys = FALSE)
  testthat::expect_equal (eneRgymaps::palette.length (palette = myACERPal1), 4)
  testthat::expect_true (all(eneRgymaps::valid.colors(myACERPal1)))

  myACERPal2 <- eneRgymaps::acer.colors.palette (add.greys = TRUE)
  testthat::expect_equal (eneRgymaps::palette.length (palette = myACERPal2), 6)
  testthat::expect_true (all(eneRgymaps::valid.colors(myACERPal2)))
})


testthat::test_that("Testing color validity", {
  testthat::expect_equal (TRUE, eneRgymaps::valid.color ("blue"))
  testthat::expect_equal (TRUE, eneRgymaps::valid.color ("forestgreen"))
  testthat::expect_equal (TRUE, eneRgymaps::valid.color ("Red"))
  testthat::expect_equal (TRUE, eneRgymaps::valid.color ("RED"))
  testthat::expect_equal (TRUE, eneRgymaps::valid.color ("#FF2400FF"))

  testthat::expect_equal (FALSE, eneRgymaps::valid.color ("myUndefinedColor"))
  testthat::expect_equal (FALSE, eneRgymaps::valid.color ("#AABBCCDDEE"))
  testthat::expect_equal (FALSE, eneRgymaps::valid.color ("#ZZ0000ZZ"))
})


testthat::test_that("Testing colors validity", {
  testthat::expect_equal (TRUE, eneRgymaps::valid.colors ("blue"))
  testthat::expect_equal (FALSE, eneRgymaps::valid.colors ("hidden color"))
  testthat::expect_equal (c (TRUE, TRUE, FALSE, FALSE, TRUE), eneRgymaps::valid.colors (c ("blue", "Purple", "NoIdea", "ZZ1530FF", "YELLOW")))
  testthat::expect_warning({eneRgymaps::valid.colors (colors = c ("blue", "Purple", "NoIdea", "ZZ1530FF", "YELLOW"), silent = FALSE)})
})
