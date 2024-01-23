testthat::context("Test geographic maps - combined")

testthat::test_that("Combined filled/border maps - bidding zone", {
  fillDataPath <- file.path(eneRgymaps::example.data.dir(), "Prices.csv")
  fillData <- read.csv(file = fillDataPath, fileEncoding = "UTF-8") %>%
    dplyr::filter (`year` == 2017)

  borderDataPath <- file.path(eneRgymaps::example.data.dir(), "Flows.xlsx")
  borderData <- readxl::read_excel (path = borderDataPath) %>%
    dplyr::filter (`year` == 2017)

  tmpSaveDir <- file.path (tempdir(), "saved_data")
  dir.create(tmpSaveDir, showWarnings = FALSE) # will do nothing if the directory already exists

  eneRgycache::initialise.cache()

  # combined map with border labels
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",
                                               fill.label.first.field.name = "price",
                                               fill.label.second.field.name = "percentage_change",

                                               border.data = borderData,
                                               border.id.field.name = "border",
                                               border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                               border.label.value.field.name = "Average",
                                               border.shape.color = "Yellow",
                                               border.shape.name = "arrow",

                                               save.plot = FALSE,
                                               save.data = FALSE); combinedMap; TRUE})

  # combined maps with border colors (palette)
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.color.value.field.name = "Average",
                                                   border.color.palette = "YlOrRd",
                                                   border.shape.name = "arrow",

                                                   save.plot = FALSE,
                                                   save.data = FALSE); combinedMap; TRUE})

  # combined maps with border colors (gradient)
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.color.value.field.name = "Average",
                                                   border.color.gradient.colors = c ("yellow", "red"),
                                                   border.shape.name = "arrow",

                                                   save.plot = FALSE,
                                                   save.data = FALSE); combinedMap; TRUE})

  # combined maps with border colors (gradient) and label
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.label.value.field.name = "Average",
                                                   border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                   border.color.gradient.colors = c ("yellow", "red"),
                                                   border.shape.name = "arrow",

                                                   save.plot = FALSE,
                                                   save.data = FALSE); combinedMap; TRUE})

  # combined maps with border widths and label
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.label.value.field.name = "Average",
                                                   border.width.value.field.name = "Average", border.legend.width.name = "Average flows",
                                                   border.shape.color = "green",
                                                   border.shape.name = "arrow",

                                                   save.plot = FALSE,
                                                   save.data = FALSE); combinedMap; TRUE})

  # combined maps with border widths and label, saving plot and data
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.label.value.field.name = "Average",
                                                   border.width.value.field.name = "Average", border.legend.width.name = "Average flows",
                                                   border.shape.color = "green",
                                                   border.shape.name = "arrow",

                                                   output.directory = tmpSaveDir, map.name = "test combined map", save.plot = TRUE,
                                                   save.data = TRUE); combinedMap; TRUE})

  # combined maps with border widths and label, saving plot and data (without map name)
  testthat::expect_true ({combinedMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.label.value.field.name = "Average",
                                                   border.width.value.field.name = "Average", border.legend.width.name = "Average flows",
                                                   border.shape.color = "green",
                                                   border.shape.name = "arrow",

                                                   output.directory = tmpSaveDir, save.plot = TRUE,
                                                   save.data = TRUE); combinedMap; TRUE})

  # error : asked to save plot and data, but no information about output directory
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                   fill.value.field.name = "price",
                                                   fill.legend.name = "Electricity price",
                                                   fill.color.palette = "Blues",
                                                   fill.label.first.field.name = "price",
                                                   fill.label.second.field.name = "percentage_change",

                                                   border.data = borderData,
                                                   border.id.field.name = "border",
                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                   border.label.value.field.name = "Average",
                                                   border.width.value.field.name = "Average", border.legend.width.name = "Average flows",
                                                   border.shape.color = "green",
                                                   border.shape.name = "arrow",

                                                   save.plot = TRUE,
                                                   save.data = TRUE))
})
