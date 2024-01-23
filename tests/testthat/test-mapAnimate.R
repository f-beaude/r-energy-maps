testthat::context("Test geographic maps - animated")

testthat::test_that("Animated maps - fill", {
  eneRgycache::initialise.cache()

  ## example data sets
  # WARNING : the data sets included in this package are not intended to be realistic, but rather to illustrate what kinds of displays may be achieved
  exampleDataDir <- eneRgymaps::example.data.dir()
  ElectricityPrices <- read.csv(file = file.path(exampleDataDir, "Prices.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  LFCAreasAssessment <- read.csv(file = file.path(exampleDataDir, "LFC_assessment.csv"), fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"))
  TradeData <- readxl::read_excel(path = file.path(exampleDataDir, "Trade.xlsx"))

  greenOrangeRed <- eneRgymaps::green.orange.red.palette (add.grey = FALSE)

  outputDir <- file.path (tempdir(), "test_data")
  dir.create(outputDir, showWarnings = FALSE)
  outputPath <- file.path (outputDir, "automated_test_fill")

  # standard animation with bidding-zone data
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                             fill.color.gradient.colors = greenOrangeRed,
                                             animation.variable.name = "year",
                                             legend.position = "right"); TRUE})

  # standard animation with country data
  testthat::expect_true ({eneRgymaps::animate.mapCountry(fill.data = TradeData, fill.id.field.name = "Country", fill.value.field.name = "Value",
                                                         fill.color.palette = "Blues",
                                                         animation.variable.name = "Year",
                                                         legend.position = "right"); TRUE})

  # standard animation with LFC area data
  testthat::expect_true ({eneRgymaps::animate.mapLFCArea(fill.data = LFCAreasAssessment, fill.id.field.name = "LFC.EIC", fill.value.field.name = "Value.1",
                                                         fill.colors.list = greenOrangeRed,
                                                         animation.variable.name = "year",
                                                         legend.position = "right"); TRUE})

  # customised time interval
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year",
                                                 animation.time.interval = 5,
                                                 legend.position = "right"); TRUE})

  # customised values order
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.variable.order = rev (unique (ElectricityPrices[["year"]])),
                                                 legend.position = "right"); TRUE})

  # with(out) animation caption
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", with.animation.caption = TRUE,
                                                 legend.position = "right"); TRUE})
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", with.animation.caption = FALSE,
                                                 legend.position = "right"); TRUE})

  # customised width/height
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.width = 750, animation.height = 500,
                                                 legend.position = "right"); TRUE})

  # customised float-number width/height
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.width = 751.3, animation.height = 500.15,
                                                 legend.position = "right"); TRUE})

  # gif export
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.format = "gif",
                                                 output.path = outputPath,
                                                 legend.position = "right"); TRUE})

  # mp4 export
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.format = "mp4",
                                                 output.path = outputPath,
                                                 legend.position = "right"); TRUE})

  # customised date values
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", date.variable.name = "year",
                                                 legend.position = "right"); TRUE})

  # the ordered field name is already in the data set (leading to create another field name)
  ElectricityPricesWithOrder <- ElectricityPrices %>%
    dplyr::mutate (`year_ordered_for_animation` = `year`)
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPricesWithOrder, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.variable.order = rev (unique (ElectricityPricesWithOrder[["year"]])),
                                                 legend.position = "right"); TRUE})

  # warning : NA time values
  naData <- ElectricityPrices %>%
    dplyr::mutate (`na_year` = NA)
  testthat::expect_warning (eneRgymaps::animate.mapBiddingZone(fill.data = naData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                    fill.color.gradient.colors = greenOrangeRed,
                                                    animation.variable.name = "na_year",
                                                    legend.position = "right"))

  # warning : save.plot not allowed
  testthat::expect_warning (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                   fill.color.gradient.colors = greenOrangeRed,
                                                   animation.variable.name = "year", save.plot = TRUE,
                                                   legend.position = "right"))

  # warning : save.data not allowed
  testthat::expect_warning (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                   fill.color.gradient.colors = greenOrangeRed,
                                                   animation.variable.name = "year", save.data = TRUE,
                                                   legend.position = "right"))

  # error : NULL animation.variable.name
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = NULL,
                                                 legend.position = "right"))

  # error : animation.variable.name not in data set
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                   fill.color.gradient.colors = greenOrangeRed,
                                                   animation.variable.name = "anotherYear",
                                                   legend.position = "right"))

  # error : the animation.variable.order has duplicate values
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.variable.order = c (rev (unique (ElectricityPrices[["year"]])), 2018),
                                                 legend.position = "right"))

  # error : the time interval is not numeric
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.time.interval = "five",
                                                 legend.position = "right"))

  # error : the time interval is negative
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.time.interval = -1,
                                                 legend.position = "right"))

  # error : the width is not numeric
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.width = "thousand",
                                                 legend.position = "right"))

  # error : the width is negative
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.width = -1,
                                                 legend.position = "right"))

  # error : the height is not numeric
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.height = "thousand",
                                                 legend.position = "right"))

  # error : the height is negative
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.height = -1,
                                                 legend.position = "right"))

  # error : bad animation format
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.format = "mp3",
                                                 legend.position = "right"))

  # error : asking for caption for both animation and static map
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 caption.text = "My map",
                                                 animation.variable.name = "year", with.animation.caption = TRUE,
                                                 legend.position = "right"))

  # error : variable order does not include all values
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", animation.variable.order = c (2018, 2016),
                                                 legend.position = "right"))

  # error: customised date field not in data set
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", date.variable.name = "AnotherYear",
                                                 legend.position = "right"))

  # error: date.variable.name defined along with date
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", date.variable.name = "year",
                                                 date = as.Date("2017-12-31"),
                                                 legend.position = "right"))

  # error: customised date field in bad format
  testthat::expect_error (eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.color.gradient.colors = greenOrangeRed,
                                                 animation.variable.name = "year", date.variable.name = "area_code",
                                                 legend.position = "right"))

})

testthat::test_that("Animated maps - border", {
  eneRgycache::initialise.cache()

  ## example data sets
  # WARNING : the data sets included in this package are not intended to be realistic, but rather to illustrate what kinds of displays may be achieved
  exampleDataDir <- eneRgymaps::example.data.dir()
  FlowsData <- readxl::read_xlsx(path = file.path(exampleDataDir, "Flows.xlsx"))

  outputDir <- file.path (tempdir(), "test_data")
  dir.create(outputDir, showWarnings = FALSE)
  outputPath <- file.path (outputDir, "automated_test_border")

  # standard animation
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(border.data = FlowsData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average",
                                                 border.shape.name = "arrow",
                                                 border.label.font.color = "red",
                                                 border.shape.color = "darkgoldenrod1",
                                                 border.flip.arrows.negative.values = TRUE,
                                                 border.label.rounding.ndigits = 0,
                                                 animation.variable.name = "year",
                                                 output.path = outputPath,
                                                 legend.position = "right"); TRUE})

})


testthat::test_that("Animated maps - combined", {
  eneRgycache::initialise.cache()

  ## example data sets
  # WARNING : the data sets included in this package are not intended to be realistic, but rather to illustrate what kinds of displays may be achieved
  exampleDataDir <- eneRgymaps::example.data.dir()
  ElectricityPrices <- read.csv(file = file.path(exampleDataDir, "Prices.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  FlowsData <- readxl::read_xlsx(path = file.path(exampleDataDir, "Flows.xlsx"))

  greenOrangeRed <- eneRgymaps::green.orange.red.palette (add.grey = FALSE)

  outputDir <- file.path (tempdir(), "test_data")
  dir.create(outputDir, showWarnings = FALSE)
  outputPath <- file.path (outputDir, "automated_test_combined")

  # standard animation
  testthat::expect_true ({eneRgymaps::animate.mapBiddingZone(fill.data = ElectricityPrices, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                 fill.legend.name = "Electricity price", fill.color.gradient.colors = greenOrangeRed,
                                                 fill.color.min.value = 30, fill.color.max.value = 60,
                                                 border.data = FlowsData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average",
                                                 border.shape.name = "arrow",
                                                 border.label.font.color = "red",
                                                 border.shape.color = "darkgoldenrod1",
                                                 border.flip.arrows.negative.values = TRUE,
                                                 border.label.rounding.ndigits = 0,
                                                 animation.variable.name = "year",
                                                 output.path = outputPath,
                                                 legend.position = "right"); TRUE})
})
