testthat::context("Test geographic maps - fill")

testthat::test_that("Fill maps - bidding zone", {
  fillDataPath <- file.path(eneRgymaps::example.data.dir(), "Prices.csv")
  fillData <- read.csv(file = fillDataPath, fileEncoding = "UTF-8", stringsAsFactors = FALSE) %>%
    dplyr::filter (`year` == 2017) %>%
    dplyr::mutate(`shape` = dplyr::if_else (`discrete_value` == "medium", 16, dplyr::if_else (`discrete_value` == "high", 17, 18)))
  fillDataManyYears <- read.csv(file = fillDataPath, fileEncoding = "UTF-8", stringsAsFactors = FALSE)
  fillBZData <- read.csv(file = file.path(eneRgymaps::example.data.dir(), "Bidding_zones.csv"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)

  eneRgycache::initialise.cache()

  # Map with variable used to fill bidding zones
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and legend displayed on the left of the map)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                legend.position = "left",
                                                                date = as.Date("2017-08-15"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and title and caption)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                title.text = "Electricity prices", title.size = 15, title.color = "blue",
                                                                caption.text = "eneRgymaps, 2017",

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and legend displayed on the top-right corner of the map)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                legend.position = c (1, 1),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and legend displayed on top of the map, close to the bottom left)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                legend.position = c (0.25, 0.25),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and country labels)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                geographic.entity.to.label = "Country",

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and country labels for both EU and EFTA countries)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                geographic.entity.to.label = "Country",
                                                                geographic.entity.to.label.political.initiatives = c ("EFTA", "EU"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and bidding zone labels written in red for EU countries)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                geographic.entity.to.label = "Bidding zone",
                                                                geographic.entity.label.font.color = "red",
                                                                geographic.entity.to.label.political.initiatives = c ("EU"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and control area labels written in yellow for EC countries)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                geographic.entity.to.label = "Control area",
                                                                geographic.entity.label.font.color = "yellow",
                                                                geographic.entity.to.label.political.initiatives = c ("EC"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (no legend)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.color.palette = "Blues",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and color gradient
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.color.gradient.colors = c ("green", "yellow", "red"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones, color gradient, and color gradient percentiles
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.color.gradient.colors = c ("green", "yellow", "red"),
                                                                fill.color.gradient.perc.values = c (0, 0.2, 1),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and one label
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",
                                               fill.label.first.field.name = "price",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and one label with frame
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.label.first.field.name = "price",
                                                                fill.label.with.frame = TRUE,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and one label without frame (same as default)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.label.first.field.name = "price",
                                                                fill.label.with.frame = FALSE,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and a color list
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData %>% mutate(`country` = as.factor(`country`)), fill.id.field.name = "area_code",
                                               fill.value.field.name = "country",
                                               fill.legend.name = "Electricity price",
                                               fill.colors.list = c ("red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow"),
                                               fill.label.first.field.name = "price",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and a color list (as.factor conversion conducted internally)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "country",
                                               fill.legend.name = "Electricity price",
                                               fill.colors.list = c ("red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow"),
                                               fill.label.first.field.name = "price",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})


  # Map with variable used to fill bidding zones and a color list (fill.colors.list larger than number of distinct values)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "country",
                                               fill.legend.name = "Electricity price",
                                               fill.colors.list = c ("red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                     "red", "yellow", "blue", "orange", "green", "purple", "black", "white"),
                                               fill.label.first.field.name = "price",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and two labels
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",
                                               fill.label.first.field.name = "price",
                                               fill.label.second.field.name = "percentage_change",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones and twice the same label
  testthat::expect_warning ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.label.first.field.name = "price",
                                                                fill.label.second.field.name = "price",

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with no variable used to fill bidding zones and one label
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.label.first.field.name = "price",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with no variable used to fill bidding zones and two labels
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.label.first.field.name = "price",
                                               fill.label.second.field.name = "percentage_change",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with no variable used to fill bidding zones and one label (written in red in a customised size)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.label.first.field.name = "price",
                                               fill.label.font.color = "Red", fill.label.font.size = 10,

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and NA in yellow)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",
                                               fill.na.color = "Yellow",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and borders displayed in yellow)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",
                                               fill.borders.color  = "yellow",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and borders hidden by fill.borders.color)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",
                                               fill.borders.color  = NULL,

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and borders hidden by fill.borders.width)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.borders.width  = 0,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and borders displayed more widely)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.borders.width  = 3,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and minimum and maximum color values set for palette)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.color.min.value = 40,
                                                                fill.color.max.value = 100,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and palette with user-defined color labels)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "BZN_vs_country",
                                                                fill.legend.name = "BZN_vs_country",
                                                                fill.legend.keys = c ("equal", "larger", "smaller"), fill.legend.values = c ("EQ", "LG", "SM"),
                                                                fill.colors.list = c ("blue", "orange", "green"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and minimum and maximum color values set for gradient), along with user-defined frame
  customFrameCoordinates <- list ("longitude.min" = -15, "longitude.max" = 25,
                                  "latitude.min" = 65, "latitude.max" = 80)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.gradient.colors = c ("green", "red"),
                                                                fill.color.min.value = 40,
                                                                fill.color.max.value = 100,

                                                                manual.frame.coordinates = customFrameCoordinates,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones, along with user-defined margins (on top of automated coordinates)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.gradient.colors = c ("green", "red"),

                                                                automated.frame.margins = list ("latitude.min" = 100, "latitude.max" = 1000,
                                                                                                      "longitude.min" = -10, "longitude.max" = 1250),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones, along with user-defined min/max aspect ratio
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.gradient.colors = c ("green", "red"),

                                                                aspect.ratio.min = 1,
                                                                aspect.ratio.max = 1,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with variable used to fill bidding zones (and minimum and maximum color values and percentiles set for gradient)
  # warning due to the fact that fill.color.gradient.perc.values does not cover the full [0 ; 1] range
  testthat::expect_warning ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.gradient.colors = c ("green", "yellow", "red"),
                                                                fill.color.gradient.perc.values = c (0.1, 0.9, 1),
                                                                fill.color.min.value = 40,
                                                                fill.color.max.value = 100,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with separate AT and DE data
  fillDataATDE <- fillData %>%
    dplyr::mutate(`area_code` = dplyr::if_else(`area_code` == eneRgymaps::eic.code.merged.AT.DE(), eneRgymaps::eic.code.AT(), `area_code`),
                         `area_code` = dplyr::if_else(`area_code` == "10YRO-TEL------P", eneRgymaps::eic.code.DELU(), `area_code`))
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillDataATDE, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap; TRUE})

  # warning : fill.legend.keys and fill.legend.values defined without fill.legend.name
  testthat::expect_warning ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "BZN_vs_country",
                                                                fill.legend.keys = c ("equal", "larger", "smaller"), fill.legend.values = c ("EQ", "LG", "SM"),
                                                                fill.colors.list = c ("blue", "orange", "green"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with fixed shapes on top of filling
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                fill.center.shape.name = 17,
                                                                fill.center.shape.color = "red",
                                                                fill.center.shape.width = 3,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with fixed shapes (without filling)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",

                                                                fill.center.shape.name = 17,
                                                                fill.center.shape.color = "red",
                                                                fill.center.shape.width = 3,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with dynamic shapes (without filling)
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",

                                                                fill.center.shape.value.field.name = "shape",
                                                                fill.center.shape.color = "red",
                                                                fill.center.shape.width = 3,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with dynamic shapes on top of filling
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                fill.center.shape.value.field.name = "shape",
                                                                fill.center.shape.color = "red",
                                                                fill.center.shape.width = 3,

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with dynamic shapes (including dynamic color and width) on top of filling
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                fill.center.shape.value.field.name = "shape",
                                                                fill.center.shape.color.field.name = "shape",
                                                                fill.center.shape.width = 3,
                                                                fill.center.shape.colors.list = c ("orange", "yellow", "green"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # Map with dynamic shapes (including dynamic color and width) on top of filling, with center shape legend
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                fill.center.shape.value.field.name = "shape",
                                                                fill.center.shape.color.field.name = "shape",
                                                                fill.center.shape.width = 2.5,
                                                                fill.center.shape.colors.list = c ("orange", "yellow", "green"),
                                                                fill.legend.center.shape.name = "Competition level",
                                                                fill.legend.center.shape.keys = c (16, 17, 18),
                                                                fill.legend.center.shape.values = c ("medium", "high", "very high"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # keep NAs in legend
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                fill.center.shape.value.field.name = "shape",
                                                                fill.center.shape.color.field.name = "shape",
                                                                fill.center.shape.width = 2.5,
                                                                fill.center.shape.colors.list = c ("orange", "yellow", "green"),
                                                                fill.legend.center.shape.name = "Competition level",
                                                                fill.legend.center.shape.keep.na = TRUE,
                                                                fill.legend.center.shape.keys = c (16, 17, 18),
                                                                fill.legend.center.shape.values = c ("medium", "high", "very high"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # remove NAs from legend
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                fill.center.shape.value.field.name = "shape",
                                                                fill.center.shape.color.field.name = "shape",
                                                                fill.center.shape.width = 2.5,
                                                                fill.center.shape.colors.list = c ("orange", "yellow", "green"),
                                                                fill.legend.center.shape.name = "Competition level",
                                                                fill.legend.center.shape.keep.na = FALSE,
                                                                fill.legend.center.shape.keys = c (16, 17, 18),
                                                                fill.legend.center.shape.values = c ("medium", "high", "very high"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})

  # map with legend values as percentages
  testthat::expect_true ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.legend.values.as.percentages = TRUE,
                                                                fill.color.palette = "Blues",

                                                                save.plot = FALSE,
                                                                save.data = FALSE); fillMap; TRUE})


  # error : both AT/DE and DE defined
  fillDataATDEError <- fillData %>%
    dplyr::mutate(`area_code` = dplyr::if_else(`area_code` == "10YRO-TEL------P", eneRgymaps::eic.code.DELU(), `area_code`))
  testthat::expect_error (fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillDataATDEError, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",

                                               save.plot = FALSE,
                                               save.data = FALSE))

  # error : both AT/DE and AT defined
  fillDataATDEError <- fillData %>%
    dplyr::mutate(`area_code` = dplyr::if_else(`area_code` == "10YRO-TEL------P", eneRgymaps::eic.code.AT(), `area_code`))
  testthat::expect_error (fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillDataATDEError, fill.id.field.name = "area_code",
                                               fill.value.field.name = "price",
                                               fill.legend.name = "Electricity price",
                                               fill.color.palette = "Blues",

                                               save.plot = FALSE,
                                               save.data = FALSE))

  # error : no fill data, but fill id
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = NULL, fill.id.field.name = "area_code",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : ask to save data and map, but no output directory
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",

                                    save.plot = TRUE,
                                    save.data = TRUE))

  # error : forbidden character (" ") in value field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData %>% rename(`Electricity price` = `price`), fill.id.field.name = "area_code",
                                    fill.value.field.name = "Electricity price",
                                    fill.legend.name = "Electricity price",
                                    fill.color.palette = "Blues",
                                    fill.borders.color  = NULL,

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden character (" ") in id field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData %>% rename(`area code` = `area_code`), fill.id.field.name = "area code",
                                    fill.value.field.name = "price",
                                    fill.legend.name = "Electricity price",
                                    fill.color.palette = "Blues",
                                    fill.borders.color  = NULL,

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : fill value field does not exist
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.value.field.name = "AFieldWhichDoesNotExist",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : second label set, but not first label
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.label.first.field.name = NULL,
                                    fill.label.second.field.name = "percentage_change",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : both color palette and gradient color min/max set
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.value.field.name = "price",

                                    fill.color.palette = "Blues",
                                    fill.color.gradient.colors = c ("red", "green"),

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : both color palette and colors list set
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.value.field.name = "price",

                                    fill.color.palette = "Blues",
                                    fill.colors.list = c ("red", "purple"),

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : colors list set without fill.value.field.name
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.colors.list = c ("red", "purple"),

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : not the right number of colors
  testthat::expect_error ({fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData %>% mutate(`country` = as.factor(`country`)), fill.id.field.name = "area_code",
                                               fill.value.field.name = "country",
                                               fill.legend.name = "Electricity price",
                                               fill.colors.list = c ("red", "yellow", "blue"),
                                               fill.label.first.field.name = "price",

                                               save.plot = FALSE,
                                               save.data = FALSE); fillMap})

  # error : only one gradient color set (at least two required)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.value.field.name = "price",

                                    fill.color.gradient.colors = c ("yellow"),

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : color gradient values set, without a list of gradient colors
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",

                                                     fill.color.palette = "Blues",
                                                     fill.color.gradient.perc.values = c (0.1, 0.5),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : color gradient colors and values do not have the same length
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",

                                                     fill.color.gradient.colors = c ("green", "orange", "red"),
                                                     fill.color.gradient.perc.values = c (0.1, 0.5),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : fill value set, but no indication on color
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                    fill.value.field.name = "price",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : minimum color value set (without maximum)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.color.palette = "Blues",

                                                     fill.color.min.value = 15,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : maximum color value set (without maximum)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.color.palette = "Blues",

                                                     fill.color.max.value = 100,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : maximum color value below minimum color value
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.color.palette = "Blues",

                                                     fill.color.min.value = 150,
                                                     fill.color.max.value = 100,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : min/max color values set along with colors list
  # Map with variable used to fill bidding zones and a color list (fill.colors.list larger than number of distinct values)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "country",
                                                                fill.legend.name = "Electricity price",
                                                                fill.colors.list = c ("red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                                      "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                                      "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                                      "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                                      "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                                      "red", "yellow", "blue", "orange", "green", "purple", "black", "white",
                                                                                      "red", "yellow", "blue", "orange", "green", "purple", "black", "white"),
                                                                fill.label.first.field.name = "price",

                                                                fill.color.min.value = 15,
                                                                fill.color.max.value = 100,

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : fill.borders.width is not numeric
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                fill.borders.width  = "one",

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : fill.borders.width is negative
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.palette = "Blues",
                                                     fill.borders.width  = -1,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : too many distinct string values to plot with palette
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "country",
                                                     fill.color.palette = "Blues",

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : fill.legend.keys set without legend.values
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "BZN_vs_country",
                                                                fill.legend.name = "BZN vs. country", fill.legend.keys = c ("equal", "larger", "smaller"),
                                                                fill.colors.list = c ("red", "yellow", "blue"),

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : fill.legend.keys and fill.legend.values set without fill.value.field.name
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                     fill.legend.name = "BZN vs. country",
                                                     fill.legend.keys = c ("equal", "larger", "smaller"), fill.legend.values = c ("EQ", "LG", "SM"),
                                                     fill.colors.list = c ("red", "yellow", "blue"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : fill.legend.values set without legend.values
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "BZN_vs_country",
                                                     fill.legend.name = "BZN vs. country", fill.legend.values = c ("EQ", "LG", "SM"),
                                                     fill.colors.list = c ("red", "yellow", "blue"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : fill.legend.keys and legend.values set along with a color gradient
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "BZN_vs_country",
                                                     fill.legend.name = "BZN vs. country",
                                                     fill.legend.keys = c ("equal", "larger", "smaller"), fill.legend.values = c ("EQ", "LG", "SM"),
                                                     fill.color.gradient.colors = c ("red", "yellow", "blue"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : fill.legend.keys and legend.values set along with a color palette
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "BZN_vs_country",
                                                     fill.legend.name = "BZN vs. country",
                                                     fill.legend.keys = c ("equal", "larger", "smaller"), fill.legend.values = c ("EQ", "LG", "SM"),
                                                     fill.color.palette = "Blues",

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : some keys are missing in fill.legend.keys (compared to the dataset, "equal" is missing)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "BZN_vs_country",
                                                     fill.legend.name = "BZN vs. country",
                                                     fill.legend.keys = c ("larger", "smaller"), fill.legend.values = c ("LG", "SM"),
                                                     fill.colors.list = c ("red", "yellow", "blue"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : fill.legend.keys and fill.legend.values do not have the same length
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillBZData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "BZN_vs_country",
                                                     fill.legend.name = "BZN vs. country",
                                                     fill.legend.keys = c ("equal", "larger", "smaller"), fill.legend.values = c ("EQ", "LG", "SM", "XX"),
                                                     fill.colors.list = c ("red", "yellow", "blue"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : bad geographic entity name when requesting geographic entities' labels
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                geographic.entity.to.label = "Jurisdiction",

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : geographic.entity.to.label.political.initiatives set without geographic.entity.to.label
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.palette = "Blues",

                                                     geographic.entity.to.label.political.initiatives = c ("EU"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : bad political initiative name ("USA")
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.palette = "Blues",

                                                     geographic.entity.to.label = "Country",
                                                     geographic.entity.to.label.political.initiatives = c ("EU", "USA"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : geographic entity labels requested jointly with fill labels
  testthat::expect_error (fillMap <- eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.label.first.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                geographic.entity.to.label = "Bidding zone",
                                                                geographic.entity.label.font.color = "red",

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : bad legend position
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",
                                                                legend.position = "behind",

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : bad legend position (2)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                 fill.value.field.name = "price",
                                                                 fill.legend.name = "Electricity price",
                                                                 fill.color.palette = "Blues",
                                                                 legend.position = c (-1.5, 0.5),

                                                                 save.plot = FALSE,
                                                                 save.data = FALSE))

  # error : bad legend position (3)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                 fill.value.field.name = "price",
                                                                 fill.legend.name = "Electricity price",
                                                                 fill.color.palette = "Blues",
                                                                 legend.position = c (0.75, NA),

                                                                 save.plot = FALSE,
                                                                 save.data = FALSE))


  # error : missing longitude frame coordinates
  badFrameCoordinates1 <- list ("latitude.min" = 1000, "latitude.max" = 3000)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.gradient.colors = c ("green", "red"),
                                                                fill.color.min.value = 40,
                                                                fill.color.max.value = 100,

                                                                manual.frame.coordinates = badFrameCoordinates1,

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : frame coordinates are not numeric
  badFrameCoordinates2 <- c ("latitude.min" = "thousand", "latitude.max" = "two thousand", "longitude.min" = "thousand", "longitude.max" = "two thousand")
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),
                                                     fill.color.min.value = 40,
                                                     fill.color.max.value = 100,

                                                     manual.frame.coordinates = badFrameCoordinates2,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : bad min/max latitudes
  badFrameCoordinates3 <- c ("latitude.min" = 1000, "latitude.max" = 500, "longitude.min" = 1000, "longitude.max" = 2000)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),
                                                     fill.color.min.value = 40,
                                                     fill.color.max.value = 100,

                                                     manual.frame.coordinates = badFrameCoordinates3,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : bad min/max longitudes
  badFrameCoordinates4 <- c ("latitude.min" = 1000, "latitude.max" = 5000, "longitude.min" = 1000, "longitude.max" = 500)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),
                                                     fill.color.min.value = 40,
                                                     fill.color.max.value = 100,

                                                     manual.frame.coordinates = badFrameCoordinates4,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : duplicate data (due to 2 years of data)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillDataManyYears, fill.id.field.name = "area_code",
                                                                fill.value.field.name = "price",
                                                                fill.legend.name = "Electricity price",
                                                                fill.color.palette = "Blues",

                                                                save.plot = FALSE,
                                                                save.data = FALSE))

  # error : bad automated coordinates margin
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),

                                                     automated.frame.margins = list ("latitude.min" = 100, "latitude.max" = 1000,
                                                                                     "longitude.min" = "ten", "longitude.max" = 1250)))

  # error : non-numerical aspect ratio
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),

                                                     aspect.ratio.min = "zero"))

  # error: non-numerical aspect ratio (bis)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),

                                                     aspect.ratio.max = "infinite"))


  # error: min aspect ratio > max aspect ratio
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),

                                                     aspect.ratio.min = 0.95,
                                                     aspect.ratio.max = 0.9))

  # error: date before the first map
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),
                                                     date = as.Date("1988-05-06")))

  # error: bad date format (ISOdate)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),
                                                     date = ISOdate(year = 2015, month = 5, day = 12)))

  # error: bad date format (integer)
  testthat::expect_error (eneRgymaps::mapBiddingZone(fill.data = fillData, fill.id.field.name = "area_code",
                                                     fill.value.field.name = "price",
                                                     fill.legend.name = "Electricity price",
                                                     fill.color.gradient.colors = c ("green", "red"),
                                                     date = 2018))
})


testthat::test_that("Fill maps - country", {
  countryDataPath <- file.path (eneRgymaps::example.data.dir(), "Countries_assessment.csv")
  countryData <- read.csv (file = countryDataPath, na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE, fileEncoding = "UTF-8")

  # Fill countries with data (message because NI is displayed individually, whereas it is not a country)
  testthat::expect_message ({countryMap <- eneRgymaps::mapCountry(fill.data = countryData, fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                  fill.legend.name = "Value 2", fill.color.palette = "RdYlGn",
                  save.plot = FALSE, save.data = FALSE); countryMap; TRUE})

  # Fill countries (with UK data instead of GB data)
  testthat::expect_true ({countryMap <- eneRgymaps::mapCountry(fill.data = countryData %>% dplyr::mutate(`Country` = dplyr::if_else(`Country` == "GB", "UK", `Country`))%>% dplyr::filter (`Country` != "NI"),
                                              fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                              fill.legend.name = "Value 2", fill.color.palette = "RdYlGn",

                                              save.plot = FALSE, save.data = FALSE); countryMap; TRUE})

  # Fill countries (with IE (SEM) data instead of IE data)
  testthat::expect_message ({countryMap <- eneRgymaps::mapCountry(fill.data = countryData %>% dplyr::mutate(`Country` = dplyr::if_else(`Country` == "IE", "IE (SEM)", `Country`)) %>% dplyr::filter (`Country` != "NI"),
                                              fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                              fill.legend.name = "Value 2", fill.color.palette = "RdYlGn",

                                              save.plot = FALSE, save.data = FALSE); countryMap; TRUE})

  # error : no id field name defined, whereas a fill.value.field.name is defined
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = countryData, fill.value.field.name = "Value.2",
                                fill.legend.name = "Value 2", fill.color.palette = "RdYlGn",

                                save.plot = FALSE, save.data = FALSE))

  # error : asked to save plot, but no output directory defined
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = countryData, fill.id.field.name = "Country",
                                fill.value.field.name = "Value.2", fill.color.palette = "RdYlGn",

                                save.plot = TRUE, save.data = FALSE))

  # error : asked to save data, but no output directory defined
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = countryData, fill.id.field.name = "Country",
                                fill.value.field.name = "Value.2", fill.color.palette = "RdYlGn",

                                save.plot = FALSE, save.data = TRUE))

  # error : asked to save plot and data, but no output directory defined
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = countryData, fill.id.field.name = "Country",
                                fill.value.field.name = "Value.2", fill.color.palette = "RdYlGn",
                                save.plot = TRUE, save.data = TRUE))

  # error : both UK and GB defined
  dataUK <- countryData %>%
    dplyr::filter (`Country` == "GB") %>%
    dplyr::mutate (`Country` = "UK")
  dataGBUK <- countryData %>%
    rbind (dataUK)
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = dataGBUK, fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                              fill.legend.name = "Value 2", fill.color.palette = "RdYlGn",
                                              save.plot = FALSE, save.data = FALSE))

  # error : both UK and NI defined
  dataUK <- countryData %>%
    dplyr::filter (`Country` == "GB") %>%
    dplyr::mutate (`Country` = "UK")
  dataGBNI <- countryData %>%
    dplyr::mutate (`Country` = dplyr::if_else (`Country` == "GB", "NI", `Country`)) %>%
    rbind (dataUK)
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = dataGBNI, fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                fill.legend.name = "Value", fill.color.palette = "RdYlGn",
                                save.plot = FALSE, save.data = FALSE))

  # error : both UK and IE (SEM) defined
  dataUKIESEM <- countryData %>%
    dplyr::mutate (`Country` = dplyr::if_else (`Country` == "GB", "UK", `Country`),
                   `Country` = dplyr::if_else (`Country` == "IE", "IE (SEM)", `Country`))
  testthat::expect_error (eneRgymaps::mapCountry(fill.data = dataUKIESEM, fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                fill.legend.name = "Value", fill.color.palette = "RdYlGn",
                                save.plot = FALSE, save.data = FALSE))

  # error : both IE and IE (SEM) defined
  dataIEIESEM <- countryData %>%
    dplyr::mutate (`Country` = dplyr::if_else (`Country` == "GB", "IE (SEM)", `Country`))
  testthat::expect_error (suppressWarnings (eneRgymaps::mapCountry(fill.data = dataIEIESEM, fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                fill.legend.name = "Value", fill.color.palette = "RdYlGn",
                                save.plot = FALSE, save.data = FALSE)))

  # error : both NI and IE (SEM) defined
  dataNIIESEM <- countryData %>%
    dplyr::mutate (`Country` = dplyr::if_else (`Country` == "GB", "NI", `Country`),
                   `Country` = dplyr::if_else (`Country` == "IE", "IE (SEM)", `Country`))
  testthat::expect_error (suppressWarnings (eneRgymaps::mapCountry(fill.data = dataNIIESEM, fill.id.field.name = "Country", fill.value.field.name = "Value.2",
                                fill.legend.name = "Value", fill.color.palette = "RdYlGn",
                                save.plot = FALSE, save.data = FALSE)))
})


testthat::test_that("Fill maps - Control area", {
  controlAreaDataPath <- file.path (eneRgymaps::example.data.dir(), "LFC_assessment.csv")
  controlAreaData <- read.csv (file = controlAreaDataPath, na.strings = c("NA", "#N/A", "Miss"), fileEncoding = "UTF-8", stringsAsFactors = FALSE) %>%
    dplyr::filter(`year` == 2019)
  controlAreaDataDEPath <- file.path (eneRgymaps::example.data.dir(), "LFC_assessment_DE.csv")
  controlAreaDataDE <- read.csv (file = controlAreaDataDEPath, na.strings = c("NA", "#N/A", "Miss"), fileEncoding = "UTF-8", stringsAsFactors = FALSE)

  eneRgycache::initialise.cache()

  # Fill control areas with data
  testthat::expect_true ({controlAreaMap <- eneRgymaps::mapControlArea(fill.data = controlAreaData, fill.id.field.name = "LFC.EIC", fill.value.field.name = "Value.1",
                                              fill.legend.name = "Value", fill.color.palette = "RdYlGn",

                                              save.plot = FALSE, save.data = FALSE); controlAreaMap; TRUE})

  # Fill control areas with data (DE/LU as one entity)
  testthat::expect_true ({controlAreaMap <- eneRgymaps::mapControlArea(fill.data = controlAreaDataDE, fill.id.field.name = "LFC.EIC", fill.value.field.name = "Value.1",
                                                             fill.legend.name = "Value", fill.color.palette = "RdYlGn",

                                                             save.plot = FALSE, save.data = FALSE); controlAreaMap; TRUE})

  # error : no id field name defined, whereas a fill.value.field.name is defined
  testthat::expect_error (eneRgymaps::mapControlArea(fill.data = controlAreaData, fill.value.field.name = "Value.1",
                                fill.legend.name = "Value", fill.color.palette = "RdYlGn",
                                save.plot = FALSE, save.data = FALSE))

  # error : asked to save plot, but no output directory defined
  testthat::expect_error (eneRgymaps::mapControlArea(fill.data = controlAreaData, fill.id.field.name = "LFC.EIC",
                                                     fill.value.field.name = "Value.1", fill.color.palette = "RdYlGn",
                                                     save.plot = TRUE, save.data = FALSE))

  # error : asked to save data, but no output directory defined
  testthat::expect_error (eneRgymaps::mapControlArea(fill.data = controlAreaData, fill.id.field.name = "LFC.EIC",
                                                     fill.value.field.name = "Value.1", fill.color.palette = "RdYlGn",
                                                     save.plot = FALSE, save.data = TRUE))

  # error : asked to save plot and data, but no output directory defined
  testthat::expect_error (eneRgymaps::mapControlArea(fill.data = controlAreaData, fill.id.field.name = "LFC.EIC",
                                                     fill.value.field.name = "Value.1", fill.color.palette = "RdYlGn",
                                                     save.plot = TRUE, save.data = TRUE))

  # error : defined both DE/LU and a German TSO
  controlAreaDataBug <- controlAreaData %>%
    dplyr::mutate (`LFC.EIC` = dplyr::if_else (`LFC.EIC` == "10YDE-VE-------2", eneRgymaps::eic.code.DELU(), `LFC.EIC`))
  testthat::expect_error (controlAreaMap <- eneRgymaps::mapControlArea(fill.data = controlAreaDataBug, fill.id.field.name = "LFC.EIC", fill.value.field.name = "Value.1",
                                                             fill.color.palette = "RdYlGn",
                                                             save.plot = FALSE, save.data = FALSE))
})
