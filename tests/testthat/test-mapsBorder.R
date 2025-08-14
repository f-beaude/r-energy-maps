testthat::context("Test geographic maps - borders")

testthat::test_that("Border maps - bidding zones", {
  eneRgycache::initialise.cache()

  borderDataPath <- file.path(eneRgymaps::example.data.dir(), "Flows.xlsx")
  borderDataManyYears <- readxl::read_excel (path = borderDataPath)

  borderData <- borderDataManyYears %>%
    dplyr::filter (`year` == 2017) %>%
    dplyr::rowwise() %>% dplyr::mutate (`interconnector_type` =  dplyr::if_else (runif(1, 0.0, 1.0) > 0.5, "AC", "DC"),
                          `shapeId` = dplyr::if_else (`interconnector_type` == "AC", 16, 17)) %>% dplyr::ungroup()

  # Map with variable used to label bidding-zones borders
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                               border.id.field.name = "border",
                                               border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                               border.label.value.field.name = "Average",
                                               border.shape.color = "black",
                                               border.shape.name = "arrow",

                                               save.plot = FALSE,
                                               save.data = FALSE); borderMap; TRUE})

  # Map with variable used to label bidding zones borders (and labels written in red)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.shape.name = "arrow",
                                                 border.shape.color = "black",

                                                 border.label.font.color = "red",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})


  # Map with variable used to label bidding zones borders (and rounded number)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.shape.color = "black",
                                                                  border.shape.name = "arrow",

                                                                  border.label.rounding.ndigits = -1, # round to nearest tens

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to label bidding zones borders (and fixed arrow width)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.shape.color = "black",

                                                                  border.shape.name = "arrow",
                                                                  border.shape.width = 5, # set a predefined arrow width

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with uniform color
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.shape.color = "black",
                                                 border.shape.name = "arrow",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with label with frame (same as default)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.shape.color = "black",
                                                                  border.shape.name = "arrow",
                                                                  border.label.with.frame = TRUE,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with label without frame
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.shape.color = "black",
                                                                  border.shape.name = "arrow",
                                                                  border.label.with.frame = FALSE,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                 border.color.palette = "Blues",
                                                 border.shape.name = "arrow",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors and legend values as percentages
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                                  border.legend.color.values.as.percentages = TRUE,
                                                                  border.color.palette = "Blues",
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors (and no legend)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.color.value.field.name = "Average",
                                                 border.color.palette = "Blues",
                                                 border.shape.name = "arrow",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors (and minimum/maximum color value)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                        border.id.field.name = "border",
                                                        border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                        border.label.value.field.name = "Average",
                                                        border.color.value.field.name = "Average",
                                                        border.color.palette = "Blues",
                                                        border.shape.name = "arrow",

                                                        border.color.min.value = 50,
                                                        border.color.max.value = 300,

                                                        save.plot = FALSE,
                                                        save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors (and labelling countries)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.color.value.field.name = "Average",
                                                                  border.color.palette = "Blues",
                                                                  border.shape.name = "arrow",

                                                                  geographic.entity.to.label = "Country",


                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors (and labelling bidding zones in blue)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.color.value.field.name = "Average",
                                                                  border.color.palette = "Blues",

                                                                  geographic.entity.to.label = "Bidding zone",
                                                                  geographic.entity.label.font.color = "blue",
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with palette of colors (and labelling control areas in orange)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.color.value.field.name = "Average",
                                                                  border.color.palette = "Blues",

                                                                  geographic.entity.to.label = "Control area",
                                                                  geographic.entity.label.font.color = "orange",
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors and custom frame coordinates
  customFrameCoordinates <- list ("longitude.min" = -15, "longitude.max" = 25,
                                  "latitude.min" = 65, "latitude.max" = 80)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                 border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                 border.shape.name = "arrow",

                                                 manual.frame.coordinates = customFrameCoordinates,

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors and custom frame margins (on top of automated frame coordinates)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                                  border.shape.name = "arrow",

                                                                  automated.frame.margins = list ("latitude.min" = -125.1, "latitude.max" = 351.4,
                                                                                                        "longitude.min" = -0.5, "longitude.max" = 1250),

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors and custom aspect ratio
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                                  border.shape.name = "arrow",

                                                                  aspect.ratio.min = 0.9,
                                                                  aspect.ratio.max = 0.91,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors (and related percentiles)
  # warning expected because border.color.gradient.perc.values does not cover the full range of values (0-10% not covered)
  testthat::expect_warning ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                                  border.color.gradient.perc.values = c (0.1, 0.2, 1),
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors (and related values)
  # warning due to the fact that the border.color.gradient.values do not cover the full spectrum of Average values
  testthat::expect_warning ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                                  border.color.gradient.values = c (50, 500, 1000),
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors (and no legend)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.color.value.field.name = "Average",
                                                 border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                 border.shape.name = "arrow",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors (and min/max color values)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                        border.id.field.name = "border",
                                                        border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                        border.label.value.field.name = "Average",
                                                        border.color.value.field.name = "Average",
                                                        border.color.gradient.colors = c ("yellow", "orange", "red"),

                                                        border.color.min.value = 50,
                                                        border.color.max.value = 300,
                                                        border.shape.name = "arrow",

                                                        save.plot = FALSE,
                                                        save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors (and min/max color values and color gradient percentiles)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average",
                                                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                                  border.color.gradient.perc.values = c (0, 0.7, 1),

                                                                  border.color.min.value = 50,
                                                                  border.color.max.value = 300,
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with gradient of colors (and min/max color values and color gradient values)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "Average",
                                                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                                  border.color.gradient.values = c (-50, 500, 1000),

                                                                  border.color.min.value = 50,
                                                                  border.color.max.value = 300,
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with bidding zones borders depicted as triangles
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                                   border.width.value.field.name = "Average",
                                                                   border.shape.name = 17,
                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with bidding zones borders depicted as circles
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                                   border.width.value.field.name = "Average",
                                                                   border.shape.name = 16,
                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with bidding zones borders depicted as circles of fixed width
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                                   border.shape.width = 2,
                                                                   border.shape.name = 16,
                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with bidding zones borders depicted based on dynamic shapes
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.shape.value.field.name = "shapeId", border.legend.shape.name = "Interconnector type",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with bidding zones borders depicted based on dynamic shapes with customised legend
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.shape.value.field.name = "shapeId", border.legend.shape.name = "Interconnector type",
                                                                   border.legend.shape.keys = c (16, 17), border.legend.shape.values = c ("AC", "DC"),
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with bidding zones borders depicted based on dynamic shapes and labels
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.shape.value.field.name = "shapeId",
                                                                   border.label.value.field.name = "shapeId",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with colors list
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                  border.id.field.name = "border",
                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                  border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                  border.width.value.field.name = "Average",
                                                  border.shape.name = "line",
                                                  border.colors.list = c ("Green", "Yellow"),

                                                  save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with colors list and user-defined color legend names
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                                   border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                                   border.width.value.field.name = "Average",
                                                                   border.shape.name = "line",
                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with colors list (and two legends)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                  border.id.field.name = "border",
                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                  border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                  border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                  border.shape.name = "line",
                                                  border.colors.list = c ("Green", "Yellow"),

                                                  save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with variable used to color bidding zones borders, with colors list (and no legend)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                  border.id.field.name = "border",
                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                  border.color.value.field.name = "interconnector_type",
                                                  border.width.value.field.name = "Average",
                                                  border.shape.name = "line",
                                                  border.colors.list = c ("Green", "Yellow"),

                                                  save.plot = FALSE, save.data = FALSE); borderMap; TRUE})

  # Map with variable used to set the width of bidding zones borders
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average",

                                                 border.shape.color = "green",
                                                 border.shape.name = "arrow",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with variable used to set the width of bidding zones borders, with non-oriented arrows
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average",

                                                 border.shape.color = "green",
                                                 border.shape.name = "line",

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with negative values' arrows flipped flipped
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average",

                                                 border.shape.color = "green",
                                                 border.shape.name = "arrow",
                                                 border.flip.arrows.negative.values = TRUE,

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with both label, width and color fields
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average",
                                                 border.color.value.field.name = "Average",

                                                 border.color.palette = "Blues",
                                                 border.shape.name = "arrow",
                                                 border.flip.arrows.negative.values = TRUE,

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with both label, width and color fields (with merged legend)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                 border.color.value.field.name = "Average", border.legend.color.name = "Flows",

                                                 border.color.palette = "Blues",
                                                 border.shape.name = "arrow",
                                                 border.flip.arrows.negative.values = TRUE,

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with both label, width and color fields (with separate legends)
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                 border.id.field.name = "border",
                                                 border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                 border.label.value.field.name = "Average",
                                                 border.width.value.field.name = "Average", border.legend.width.name = "Flows - witdh",
                                                 border.color.value.field.name = "Average", border.legend.color.name = "Flows - color",

                                                 border.color.palette = "Blues",
                                                 border.shape.name = "arrow",
                                                 border.flip.arrows.negative.values = TRUE,

                                                 save.plot = FALSE,
                                                 save.data = FALSE); borderMap; TRUE})

  # Map with min/max border width value
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                                  border.shape.color = "blue",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = 350, border.width.max.value = 1000,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with max border width only
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                                  border.shape.color = "blue",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = NA, border.width.max.value = 1000,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # Map with min border width only
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                                  border.shape.color = "blue",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = 350, border.width.max.value = NA,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})


  # warning : border.legend.color.keys and border.legend.color.values defined without border.legend.color.name
  testthat::expect_warning ({borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "interconnector_type",
                                                                   border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                                   border.shape.width = 2,
                                                                   border.shape.name = "line",
                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE); borderMap; TRUE})


  # color legend: keep NAs
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "interconnector_type",
                                                                  border.colors.list = c ("green", "blue"),
                                                                  border.shape.name = "arrow",
                                                                  border.legend.color.name = "Average flows",
                                                                  border.legend.keep.na = TRUE,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # color legend: remove NAs
  testthat::expect_true ({borderMap <- eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.color.value.field.name = "interconnector_type",
                                                                  border.colors.list = c ("green", "blue"),
                                                                  border.shape.name = "arrow",
                                                                  border.legend.color.name = "Average flows",
                                                                  border.legend.keep.na = FALSE,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE); borderMap; TRUE})

  # error : neither label, nor color, nor width field name set
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",

                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : field name set, but not id
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.label.value.field.name = "Average",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : field name does not exist (label)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.label.value.field.name = "AFieldNotInData",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : id field defined, but not in/out ids
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : in/out id field names defined, but not id field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden id field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "out_area_code",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden label field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    border.label.value.field.name = "out_area_code",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : field name does not exist (width)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.width.value.field.name = "AFieldNotInData",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : field name does not exist (color)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.color.value.field.name = "AFieldNotInData",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : width legend set, but not the variable
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",
                                    border.width.legend.name = "Flows",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : color legend set, but not the variable
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    border.color.legend.name = "Flows",

                                    save.plot = FALSE,
                                    save.data = FALSE))


  # error : ask to save data and map, but no output directory
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.shape.color = "black",
                                    border.shape.name = "arrow",

                                    save.plot = TRUE,
                                    save.data = TRUE))

  # error : color set twice
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.color.value.field.name = "Average",
                                    border.shape.name = "arrow",

                                    border.color.gradient = c ("black", "green"),
                                    border.color.palette = "Blues",
                                    color.value.field.name = "Average",

                                    save.plot = FALSE,
                                    save.data = FALSE))



  # error : palette color set without color value field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.color.palette = "Blues",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : color gradient set without color value field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.color.gradient.colors = c ("yellow", "red"),
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : color gradient set with only one color
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "Average",
                                                     border.color.gradient.colors = c ("yellow"),
                                                     border.shape.name = "arrow",

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : color gradient perc values set without colors gradient
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "Average",
                                                     border.shape.name = "arrow",

                                                     border.color.palette = "Blues",
                                                     border.color.gradient.perc.values = c (0, 1),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : color gradient values set without colors gradient
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "Average",
                                                     border.shape.name = "arrow",

                                                     border.color.palette = "Blues",
                                                     border.color.gradient.values = c (0, 500, 1000),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : color gradient colors and values do not have the same length
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "Average",
                                                     border.shape.name = "arrow",

                                                     border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                     border.color.gradient.perc.values = c (0, 1),


                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : asked to flip non-oriented arrows
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                      border.id.field.name = "border",
                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                      border.label.value.field.name = "Average",
                      border.width.value.field.name = "Average",
                      border.shape.name = "line",

                      border.shape.color = "green",

                      border.flip.arrows.negative.values = TRUE,

                      save.plot = FALSE,
                      save.data = FALSE))

  # error : forbidden character (" " in in id field name)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData %>% rename (`in area code` = `in_area_code`),
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in area code", border.out.id.field.name = "out_area_code",
                                    border.label.value.field.name = "Average",
                                    border.width.value.field.name = "Average",

                                    border.shape.color = "green",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden character (" " in out id field name)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData %>% rename (`out area code` = `out_area_code`),
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out area code",
                                    border.label.value.field.name = "Average",
                                    border.width.value.field.name = "Average",

                                    border.shape.color = "green",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden character (" " in label field name)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData %>% rename (`avg 1` = `Average`),
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.label.value.field.name = "avg 1",

                                    border.shape.color = "green",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden character (" " in width field name)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData %>% rename (`avg 1` = `Average`),
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.width.value.field.name = "avg 1",

                                    border.shape.color = "green",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : forbidden character (" " in color field name)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData %>% rename (`avg 1` = `Average`),
                                    border.id.field.name = "border",
                                    border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                    border.color.value.field.name = "avg 1",

                                    border.color.palette = "Greens",
                                    border.shape.name = "arrow",

                                    save.plot = FALSE,
                                    save.data = FALSE))

  # error : min color value without max color value
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.color.value.field.name = "Average",
                                                                  border.shape.name = "arrow",

                                                                  border.color.palette = "Blues",
                                                                  border.color.min.value = 50,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : max color value without min color value
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.color.value.field.name = "Average",
                                                      border.shape.name = "arrow",

                                                      border.color.palette = "Blues",
                                                      border.color.max.value = 50,

                                                      save.plot = FALSE,
                                                      save.data = FALSE))

  # error : min color value higher than max color value
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.color.value.field.name = "Average",
                                                                  border.shape.name = "arrow",

                                                                  border.color.palette = "Blues",
                                                                  border.color.min.value = 50,
                                                                  border.color.max.value = 20,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : min/max color values set along with colors list
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.color.value.field.name = "year_1",
                                                                  border.shape.name = "arrow",

                                                                  border.colors.list = c ("green", "yellow", "orange"),
                                                                  border.color.min.value = 20,
                                                                  border.color.max.value = 50,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : colors list without color value field
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.shape.name = "arrow",

                                                     border.colors.list = c ("green", "yellow", "orange"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : not enough colors within the colors list
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "interconnector_type",
                                                     border.shape.name = "line",

                                                     border.colors.list = c ("green"),

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : both arrow width and arrow width field name set
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "Average",
                                                                  border.shape.width = 3,

                                                                  border.color.palette = "Blues",
                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : arrow width is not a number
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.shape.width = "five",

                                                     border.color.palette = "Blues",
                                                     border.shape.name = "arrow",
                                                     border.flip.arrows.negative.values = TRUE,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : arrow width is zero
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.shape.width = 0,

                                                     border.color.palette = "Blues",
                                                     border.shape.name = "arrow",
                                                     border.flip.arrows.negative.values = TRUE,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : arrow width is negative
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.shape.width = -1,

                                                     border.color.palette = "Blues",
                                                     border.shape.name = "arrow",
                                                     border.flip.arrows.negative.values = TRUE,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : border.legend.color.keys and border.legend.color.values defined without border.color.value.field.name
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                                   border.width.value.field.name = "Average",
                                                                   border.shape.name = "line",
                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : border.legend.color.keys defined without border.legend.color.values
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                                   border.legend.color.keys = c ("DC", "AC"),
                                                                   border.width.value.field.name = "Average",

                                                                   border.shape.name = "line",

                                                                   border.colors.list = c ("Green", "Yellow"),

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : border.legend.color.values defined without border.legend.color.keys
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                      border.legend.color.values = c ("direct current" ,"alternating current"),
                                                      border.width.value.field.name = "Average",
                                                      border.shape.name = "line",
                                                      border.colors.list = c ("Green", "Yellow"),

                                                      save.plot = FALSE, save.data = FALSE))

  # error : border.legend.color.keys and border.legend.color.values do not have the same length
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                      border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current", "other"),
                                                      border.width.value.field.name = "Average",
                                                      border.shape.name = "line",
                                                      border.colors.list = c ("Green", "Yellow"),

                                                      save.plot = FALSE, save.data = FALSE))

  # error : missing value in border.legend.color.keys ("AC")
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                      border.legend.color.keys = c ("DC", "FC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                      border.width.value.field.name = "Average",
                                                      border.shape.name = "line",
                                                      border.colors.list = c ("Green", "Yellow"),

                                                      save.plot = FALSE, save.data = FALSE))

  # error : border.legend.color.keys and border.legend.color.values defined along with border.color.gradient.colors
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                      border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                      border.width.value.field.name = "Average",
                                                      border.shape.name = "line",
                                                      border.color.gradient.colors = c ("Green", "Yellow"),

                                                      save.plot = FALSE, save.data = FALSE))

  # error : border.legend.color.keys and border.legend.color.values defined along with border.color.palette
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                      border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                      border.width.value.field.name = "Average",
                                                      border.shape.name = "line",
                                                      border.color.palette = "Blue",

                                                      save.plot = FALSE, save.data = FALSE))

  # error : the shape value field name does not exist
  testthat::expect_error (borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "shapeId",
                                                                   border.shape.value.field.name = "ANewFieldName",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE))


  # error : both shape value and fixed shape name
  testthat::expect_error (borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.shape.value.field.name = "shapeId",
                                                                   border.shape.name = "line",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : bad shape values
  borderDataShapesBad <- borderData %>%
    dplyr::mutate (`badShapeId` = dplyr::if_else (`shapeId` >= 17, "oneShape", "anotherShape"))
  testthat::expect_error (borderMap <- eneRgymaps::mapBiddingZone (border.data = borderDataShapesBad,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.shape.value.field.name = "badShapeId",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : bad fixed shape name
  testthat::expect_error (borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "shapeId",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",
                                                                   border.shape.name = "myOwnShape",

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : asking to flip non-arrows
  testthat::expect_error (borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "shapeId",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",
                                                                   border.shape.name = 16,
                                                                   flip.arrows.negative.values = TRUE,

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : neither shape field nor shape name
  testthat::expect_error (borderMap <- eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.color.value.field.name = "shapeId",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : both border.color.gradient.perc.values and border.color.gradient.values defined
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.label.value.field.name = "Average",
                                                     border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                     border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                     border.color.gradient.values = c (0, 500, 1000),
                                                     border.color.gradient.perc.values = c (0, 0.75, 1),
                                                     border.shape.name = "arrow",

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : border.legend.shape.name defined without border.shape.value.field.name
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                                   border.id.field.name = "border",
                                                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                   border.shape.name = "line", border.legend.shape.name = "Interconnector type",
                                                                   border.shape.width = 2,
                                                                   border.shape.color = "Green",

                                                                   save.plot = FALSE, save.data = FALSE))

  # error : legend shape keys and values defined without border.shape.value.field.name
  testthat::expect_error (eneRgymaps::mapBiddingZone (border.data = borderData,
                                                      border.id.field.name = "border",
                                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                      border.shape.name = "line",
                                                      border.legend.shape.keys = c (16, 17), border.legend.shape.values = c ("AC", "DC"),
                                                      border.shape.width = 2,
                                                      border.shape.color = "Green",

                                                      save.plot = FALSE, save.data = FALSE))

  # error : duplicate value for the same id (due to many years of data)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderDataManyYears,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.label.value.field.name = "Average",
                                                                  border.shape.color = "black",
                                                                  border.shape.name = "arrow",

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : border.width.min.value set alone
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                                  border.shape.color = "blue",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = 350,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : border.width.max.value set alone
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.width.value.field.name = "Average", border.legend.width.name = "Flows",
                                                     border.shape.color = "blue",

                                                     border.shape.name = "arrow",
                                                     border.flip.arrows.negative.values = TRUE,

                                                     border.width.max.value = 1000,

                                                     save.plot = FALSE,
                                                     save.data = FALSE))

  # error : min/max border width value set without width value field name
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",

                                                                  border.shape.width = 1,

                                                                  border.color.value.field.name = "Average",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = 350, border.width.max.value = 1000,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : forbidden field name
  borderDataForbidden <- borderData %>%
    mutate (`avg_1_for_width_plot` = `Average`)
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderDataForbidden,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "Average",
                                                                  border.color.value.field.name = "avg_1_for_width_plot",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = 350, border.width.max.value = 1000,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))

  # error : border.width.min.value > border.width.max.value
  testthat::expect_error (eneRgymaps::mapBiddingZone(border.data = borderData,
                                                                  border.id.field.name = "border",
                                                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                                  border.width.value.field.name = "border",
                                                                  border.shape.color = "blue",

                                                                  border.shape.name = "arrow",
                                                                  border.flip.arrows.negative.values = TRUE,

                                                                  border.width.min.value = 350, border.width.max.value = 300,

                                                                  save.plot = FALSE,
                                                                  save.data = FALSE))
  })
