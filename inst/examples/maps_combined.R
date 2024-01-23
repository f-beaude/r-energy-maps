rm(list=ls())

library(dplyr)

# "combined" maps combine fill and border data

# initialise cache, for faster calculations
eneRgycache::initialise.cache()

## example data sets
# WARNING : the data sets included in this package are for illustration only, to illustrate what kinds of displays may be generated
exampleDataDir <- eneRgymaps::example.data.dir()
PriceData <- read.csv(file = file.path(exampleDataDir, "Prices.csv"), fileEncoding = "UTF-8",
                           header = TRUE, na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
  dplyr::filter (`year` == 2017) %>%
  dplyr::mutate(`shape` = dplyr::if_else (`discrete_value` == "medium", 16, dplyr::if_else (`discrete_value` == "high", 17, 18)))
FlowsData <- readxl::read_excel(path = file.path(exampleDataDir, "Flows.xlsx")) %>%
  dplyr::filter (`year` == 2017) %>%
  dplyr::rowwise() %>% dplyr::mutate (`interconnector_type` =  dplyr::if_else (runif(1, 0.0, 1.0) > 0.5, "AC", "DC"),
                        `interconnector_shape` = as.integer (dplyr::if_else (`interconnector_type` == "AC", 16, 17))) %>% dplyr::ungroup()

greenOrangeRed <- eneRgymaps::green.orange.red.palette (add.grey = FALSE)
redOrangeGreenGray <- eneRgymaps::red.orange.green.palette (add.grey = TRUE)
redOrangeGreen <- eneRgymaps::red.orange.green.palette (add.grey = FALSE)

lineCoordinateIdentifiers <- c (NA, NA, "long1", "lat1", "long2", "lat2")

## map combining bidding zone fill and border information, with customised legend position
combinedBZMap <- eneRgymaps::mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                  fill.color.palette = "Blues", fill.legend.name = "Electricity price",
                                  fill.label.first.field.name = "price", fill.label.rounding.ndigits = 1,

                                  border.data = FlowsData,
                                  border.id.field.name = "border",
                                  border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                  border.label.value.field.name = "Average",
                                  border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                  border.color.gradient.colors = c ("yellow", "orange", "red"),
                                  border.shape.width = 2,
                                  border.shape.name = "arrow",
                                  date = as.Date("2018-05-06"),

                                  legend.position = c (1, 0.67),

                                  save.plot = FALSE, save.data = FALSE)
combinedBZMap


## map combining bidding zone fill and border information (with border gradient customised based on values, and min/max color values defined)
combinedBZMapAbs <- eneRgymaps::mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                                fill.color.palette = "Blues", fill.legend.name = "day-ahead price",
                                                fill.label.first.field.name = "price",fill.label.rounding.ndigits = 1,

                                                border.data = FlowsData,
                                                border.id.field.name = "border",
                                                border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                border.label.value.field.name = "Average",
                                                border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                                border.width.value.field.name = "Average", border.legend.width.name = "Average flows",
                                                border.color.gradient.colors = c ("yellow", "orange", "red"),
                                                border.color.gradient.values = c (0, 250, 1000), # orange will be at 2500MW, and red at 1000MW
                                                border.color.min.value = 0, # without this option, any value below the min.gradient.value would be grey (instead of yellow)
                                                border.color.max.value = 1000, # without this option, any value above the max.gradient.value would be grey (instead of red)
                                                border.shape.name = "arrow",
                                                date = as.Date("2016-07-06"),

                                                save.plot = FALSE, save.data = FALSE)
combinedBZMapAbs


## map combining bidding zone fill and border information (and merged border legend)
outputDir <- file.path(base::tempdir(), "example_map")
dir.create(outputDir, showWarnings = FALSE, recursive = TRUE)
combinedBZMapMergedLegend <- eneRgymaps::mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                     fill.color.palette = "Blues", fill.legend.name = "day-ahead price",
                                     fill.label.first.field.name = "price", fill.label.rounding.ndigits = 1,

                                     border.data = FlowsData,
                                     border.id.field.name = "border",
                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                     border.label.value.field.name = "Average",
                                     border.color.value.field.name = "Average", border.legend.color.name = "Average flows",
                                     border.width.value.field.name = "Average", border.legend.width.name = "Average flows",
                                     border.color.palette = "Greens",
                                     border.shape.name = "arrow",
                                     date = as.Date("2016-07-06"),

                                     save.plot = TRUE, save.data = TRUE,
                                     output.directory = outputDir)
combinedBZMapMergedLegend

# help for setting shapes
eneRgymaps::help.shapes()

# help to set linetypes
eneRgymaps::help.path.linetypes()
