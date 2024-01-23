rm(list=ls())

library(dplyr)

# A geographic map is made of layers.
# These layers can relate to either
# - geographic entities (bidding-zones, countries or LFC areas)
# - borders between geographic entities

# There are two ways to generate a geographic map
# - rely on a default function: eneRgymaps::mapBiddingZone, eneRgymaps::mapCountry, or eneRgymaps::mapControlArea
# - add map components

# The arguments of the function refer to
# - the data set to display (as data.frame)
# - the name of data items to use as identifier of geographic entities
# - the name of data items to plot for each element
# - display options
# - options to save the underlying data
# Display help on all parameters by running ?eneRgymaps::mapBiddingZone (or ??eneRgymaps::mapXXX where XXX is the name of the geographic entity)
?eneRgymaps::mapBiddingZone

# initialise cache, for faster calculations
eneRgycache::initialise.cache()

## example data sets
# WARNING : the data sets included in this package are for illustration only, to illustrate what kinds of displays may be generated
exampleDataDir <- eneRgymaps::example.data.dir()
PriceData <- read.csv(file = file.path(exampleDataDir, "Prices.csv"), header = TRUE,
                           fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
  dplyr::filter (`year` == 2017) %>%
  dplyr::mutate(`shape` = dplyr::if_else (`discrete_value` == "medium", 16, dplyr::if_else (`discrete_value` == "high", 17, 18)))
FlowsData <- readxl::read_excel(path = file.path(exampleDataDir, "Flows.xlsx")) %>%
  dplyr::filter (`year` == 2017) %>%
  dplyr::rowwise() %>% dplyr::mutate (`interconnector_type` =  dplyr::if_else (runif(1, 0.0, 1.0) > 0.5, "AC", "DC"),
                                      `interconnector_shape` = as.integer (dplyr::if_else (`interconnector_type` == "AC", 16, 17))) %>% dplyr::ungroup()

LFCAreasAssessment <- read.csv(file = file.path(exampleDataDir, "LFC_assessment.csv"), fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss")) %>%
  dplyr::filter(`year` == 2019)
TradeData <- readxl::read_excel(path = file.path(exampleDataDir, "Trade.xlsx")) %>%
  dplyr::filter(`Year` == 2021) %>%
  dplyr::rename (`trade_factor` = `Value`)

# color palettes can be used to customise color display
# Here, a random sample of 3 colors
myColorPalette <- eneRgymaps::palette.colors.sample(length = 3)

## map with LFC areas filled
LFCAreaFillMap <- eneRgymaps::mapControlArea(fill.data = LFCAreasAssessment, fill.id.field.name = "LFC.EIC",
                                                 fill.value.field.name = "Value.1",
                                                 fill.colors.list = myColorPalette,
                                                 save.plot = FALSE, save.data = FALSE)
LFCAreaFillMap


## map related to trade (with color palette and bidding-zone labels for EU and EFTA countries only)
tradeMapPalette <- eneRgymaps::mapCountry(fill.data = TradeData, fill.id.field.name = "Country", fill.value.field.name = "trade_factor",
                                              fill.legend.name = "Trade factor",
                                              fill.color.palette = "RdYlGn",
                                              fill.color.min.value = 0.5, fill.color.max.value = 4,
                                              background.iso2s.to.ignore = c ("BY", "DZ", "EG", "GL", "IL", "IS", "LB", "LY", "MA", "MD", "RU", "SY", "TN", "TR", "UA"),

                                              save.plot = FALSE, save.data = FALSE)
tradeMapPalette


## map relying on dynamic shapes to describe border types (and merged shape/color legend)
mapBorderTypeDynShape <- eneRgymaps::mapBiddingZone (border.data = FlowsData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                     border.shape.value.field.name = "interconnector_shape", border.legend.shape.name = "Interconnector type",
                                                     border.legend.shape.keys = c (16, 17), border.legend.shape.values = c ("AC", "DC"),
                                                     border.shape.solid = TRUE, # fully fill the shapes (not only the shape border, but also the inside)
                                                     border.colors.list = c ("green", "yellow"),
                                                     border.shape.width = 5,

                                                     save.plot = FALSE, save.data = FALSE)
mapBorderTypeDynShape


## map combining bidding zone fill and border information, with customised legend position
combinedBZMap <- eneRgymaps::mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                            fill.color.palette = "Blues", fill.legend.name = "Electricity price",
                                            fill.label.first.field.name = "price", fill.label.rounding.ndigits = 0,

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


# To create a customised plot
# 1. generate data sets including geographic coordinates; and
# 2. create the plot layers one by one (or add a layer to an existing plot)

# 1. generate data sets including geographic coordinates
# retrieve the geographic shapes of bidding-zones, as they were on 21/04/2018
# (add the geographic coordinates of centroids of shapes, to plot labels for each geographic entity)
bzShapes <- eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", date = as.Date("2017-04-21"), with.centroids = TRUE)

# merge the data to plot with the geographic shapes
pricesWithCoordinates <- base::merge (x = PriceData, by.x = "area_code",
                                       y = bzShapes, by.y = "eic_code",
                                       all = FALSE)

# Lines data already include geographic coordinates
LinesData <- readxl::read_excel(path = file.path(exampleDataDir, "Lines.xlsx")) %>%
  dplyr::mutate(`loading` = dplyr::if_else(`flow` >= `Fmax`, "overload",
                                    dplyr::if_else(`flow` >= 0.9 * `Fmax`, "high load", "normal")))

# 2. create the plot layers one by one (or add one layer to an existing map)
customMap <- eneRgymaps::world.background(only.keep.Europe = TRUE, fill.color = "grey", border.color = "white") %>%
  # add caption
  eneRgymaps::add.title.caption(caption.text = "Generated with eneRgymaps") %>%
  # add (and configure) filling
  eneRgymaps::add.fill.and.border(data = pricesWithCoordinates, fill.color.field = "price", border.color = "white") %>%
  eneRgymaps::configure.colors(data = pricesWithCoordinates, configure.type = "fill", field.name = "price", color.palette = "Greens") %>%
  # add arrows
  eneRgymaps::add.arrows(data = LinesData, coordinates.identifiers = c (NA, NA, "long1", "lat1", "long2", "lat2"),
                         color.field.name = "loading", colors.list = c("orange", "green", "red"), width.field.name = "flow") %>%
  # add labels
  eneRgymaps::add.labels(data = pricesWithCoordinates, coordinates.identifiers = c ("centroid_longitude", "centroid_latitude"),
                         field.names = c("price"), rounding.ndigits = 0,
                         with.frame = TRUE, font.color = "black") %>%
  # set the coordinates of the plot frame
  eneRgymaps::set.frame.coordinates(longitude.min = -15, longitude.max = 25,
                                    latitude.min = 35, latitude.max = 65)

customMap
