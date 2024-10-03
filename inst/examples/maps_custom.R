rm(list=ls())

library(dplyr)

# Custom maps can either
# - build plot layers from scratch
# - add plot layers on top of an already-existing map


# initialise cache, for faster calculations
eneRgycache::initialise.cache()

## example data sets
# WARNING : the data sets included in this package are for illustration only, to illustrate what kinds of displays may be generated
exampleDataDir <- eneRgymaps::example.data.dir()
BZs <- read.csv(file = file.path(exampleDataDir, "Bidding_zones.csv"), header = TRUE, fileEncoding = "UTF-8",
                na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE)
PriceData <- read.csv(file = file.path(exampleDataDir, "Prices.csv"), header = TRUE,
                           fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
  dplyr::filter (`year` == 2019) %>%
  dplyr::mutate(`shape` = dplyr::if_else (`discrete_value` == "medium", 16, dplyr::if_else (`discrete_value` == "high", 17, 18)))
LinesData <- readxl::read_excel(path = file.path(exampleDataDir, "Lines.xlsx")) %>%
  dplyr::mutate(`loading` = dplyr::if_else(`flow` >= `Fmax`, "overload",
                            dplyr::if_else(`flow` >= 0.9 * `Fmax`, "high load", "normal")))

# Geographic shapes of bidding-zones (as of today)
BZsShapes <- eneRgymaps::geographic.shapes(geographic.entity.name = "Bidding zone", with.centroids = TRUE)

# Random sample of 5 colours
myColorPalette <- eneRgymaps::palette.colors.sample(length = 5)

# Name of the fields from LinesData which geographic coordinates
lineCoordinateIdentifiers <- c (NA, NA, "long1", "lat1", "long2", "lat2")


# 1. Generate data sets combining the data to plot with geographic shapes
pricesWithCoordinates <- base::merge (x = PriceData, by.x = "area_code",
                                y = BZsShapes, by.y = "eic_code", all = FALSE)

highPricesWithCoordinates <- pricesWithCoordinates %>%
  dplyr::filter(`price` > 63)


# 2. Generate maps (from scratch, or on top of an existing map)

# 2.a Building a map from scratch, by adding layers
# start with world background
customMap1 <- eneRgymaps::world.background(only.keep.Europe = TRUE, fill.color = "grey", border.color = "white") %>%
  # add filling and standard borders between shapes
  eneRgymaps::add.fill.and.border(data = pricesWithCoordinates, fill.color.field = "price",
                                  border.color = "white", border.width = 2, border.linetype = 1) %>%
  eneRgymaps::configure.colors(data = pricesWithCoordinates, configure.type = "fill", field.name = "price", gradient.colors = myColorPalette[1:3], legend.name = "Price")

customMap1


# start with world background
customMap2 <- eneRgymaps::world.background(only.keep.Europe = TRUE, fill.color = "grey", border.color = "white") %>%
  # add title and caption
  eneRgymaps::add.title.caption(title.text = "My custom map", caption.text = "Generated with eneRgymaps") %>%
  # add (and configure) filling
  eneRgymaps::add.fill.and.border(data = pricesWithCoordinates, fill.color.field = "price", border.color = "white") %>%
  eneRgymaps::configure.colors(data = pricesWithCoordinates, configure.type = "fill", field.name = "price", color.palette = "Greens") %>%
  # add arrows
  eneRgymaps::add.arrows(data = LinesData, coordinates.identifiers = lineCoordinateIdentifiers,
                         color.field.name = "loading", colors.list = c("orange", "green", "red"), width.field.name = "flow") %>%
  # add labels
  eneRgymaps::add.labels(data = pricesWithCoordinates, coordinates.identifiers = c ("centroid_longitude", "centroid_latitude"),
                         field.names = c("price"), rounding.ndigits = 0,
                         with.frame = TRUE, font.color = "black") %>%
  # set frame coordinates
  eneRgymaps::set.frame.coordinates(longitude.min = -15, longitude.max = 25,
                                    latitude.min = 35, latitude.max = 65)

customMap2


# 2.b Adding a layer to an existing map

# Add a custom layer to a map
# Add lines to a geographic map
customMap3 <- eneRgymaps::mapBiddingZone(fill.data = BZs, fill.id.field.name = "area_code", fill.value.field.name = "BZN_vs_country",
                                                                fill.legend.name = "Bidding zone vs. country",
                                                                fill.legend.keys = c ("equal", "smaller", "larger"), fill.legend.values = c ("matches country", "smaller than country", "larger than country"),
                                                                fill.colors.list = myColorPalette[1:3],
                                                                fill.na.color = myColorPalette[4],
                                                                date = as.Date("2019-01-01"),
                                                                geographic.entity.to.label = "Country",
                                                                save.plot = FALSE, save.data = FALSE) %>%
  eneRgymaps::add.lines(data = LinesData, coordinates.identifiers = lineCoordinateIdentifiers,
                        color = myColorPalette[5], width = 3)
customMap3

# 2.c Combining an energy map with a ggplot layer
# Add stripes to the background map (using ggpattern)
# ?ggpattern::geom_sf_pattern for help
customMap4 <- eneRgymaps::world.background(only.keep.Europe = TRUE, fill.color = "grey", border.color = "white") +
  ggpattern::geom_sf_pattern(data = highPricesWithCoordinates,
                             mapping = ggplot2::aes_string(geometry = "geometry"), # rely on the geometry from the dataset
                             pattern = 'stripe', # the kind of pattern to use
                             pattern_fill = 'blue', # main filling
                             pattern_size = 0, # how large black lines must be between blue fillings
                             pattern_density = 0.5, # how much of the space do blue fillings take
                             pattern_spacing = 0.05, # how many lines are drawn (smaller value = more thinner lines)
                             fill = 'red', # how to fill the rest of the shape (NA means no filling)
                             color = NA) # color of the border element
customMap4

# help for setting shapes
eneRgymaps::help.shapes()

# help to set linetypes
eneRgymaps::help.path.linetypes()
