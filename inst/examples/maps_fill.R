rm(list=ls())

library(dplyr)

# "fill" maps depict information related to geographic entities

# initialise cache, for faster calculations
eneRgycache::initialise.cache()

## example data sets
# WARNING : the data sets included in this package are for illustration only, to illustrate what kinds of displays may be generated
exampleDataDir <- eneRgymaps::example.data.dir()
BZs <- read.csv(file = file.path(exampleDataDir, "Bidding_zones.csv"), header = TRUE,
                fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE)
PriceData <- read.csv(file = file.path(exampleDataDir, "Prices.csv"), header = TRUE,
                           fileEncoding = "UTF-8", na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE) %>%
  dplyr::filter (`year` == 2019) %>%
  dplyr::mutate(`shape` = dplyr::if_else(`discrete_value` == "medium", 16, dplyr::if_else (`discrete_value` == "high", 17, 18)))

CountriesAssessment <- read.csv(file = file.path(exampleDataDir, "Countries_assessment.csv"), fileEncoding = "UTF-8",
                                na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE)
CountriesAssessmentUK <- read.csv(file = file.path(exampleDataDir, "Countries_assessment_UK.csv"), fileEncoding = "UTF-8",
                                  na.strings = c("NA", "#N/A", "Miss"), stringsAsFactors = FALSE)
LFCAreasAssessment <- read.csv(file = file.path(exampleDataDir, "LFC_assessment.csv"), fileEncoding = "UTF-8",
                                   na.strings = c("NA", "#N/A", "Miss")) %>%
  dplyr::filter (`year` == 2019)
TradeData <- readxl::read_excel(path = file.path(exampleDataDir, "Trade.xlsx")) %>%
  dplyr::rename (`trade_factor` = `Value`) %>%
  dplyr::filter (`Year` == 2022)

# colour palettes (to define which colours to use in the plots)
redOrangeGreenGray <- eneRgymaps::red.orange.green.palette (add.grey = TRUE)
redOrangeGreen <- eneRgymaps::red.orange.green.palette (add.grey = FALSE)
myColorPalette <- c(eneRgymaps::palette.colors.sample(length = 3), "grey")

## map with bidding-zone data mapped with country (with user-defined colors) and date for shapefile (if date is NULL, it is set to today)
myBZCountryMap <- eneRgymaps::mapBiddingZone(fill.data = BZs, fill.id.field.name = "area_code", fill.value.field.name = "BZN_vs_country",
                                             fill.colors.list = c ("blue", "orange", "forestgreen"),
                                             fill.legend.name = "Bidding zone vs. country",
                                             date = as.Date("2019-01-01"),
                                             save.plot = FALSE, save.data = FALSE)
myBZCountryMap


# zoom towards AT and DE/LU, with constraints on the aspect ratio of the plot
myBZCountryMapDEAT <- eneRgymaps::mapBiddingZone(fill.data = BZs %>% filter (`Country` %in% c("AT", "DE-LU")),
                                                 fill.id.field.name = "area_code", fill.value.field.name = "BZN_vs_country",
                                                 fill.colors.list = c ("blue", "orange", "forestgreen"),
                                                 fill.legend.name = "Bidding zone vs. country",

                                                 aspect.ratio.min = 1,
                                                 aspect.ratio.max = 1,
                                                 date = as.Date("2019-01-01"),
                                                 save.plot = FALSE, save.data = FALSE)
myBZCountryMapDEAT


## map with bidding-zone data mapped with country (relying on a palette of colors, customising the legend string, and adding country labels)
myBZCountryMapPalette <- eneRgymaps::mapBiddingZone(fill.data = BZs, fill.id.field.name = "area_code", fill.value.field.name = "BZN_vs_country",
                                             fill.legend.name = "Bidding zone vs. country",
                                             fill.legend.keys = c ("equal", "smaller", "larger"), fill.legend.values = c ("matches country", "smaller than country", "larger than country"),
                                             fill.colors.list = myColorPalette[1:3],
                                             fill.na.color = myColorPalette[4],
                                             date = as.Date("2019-01-01"),
                                             geographic.entity.to.label = "Country",
                                             save.plot = FALSE, save.data = FALSE)
myBZCountryMapPalette


## map with bidding-zone data filled and labels (and legend displayed on the right side of the plot).
# Neighbouring countries (in Africa, Eastern Europe and Middle East) are hidden (instead of being depicting as NA)
myBZFillMap <- eneRgymaps::mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                   fill.legend.name = "Electricity price",
                                   fill.color.gradient.colors = eneRgymaps::palette.colors.sample(length = 6),
                                   fill.label.first.field.name = "price", fill.label.second.field.name = "percentage_change",
                                   title.text = "Electricity price changes in the EU", title.size = 25, title.color = "black", title.face = "bold",
                                   background.iso2s.to.ignore = c ("BY", "DZ", "EG", "GL", "IL", "IS", "LB", "LY", "MA", "MD", "RU", "SY", "TN", "TR", "UA"),
                                   legend.position = "right", legend.text.size = 15,
                                   date = as.Date("2019-01-01"),
                                   save.plot = FALSE, save.data = FALSE)
myBZFillMap


# BZ Map with merged AT/DE bidding-zone and no legend, and with fill label with frame
fillDataATDE <- PriceData %>%
  dplyr::mutate(`area_code` = dplyr::if_else(`area_code` == eneRgymaps::eic.code.DELU(), eneRgymaps::eic.code.merged.AT.DE(), `area_code`)) %>%
  dplyr::filter(`area_code` != eneRgymaps::eic.code.AT())
myBZATDEFillMap <- eneRgymaps::mapBiddingZone(fill.data = fillDataATDE, fill.id.field.name = "area_code",
                                              fill.value.field.name = "price",
                                              fill.label.first.field.name = "price", fill.color.palette = "Blues",
                                              fill.label.second.field.name = "percentage_change",
                                              fill.label.rounding.ndigits = 2, fill.label.font.size = 2.75,
                                              fill.label.with.frame = TRUE,
                                              date = as.Date("2017-01-01"),
                                              save.plot = FALSE, save.data = FALSE)
myBZATDEFillMap


# BZ map with center shapes (and related legend)
myBZCenterShapesMap <- eneRgymaps::mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code",
                                                  date = as.Date("2019-01-01"),
                                                  fill.center.shape.value.field.name = "shape",
                                                  fill.center.shape.color.field.name = "shape",
                                                  fill.center.shape.width = 2,
                                                  fill.center.shape.colors.list = redOrangeGreen,
                                                  fill.legend.center.shape.name = "Trade level",
                                                  fill.legend.center.shape.keys = c (16, 17, 18),
                                                  fill.legend.center.shape.values = c ("medium", "high", "very high"),
                                                  # need to define the exact same attributes for the general shape and shape color legends, to ensure that ggplot merges them into one
                                                  fill.legend.center.shape.color.name = "Trade level",
                                                  fill.legend.center.shape.keep.na = TRUE,
                                                  fill.legend.center.shape.color.keys = c (16, 17, 18),
                                                  fill.legend.center.shape.color.values = c ("medium", "high", "very high"),
                                                  save.plot = FALSE, save.data = FALSE)
myBZCenterShapesMap


#help for setting shapes
eneRgymaps::help.shapes()

## map with countries filled (and GB different from NI), and customised fill legend values
countryFillMap <- eneRgymaps::mapCountry(fill.data = CountriesAssessment, fill.id.field.name = "Country", fill.value.field.name = "Value.1",
                                         fill.label.first.field.name = "Value.2",
                                         fill.legend.name = "Values", fill.colors.list = redOrangeGreenGray,
                                         fill.legend.keys = c (1, 2, 3, 4), fill.legend.values = c ("poor", "to be monitored", "adequate", "other"),
                                         save.plot = FALSE, save.data = FALSE)
countryFillMap


## map with countries filled (and UK), legend values displayed as percentages, and borders filled in blue (dashed and wider)
countryFillUKMap <- eneRgymaps::mapCountry(fill.data = CountriesAssessmentUK, fill.id.field.name = "Country", fill.value.field.name = "Value.3",
                                           fill.legend.name = "Values", fill.color.palette = "Greens",
                                           fill.legend.values.as.percentages = TRUE,
                                           fill.borders.color = "Blue", fill.borders.width = 1.1, fill.borders.linetype = "dashed",
                                           save.plot = FALSE, save.data = FALSE)
countryFillUKMap


# help to set linetypes
eneRgymaps::help.path.linetypes()


## map with countries filled and hidden borders
countryFillMapNoBorder <- eneRgymaps::mapCountry(fill.data = CountriesAssessment, fill.id.field.name = "Country", fill.value.field.name = "Value.1",
                                                 fill.legend.name = "Values 1", fill.colors.list = redOrangeGreenGray,
                                                 fill.borders.color = NULL, # setting either fill.borders.color to NULL or fill.borders.width to 0 hides borders
                                                 fill.borders.width = 0,
                                                 save.plot = FALSE, save.data = FALSE)
countryFillMapNoBorder


## map related to trade (with continuous color gradient, and min/max color values)
tradeMapGradient <- eneRgymaps::mapCountry(fill.data = TradeData, fill.id.field.name = "Country", fill.value.field.name = "trade_factor",
                                               fill.legend.name = "Trade factor",
                                               fill.color.gradient.colors = c ("red", "yellow", "forestgreen"),
                                               fill.color.min.value = 0.5, fill.color.max.value = 9,
                                               save.plot = FALSE, save.data = FALSE)
tradeMapGradient


## map related to trade (with continuous color gradient, min/max color values, and user-defined percentiles associated with each gradient color)
tradeMapGradientPerc <- eneRgymaps::mapCountry(fill.data = TradeData, fill.id.field.name = "Country", fill.value.field.name = "trade_factor",
                                                   fill.legend.name = "Trade factor",
                                                   fill.color.gradient.colors = c ("red", "orange", "yellow", "forestgreen"),
                                                   fill.color.gradient.perc.values = c (0, 0.7, 0.9, 1),
                                                   fill.color.min.value = 0.5, fill.color.max.value = 9,
                                                   save.plot = FALSE, save.data = FALSE)
tradeMapGradientPerc


## map related to trade (with color palette and bidding zone labels for EU and EFTA countries only)
tradeMapPalette <- eneRgymaps::mapCountry(fill.data = TradeData, fill.id.field.name = "Country", fill.value.field.name = "trade_factor",
                                              fill.legend.name = "Trade factor",
                                              fill.color.palette = "RdYlGn",
                                              geographic.entity.to.label = "Bidding zone", geographic.entity.to.label.political.initiatives = c ("EU", "EFTA"),
                                              fill.color.min.value = 0.5, fill.color.max.value = 9,
                                              save.plot = FALSE, save.data = FALSE)
tradeMapPalette


## map with LFC areas filled
LFCAreaFillMap <- eneRgymaps::mapControlArea(fill.data = LFCAreasAssessment, fill.id.field.name = "LFC.EIC",
                                                 fill.value.field.name = "Value.1",
                                                 fill.colors.list = myColorPalette[1:3],
                                                 save.plot = FALSE, save.data = FALSE)
LFCAreaFillMap


# help for setting shapes
eneRgymaps::help.shapes()

# help to set linetypes
eneRgymaps::help.path.linetypes()
