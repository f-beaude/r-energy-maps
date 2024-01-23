#' Dynamic geographic animations
rm(list=ls())

library(dplyr)

# initialise cache, for faster calculations
eneRgycache::initialise.cache()

## example data sets
# WARNING : the data sets included in this package are for illustration only, to illustrate what kinds of displays may be generated
exampleDataDir <- eneRgymaps::example.data.dir()
PriceData <- read.csv(file = file.path(exampleDataDir, "Prices.csv"),
                           fileEncoding = "UTF-8", stringsAsFactors = FALSE)
FlowsData <- readxl::read_xlsx(path = file.path(exampleDataDir, "Flows.xlsx"))

greenOrangeRed <- eneRgymaps::green.orange.red.palette (add.grey = FALSE)
redOrangeGreenGray <- eneRgymaps::red.orange.green.palette (add.grey = TRUE)
redOrangeGreen <- eneRgymaps::red.orange.green.palette (add.grey = FALSE)

# destination path for the animation : no file extension, no space
destinationPath <- file.path (tempdir(), "example_animation")

## animation with bidding-zone data
bzAnimation <- eneRgymaps::animate.mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                   fill.legend.name = "Electricity price", fill.color.gradient.colors = greenOrangeRed,
                                   fill.color.min.value = 20, fill.color.max.value = 70, # it is usually better to pre-define the min/max color value, to ensure consistency among plots
                                   animation.variable.name = "year", date.variable.name = "year",
                                   legend.position = "right")
bzAnimation

## animation with bidding-zone data, customising the animation time interval, and without animation caption
bzAnimationNoCaption <- eneRgymaps::animate.mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                   fill.legend.name = "Electricity price", fill.color.gradient.colors = greenOrangeRed,
                                   fill.color.min.value = 20, fill.color.max.value = 70,
                                   animation.variable.name = "year", date.variable.name = "year",
                                   animation.time.interval = 0.5, with.animation.caption = FALSE,
                                   legend.position = "right")
bzAnimationNoCaption

# combined fill and border animation
# when simultaneously animating two data sets (fill and border), animation.variable.name name needs to be identical for both data sets (here: "year")
combinedAnimation <- eneRgymaps::animate.mapBiddingZone(fill.data = PriceData, fill.id.field.name = "area_code", fill.value.field.name = "price",
                                   fill.legend.name = "Electricity price", fill.color.gradient.colors = greenOrangeRed,
                                   fill.color.min.value = 20, fill.color.max.value = 70,
                                   border.data = FlowsData,
                                   border.id.field.name = "border",
                                   border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                   border.color.value.field.name = "Average", border.color.palette = "Blues",
                                   border.color.min.value = -1000, border.color.max.value = 3000,
                                   border.shape.width = 3,
                                   border.shape.name = "arrow",
                                   border.flip.arrows.negative.values = TRUE,
                                   animation.variable.name = "year", date.variable.name = "year",
                                   animation.variable.order = rev (unique (PriceData[["year"]])),
                                   output.path = destinationPath,
                                   legend.position = "right")
combinedAnimation
