rm(list=ls())

library(dplyr)

# "border" maps depict information between geographic entities

# initialise cache, for faster calculations
eneRgycache::initialise.cache()

## example data sets
# WARNING : the data sets included in this package are for illustration only, to illustrate what kinds of displays may be generated
exampleDataDir <- eneRgymaps::example.data.dir()

FlowsData <- readxl::read_excel(path = file.path(exampleDataDir, "Flows.xlsx")) %>%
  dplyr::filter (`year` == 2017) %>%
  dplyr::rowwise() %>% dplyr::mutate (`interconnector_type` =  dplyr::if_else (runif(1, 0.0, 1.0) > 0.5, "AC", "DC"),
                        `interconnector_shape` = as.integer (dplyr::if_else (`interconnector_type` == "AC", 16, 17))) %>% dplyr::ungroup()

## map with arrows on border data (and border labels written in red)
mapFlows <- eneRgymaps::mapBiddingZone (border.data = FlowsData,
                               border.id.field.name = "border",
                               border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                               border.label.value.field.name = "Average",
                               border.width.value.field.name = "Average",
                               border.shape.name = "arrow",
                               border.label.font.color = "red",
                               border.shape.color = "darkgoldenrod1",
                               border.flip.arrows.negative.values = TRUE,
                               border.width.min.value = 250, # arrows below 250 will all be plotted with the same (minimum) width
                               border.width.max.value = 1000, # arrows below 1000 will all be plotted with the same (maximum) width (set this parameter to NA to ignore this boundary)

                               save.plot = FALSE, save.data = FALSE)
mapFlows


## map with arrows on border data (and color gradient, rounding numbers to nearest tens)
mapFlowGradient <- eneRgymaps::mapBiddingZone (border.data = FlowsData,
                                      border.id.field.name = "border",
                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                      border.label.value.field.name = "Average", border.label.rounding.ndigits = -1,
                                      border.color.value.field.name = "Average", border.legend.color.name = "Flows",
                                      border.shape.name = "arrow",
                                      border.flip.arrows.negative.values = TRUE,
                                      border.shape.width = 1.5,
                                      border.color.gradient.colors = c ("yellow", "blue", "green"),

                                      save.plot = FALSE, save.data = FALSE)
mapFlowGradient


## map with arrows on border data (and color gradient with min/max color value)
mapFlowGradientMinMax <- eneRgymaps::mapBiddingZone (border.data = FlowsData,
                                      border.id.field.name = "border",
                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                      border.label.value.field.name = "Average",
                                      border.color.value.field.name = "Average", border.legend.color.name = "Flows",
                                      border.shape.name = "arrow",
                                      border.flip.arrows.negative.values = TRUE,
                                      border.shape.width = 1.5,
                                      border.color.gradient.colors = c ("yellow", "blue", "green"),
                                      border.color.min.value = 10, # any value below will be depicted as yellow
                                      border.color.max.value = 500, # any value above will be depicted as green

                                      save.plot = FALSE, save.data = FALSE)
mapFlowGradientMinMax


## map relying on colors list for depicting border types, fixed user-defined line width, and user-defined color legend names (with customised title)
mapBorderType <- eneRgymaps::mapBiddingZone (border.data = FlowsData,
                                      border.id.field.name = "border",
                                      border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                      border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                      border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                      border.shape.name = "line",
                                      border.colors.list = c ("green", "yellow"),
                                      border.shape.width = 3,

                                      title.text = "Border type", title.size = 15, title.color = "purple",

                                      save.plot = FALSE, save.data = FALSE)
mapBorderType


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


## map relying on fixed shapes to describe border types, and border labels (and caption)
mapBorderTypeFixShape <- eneRgymaps::mapBiddingZone (border.data = FlowsData,
                                                     border.id.field.name = "border",
                                                     border.in.id.field.name = "in_area_code", border.out.id.field.name = "out_area_code",
                                                     border.color.value.field.name = "interconnector_type", border.legend.color.name = "Interconnector type",
                                                     border.label.value.field.name = "Average",
                                                     border.legend.color.keys = c ("DC", "AC"), border.legend.color.values = c ("direct current" ,"alternating current"),
                                                     border.shape.name = 17,
                                                     border.colors.list = c ("green", "yellow"),
                                                     border.shape.width = 9,

                                                     caption.text = "2017 data", caption.size = 15, caption.color = "purple", caption.face = "italic",

                                                     save.plot = FALSE, save.data = FALSE)
mapBorderTypeFixShape

# help for setting shapes
eneRgymaps::help.shapes()

# help to set linetypes
eneRgymaps::help.path.linetypes()
