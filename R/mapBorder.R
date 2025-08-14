# Display geographic maps related to borders

#' Enrich a geographical map by depicting border-related information
#' the border is defined as out (origin) > in (destination)
#'
#' @param geographicMap the map to enrich
#' @param data the data for which to depict the borders' map as a data.frame
#' @param geographic.entity.name name of the geographic entities to display ("Bidding zone", "Control area", "Country"...)
#' @param id.field.name the name of the column to use when mapping borders
#' @param in.id.field.name the name of the column to use when mapping the area EIC code at the end of the oriented border
#' @param out.id.field.name the name of the column to use when mapping the area EIC code at the beginning of the oriented border
#' @param shape.value.field.name the field name to use to set the shape
#' @param width.value.field.name the field name to use to adapt the shape width
#' @param color.value.field.name the field name to use to adapt the shape color
#' @param label.value.field.name the field name to use to set the shape label
#' @param label.font.color the font color to use when writing labels
#' @param label.rounding.ndigits the number of digits to use when rounding label values (only for numeric values)
#' @param label.with.frame whether to draw a frame around the label
#' @param legend.shape.name the name of the legend related to the shapes
#' @param legend.width.name the name of the legend related to the shape width
#' @param legend.color.name the name of the legend related to the shape color
#' @param legend.color.values.as.percentages whether to display values in the color legend as percentages
#' @param legend.color.keys a list of keys used to customize the legend related to colors
#' @param legend.color.values a list of values to customize the legend related to colors
#' @param legend.keep.na whether to keep NAs in the legend
#' @param legend.shape.keys a list of keys to be used to customize the legend related to shapes
#' @param legend.shape.values a list of values to customize the legend related to shapes
#' @param flip.arrows.negative.values whether to flip arrows (and use positive values) when a negative value is reported for a border
#' @param shape.name the name of the shape to use ("line", "arrow", number -> fixed shape defined by integer - see help.shapes() -)
#' @param shape.color the (fixed) color to use to describe shapes
#' @param shape.width the (fixed) arrow width
#' @param shape.solid whether to fill the shape (or to only fill the shape border)
#' @param colors.list a list/vector of colors to use (only with discrete variables) to describe borders
#' @param na.color the color to use for borders with NA
#' @param color.palette the palette to use to display the border colors
#' @param color.gradient.colors a vector of colors defining a color gradient
#' @param color.gradient.perc.values threshold values (as percentages between min and max) to associate to each gradient color
#' @param color.gradient.values threshold values to associate to each gradient color
#' @param color.min.value the value to associate with the minimum color (anything below will be treated as minimum color). Only applies to a palette or a gradient.
#' @param color.max.value the value to associate with the maximum color (anything above will be treated as maximum color). Only applies to a palette or a gradient.
#' @param width.min.value the value to associate with the minimum shape width (anything below will be treated as minimum width)
#' @param width.max.value the value to associate with the maximum shape width (anything above will be treated as maximum width)
#' @param data.path the path in which to save the data set required in order to build the plot
#' @param save.data whether to save the data set required to build the plot
#' @return the updated map as a ggplot2 plot, and the data set as a data frame
#' @importFrom dplyr all_of select
mapBorder <- function (geographicMap,
                       data = NULL, geographic.entity.name,
                       id.field.name = NULL, in.id.field.name = NULL, out.id.field.name = NULL,
                       shape.value.field.name = NULL,
                       width.value.field.name = NULL, color.value.field.name = NULL, label.value.field.name = NULL,
                       label.font.color = "black", label.rounding.ndigits = NULL, label.with.frame = TRUE,
                       legend.shape.name = NULL, legend.width.name = NULL,
                       legend.color.name = NULL, legend.color.values.as.percentages = NULL,
                       legend.color.keys = NULL, legend.color.values = NULL,
                       legend.shape.keys = NULL, legend.shape.values = NULL,
                       legend.keep.na = NULL,
                       flip.arrows.negative.values = FALSE,
                       shape.name = NULL,
                       shape.color = NULL, shape.width = NULL, shape.solid = TRUE,
                       color.palette = NULL, na.color = "lightgrey",
                       color.gradient.colors = NULL, color.gradient.perc.values = NULL, color.gradient.values = NULL,
                       colors.list = NULL, color.min.value = NULL, color.max.value = NULL,
                       width.min.value = NULL, width.max.value = NULL,
                       data.path = NULL, save.data = TRUE) {

  # no data => nothing to do
  if (length(data) == 0) {
    return (list ("map" = geographicMap, "frame.coordinates" = eneRgymaps::uniform.geographic.interval(NA)))
  }

  # Control area borders' map not implemented yet
  stopifnot(geographic.entity.name %in% c ("Bidding zone", "Country"))

  # When an id/value field is set, the data set should not be NULL
  stopifnot(! ((length(data) == 0) &&
                 ((! is.null(id.field.name)) || (! is.null(in.id.field.name)) || (! is.null(out.id.field.name)) || (! is.null(width.value.field.name)) || (! is.null(color.value.field.name)) || (! is.null(label.value.field.name)))))

  # Both the id field, the in and out ids should also be set (and point towards different fields)
  stopifnot (length(unique(c(id.field.name, in.id.field.name, out.id.field.name))) == 3)

  # at least one of width, color, label, shape should be set
  stopifnot(length(c(width.value.field.name, color.value.field.name, label.value.field.name, shape.value.field.name)) >= 1)

  # define an object of length one related to a colors list, in order to be able to easily test whether exactly one color set is defined
  # because adding a list (of length 2) to another (empty) list does not lead to a list of length 1, but to a list of length 2.
  colorsListOneLength <- NULL
  if (! is.null(colors.list)) {
    colorsListOneLength <- "temp"
  }
  colorsGradientOneLength <- NULL
  if (! is.null (color.gradient.colors)) {
    colorsGradientOneLength <- "temp"
  }
  # One (and only one) color option setting should be set
  stopifnot(length(c(color.palette, shape.color, colorsGradientOneLength, colorsListOneLength)) == 1)

  # forbidden field id names
  crossBorderDFFields <- c("out_area_code", "in_area_code", "coor_out_long", "coor_out_lat", "coor_in_long", "coor_in_lat", "coor_cent_lat", "coor_cent_long")
  stopifnot(length(intersect(c(id.field.name, width.value.field.name, color.value.field.name, label.value.field.name, shape.value.field.name), crossBorderDFFields)) == 0)

  # Shape should only be set once
  stopifnot (length (c (shape.name, shape.value.field.name)) == 1) # may be <= 1 in the future
  shapesDefined <- length (c (shape.name, shape.value.field.name)) == 1

  # colors should be valid
  stopifnot (all (eneRgymaps::valid.colors (colors = unique(c(label.font.color, shape.color, na.color, color.gradient.colors, colors.list)), silent = FALSE)))

  # only keep useful fields from the dataset
  usefullFields <- unique(c(width.value.field.name, color.value.field.name, label.value.field.name, shape.value.field.name,
                     id.field.name, in.id.field.name, out.id.field.name))
  plotData <- data %>%
    dplyr::select(dplyr::all_of(usefullFields))

  # exactly one value per id should be defined
  if (nrow (plotData) > 0) {
    stopifnot (nrow (plotData) == length (unique (plotData [[id.field.name]])))
  }

  # check whether the plot will include arrows
  # (to flip the data when using oriented arrows)
  shapeArrow <- FALSE
  if (! is.null (shape.name)) {
    shapeArrow <- shape.name == "arrow"
  }

  # flipping negative values is only allowed for arrows
  stopifnot ((! flip.arrows.negative.values) | shapeArrow)
  if (shapeArrow & flip.arrows.negative.values & (length(c(width.value.field.name, label.value.field.name)) > 0)) {
    plotData <- plotData %>%
      flip.negative.oriented.border.values (id.field.name = id.field.name, in.id.field.name = in.id.field.name, out.id.field.name = out.id.field.name,
                                        width.value.field.name = width.value.field.name, label.value.field.name = label.value.field.name)
  }

  # convert variable to factor when relying on colors.list
  if (! is.null(colors.list)) {
    if (! inherits(plotData [[color.value.field.name]], "factor")) {
      plotData[[color.value.field.name]] <- as.factor(plotData[[color.value.field.name]])
    }
  }

  # retrieve the coordinates of the various border points
  coordinateFields <- c ("coor_cent_long", "coor_cent_lat", "coor_out_long", "coor_out_lat", "coor_in_long", "coor_in_lat")
  borderPoints <- plotData %>%
    enrich.with.geographic.coordinates (data.id.field.name = c (out.id.field.name, in.id.field.name), geographic.coordinates.data = eneRgymaps::geographic.border.points(),
                                        geographic.coordinates.data.id.field.name = c ("out_area_code", "in_area_code"),
                                        geographic.coordinates.data.field.names = coordinateFields)

  borderFrameCoordinates <- list ("longitude.min" = min (borderPoints[["coor_cent_long"]], borderPoints[["coor_out_long"]], borderPoints[["coor_in_long"]], na.rm = TRUE),
                                  "longitude.max" = max (borderPoints[["coor_cent_long"]], borderPoints[["coor_out_long"]], borderPoints[["coor_in_long"]], na.rm = TRUE),
                                  "latitude.min" = min (borderPoints[["coor_cent_lat"]], borderPoints[["coor_out_lat"]], borderPoints[["coor_in_lat"]], na.rm = TRUE),
                                  "latitude.max" = max (borderPoints[["coor_cent_lat"]], borderPoints[["coor_out_lat"]], borderPoints[["coor_in_lat"]], na.rm = TRUE))

  updatedMap <- geographicMap

  # Add the borders shapes
  if (shapesDefined) {
    updatedMap <- updatedMap %>%
      eneRgymaps::add.shapes(data = borderPoints,
                             coordinates.identifiers = coordinateFields,
                             shape.field.name = shape.value.field.name, shape = shape.name,
                             legend.shape.name = legend.shape.name, legend.shape.keys = legend.shape.keys, legend.shape.values = legend.shape.values,
                             width.field.name = width.value.field.name, width = shape.width, width.min.value = width.min.value, width.max.value = width.max.value,
                             legend.width.name = legend.width.name,
                             color.field.name = color.value.field.name, color = shape.color,
                             solid = shape.solid,
                             color.palette = color.palette, na.color = na.color,
                             color.gradient.colors = color.gradient.colors, color.gradient.values = color.gradient.values, color.gradient.perc.values = color.gradient.perc.values,
                             colors.list = colors.list,
                             color.min.value = color.min.value, color.max.value = color.max.value,
                             legend.color.name = legend.color.name, legend.color.values.as.percentages = legend.color.values.as.percentages,
                             legend.keep.na = legend.keep.na,
                             legend.color.keys = legend.color.keys, legend.color.values = legend.color.values)
  }

  # Add the borders labels
  if (! is.null (label.value.field.name)) {
    updatedMap <- updatedMap %>%
      eneRgymaps::add.labels (data = borderPoints, coordinates.identifiers = c ("coor_cent_long", "coor_cent_lat"),
                              field.names = c(label.value.field.name), rounding.ndigits = label.rounding.ndigits,
                              font.color = label.font.color, with.frame = label.with.frame)
  }

  # format data set in case it needs to be saved
  if (save.data) {
    dataSetToSave <- unique(plotData)

    if (nrow (dataSetToSave) > 0) {
      eneRgymaps::save.data (dataset = dataSetToSave, destination.path = data.path)
    }
  }

  return (list ("map" = updatedMap, "frame.coordinates" = borderFrameCoordinates))
}
