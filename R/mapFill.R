# Display geographic maps (and fill geographic entities)
#' Enrich a geographical map by filling geographic entities
#'
#' @param geographicMap the map to enrich
#' @param date date (in order to select the right geographic shapes for bidding-zones). If empty, set to today
#' @param data the data for which to fill the map as a data.frame
#' @param geographic.entity.name name of the geographic entities to display ("Bidding zone", "Control area", "Country"...)
#' @param id.field.name the name of the column to use when mapping geographic entity to fill the map
#' @param value.field.name the field to use to fill the entities
#' @param label.first.field.name the field to use as a label on top of the filled color
#' @param label.second.field.name the field to use as a second label
#' @param label.rounding.ndigits the rounding to apply to numerical values in labels
#' @param label.font.color the color to use for the label font
#' @param label.font.size the size to use for the label font (NULL -> auto size)
#' @param label.with.frame whether to draw a frame around the label
#' @param legend.name the name to display along the legend for the fill part (NULL -> no legend displayed, "" -> legend displayed without title)
#' @param legend.keys a list of keys to be used to customise the legend display
#' @param legend.values a list of values to customise the legend
#' @param color.palette the palette to use to display the fill colors
#' @param color.gradient.colors a vector of colors defining a fill color gradient
#' @param color.gradient.values values to associate to each gradient color (optional, NULL -> slope of the color gradient automatically defined)
#' @param color.gradient.perc.values same as color.gradient.values, as relative values (e.g. 0.01 for 1%)
#' @param na.color the color to use when filling areas described by NA
#' @param color.min.value the value to associate with the minimum color (any value below will be treated as minimum color). Only applies to a palette or a gradient.
#' @param color.max.value the value to associate with the maximum color (any value above will be treated as maximum color). Only applies to a palette or a gradient.
#' @param colors.list a list/vector of colors to use (only with discrete variables)
#' @param center.shape.value.field.name the field name to use to depict a shape at the center of the geographic entity
#' @param center.shape.color.field.name the field name to use to depict a shape color at the center of the geographic entity
#' @param center.shape.width.field.name the field name to use to depict a shape width at the center of the geographic entity
#' @param center.shape.name the (fixed) shape name/identifier to use to depict a shape at the center of the geographic entity
#' @param center.shape.color the (fixed) shape color to use to depict a shape at the center of the geographic entity
#' @param center.shape.width the (fixed) shape width to use to depict a shape at the center of the geographic entity
#' @param center.shape.solid whether to fill the shape (or to only draw the shape border)
#' @param legend.center.shape.name the name to display along the legend for the center shape part (NULL -> hidden, "" -> displayed without title)
#' @param legend.center.shape.keys a list of keys to be used to customize the legend related to center shapes
#' @param legend.center.shape.values a list of values to be used to customize the legend related to center shapes
#' @param legend.center.shape.width.name the name to display along the legend for the center shape width part (NULL -> hidden, "" -> displayed without title)
#' @param legend.center.shape.color.name the name to display along the legend for the center shape color part (NULL -> hidden, "" -> displayed without title)
#' @param legend.center.shape.keep.na whether the legend for shape should keep NAs
#' @param legend.center.shape.color.keys a list of keys to be used to customize the legend related to center shapes colors
#' @param legend.center.shape.color.values a list of values to be used to customize the legend related to center shapes colors
#' @param center.shape.color.palette the palette to use to display the center shapes
#' @param center.shape.color.gradient.colors a vector of colors defining a color gradient for the center shapes
#' @param center.shape.color.gradient.values values to associate to each gradient color (optional) for the center shapes
#' @param center.shape.color.gradient.perc.values values (as percentages between min and max) to associate to each gradient color (optional) for the center shapes
#' @param center.shape.color.min.value the value to associate with the minimum color (anything below will be treated as minimum color) for the center shape. Only applies to a palette or a gradient.
#' @param center.shape.color.max.value the value to associate with the maximum color (anything above will be treated as maximum color) for the center shape. Only applies to a palette or a gradient.
#' @param center.shape.colors.list a list/vector of colors to use for the center shapes (only with discrete variables)
#' @param center.shape.width.min.value the value to associate with the minimum width (anything below will be treated as minimum width) for the center shape.
#' @param center.shape.width.max.value the value to associate with the maximum width (anything above will be treated as maximum width) for the center shape.
#' @param borders.color the color to use to draw borders between areas (if NULL, no border is drawn)
#' @param borders.width the width to use to draw borders between areas (if zero, no border is drawn)
#' @param borders.linetype the type of line to use to draw borders (see help.path.linetypes)
#' @param data.path the path in which to save the data set required in order to build the plot
#' @param save.data whether to save the data set required to build the plot
#' @return the updated map as a ggplot2 plot
#' @importFrom dplyr rename select
#' @importFrom sf st_bbox
mapFill <- function (geographicMap, date = base::Sys.Date(),
                     data = NULL, geographic.entity.name,
                     id.field.name = NULL, value.field.name = NULL,

                     label.first.field.name = NULL, label.second.field.name = NULL,
                     label.font.color = "black", label.font.size = NULL,
                     label.rounding.ndigits = NULL, label.with.frame = FALSE,
                     legend.name = NULL, legend.keys = NULL, legend.values = NULL,
                     color.palette = NULL, na.color = "lightgrey",
                     color.gradient.colors = NULL, color.gradient.values = NULL, color.gradient.perc.values = NULL,
                     color.min.value = NULL, color.max.value = NULL,
                     colors.list = NULL,

                     center.shape.value.field.name = NULL,
                     center.shape.color.field.name = NULL, center.shape.width.field.name = NULL,
                     center.shape.name = NULL, center.shape.color = NULL, center.shape.width = NULL, center.shape.solid = TRUE,
                     legend.center.shape.name = NULL, legend.center.shape.keys = NULL, legend.center.shape.values = NULL,
                     legend.center.shape.width.name = NULL,
                     legend.center.shape.color.name = NULL,
                     legend.center.shape.color.keys = NULL, legend.center.shape.color.values = NULL,
                     legend.center.shape.keep.na = NULL,
                     center.shape.color.palette = NULL,
                     center.shape.color.gradient.colors = NULL, center.shape.color.gradient.values = NULL, center.shape.color.gradient.perc.values = NULL,
                     center.shape.color.min.value = NULL, center.shape.color.max.value = NULL,
                     center.shape.colors.list = NULL,
                     center.shape.width.min.value = NULL, center.shape.width.max.value = NULL,

                     borders.color, borders.width, borders.linetype,

                     data.path = NULL, save.data = TRUE) {

  # When an id/value field is set, the data set should not be NULL
  stopifnot(! ((length (data) == 0) && ((! is.null(id.field.name)) || (! is.null(value.field.name)))))

  # if the second value field name is not null, the first one should not be null
  stopifnot ((is.null(label.second.field.name)) || (! is.null (label.first.field.name)))

  # should only save if output path is not NULL
  stopifnot ((! is.null(data.path)) || (! save.data))

  # When a fill value is set, it should be in the data frame
  stopifnot ((is.null(value.field.name)) || (value.field.name %in% names(data)))

  # Shape should only be set once (but error management is done within the low-level function)
  shapesDefined <- length (c (center.shape.name, center.shape.value.field.name)) > 0
  labelsFields <- c (label.first.field.name, label.second.field.name)
  labelsDefined <- length(labelsFields) > 0

  # forbidden to define both UK and either GB or NI
  # forbidden to define both IE (SEM) and either IE or NI
  UKinMap <- FALSE
  GBNIinMap <- FALSE
  IENIinMap <- FALSE
  IESEMinMap <- FALSE
  if ((geographic.entity.name == "Country") && (! is.null(id.field.name))) {
    UKinMap <- "UK" %in% data[[id.field.name]]
    GBNIinMap <- any(c("GB", "NI") %in% data[[id.field.name]])
    stopifnot ((! UKinMap) || (! GBNIinMap))
    if (GBNIinMap) {
      message("GB and NI do not refer to countries, but are still displayed as such")
    }

    IESEMinMap <- "IE (SEM)" %in% data[[id.field.name]]
    IENIinMap <- any(c("IE", "NI") %in% data[[id.field.name]])
    stopifnot ((! IESEMinMap) || (! IENIinMap))
    if (IESEMinMap) {
      message("IE (SEM) does not refer to a country, but is still displayed as such")
    }

    # forbidden to define both IE (SEM) and UK (because the two would overlap)
    stopifnot ((! UKinMap) || (! IESEMinMap))
  }

  # forbidden to define both DE/AT and either AT or DE
  joinATDEBZinMap <- FALSE
  individualATDEBZinMap <- FALSE
  if ((geographic.entity.name == "Bidding zone") && (! is.null(id.field.name))) {
    ATDEInMap <- eneRgymaps::merged.split.AT.DE.in.data (data[[id.field.name]])
    joinATDEBZinMap <- ATDEInMap$merged
    individualATDEBZinMap <- ATDEInMap$split
    stopifnot ((! joinATDEBZinMap) || (! individualATDEBZinMap))
  }

  # forbidden to define DE/LU and either 50Hz, Amprion, Creos, TransnetBW, or TenneT
  DELUCBInMap <- FALSE
  DELUTSOsInMap <- FALSE
  if ((geographic.entity.name == "Control area") && (! is.null(id.field.name))) {
    DELUCBInMap <- eneRgymaps::eic.code.DELU() %in% data[[id.field.name]]
    DELUTSOs <- c (eneRgymaps::eic.codes.DE.TSOs(), eneRgymaps::eic.code.LU.TSO())
    DELUTSOsInMap <- any(DELUTSOs %in% data[[id.field.name]])
    stopifnot ((! DELUCBInMap) || (! DELUTSOsInMap))
  }

  # colors should be valid
  stopifnot (all (eneRgymaps::valid.colors (colors = c (borders.color, label.font.color, na.color, color.gradient.colors, colors.list,
                                                        center.shape.color, center.shape.color.gradient.colors, center.shape.colors.list) %>% unique(), silent = FALSE)))

  # Only keep useful variables
  # no need for omitting NULL elements, as R automatically does it for vectors
  usefullVariables <- unique(c(id.field.name, value.field.name, label.first.field.name, label.second.field.name,
                        center.shape.value.field.name, center.shape.color.field.name, center.shape.width.field.name))
  plotData <- data.frame()
  if ((! is.null(usefullVariables)) && (length(usefullVariables) > 0)) {
    plotData <- data %>%
      dplyr::select(dplyr::all_of(usefullVariables))
  }

  # exactly one value per id should be defined
  if (nrow (plotData) > 0) {
    stopifnot (nrow (plotData) == length (unique (plotData [[id.field.name]])))
  }

  # convert variable to factor when relying on colors.list
  if (! is.null(colors.list)) {
    if (! inherits(plotData [[value.field.name]], "factor")) {
      plotData[[value.field.name]] <- as.factor(plotData[[value.field.name]])
    }
  }

  withCentroids <- labelsDefined || shapesDefined
  geographicData <- eneRgymaps::geographic.shapes (geographic.entity.name = geographic.entity.name, date = date,
                                                   only.keep.Europe = TRUE, with.centroids = withCentroids)

  idColumnName <- NULL
  if (geographic.entity.name == "Country") {
    idColumnName <- "iso2"
  } else if (geographic.entity.name %in% c("Bidding zone", "Control area")) {
    idColumnName <- "eic_code"
  }

  # remove useless duplicate shapes from geographic data
  # by default, consider that the AT-DE border should be present, unless the merged bidding-zone has been declared
  geographicData <- geographicData %>%
    clean.map.shapes.duplicates (geographic.entity.name = geographic.entity.name,
                                 id.column.name = idColumnName,
                                 GB.NI.in.map = GBNIinMap, IE.SEM.in.map = IESEMinMap,
                                 separate.DE.LU.TSOs.in.map = DELUTSOsInMap)

  # check whether a data set is available, and should be mapped with geographic coordinates
  dataWithId <- (length(data) > 0) && (! is.null(id.field.name))

  # coordinates to use to define the fill
  # as longitude.min, longitude.max, latitude.min, latitude.max
  fillFrameCoordinates <- eneRgymaps::uniform.geographic.interval(NA)

  plotDataWithGeometry <- data.frame()
  updatedMap <- geographicMap
  if (dataWithId) {
    geometryColumnsToKeep <- c("geometry")
    if (withCentroids) {
      geometryColumnsToKeep <- c(geometryColumnsToKeep, "centroid_longitude", "centroid_latitude")
    }
    plotDataWithGeometry <- eneRgymaps::enrich.with.geographic.coordinates(data = plotData, data.id.field.name = id.field.name,
                                                                           geographic.coordinates.data = geographicData, geographic.coordinates.data.id.field.name = idColumnName,
                                                                           geographic.coordinates.data.field.names = geometryColumnsToKeep)

    # update the frame coordinates based on all geographic shapes
    limits <- sf::st_bbox(plotDataWithGeometry)
    fillFrameCoordinates [["longitude.min"]] <- limits[[1]]
    fillFrameCoordinates [["longitude.max"]] <- limits[[3]]
    fillFrameCoordinates [["latitude.min"]] <- limits[[2]]
    fillFrameCoordinates [["latitude.max"]] <- limits[[4]]

    # add fill (and borders between items)
    if (! is.null(value.field.name)) {
      updatedMap <- updatedMap %>%
        eneRgymaps::add.fill.and.border (data = plotDataWithGeometry,
                                         fill.color.field = value.field.name, fill.keep.NA = FALSE,
                                         border.color = borders.color, border.width = borders.width,
                                         border.linetype = borders.linetype)
    }

    # Add shapes
    if (shapesDefined) {
      updatedMap <- updatedMap %>%
        eneRgymaps::add.shapes(data = plotDataWithGeometry,
                               coordinates.identifiers = c("centroid_longitude", "centroid_latitude", rep.int (x = NA, times = 4)),
                               shape.field.name = center.shape.value.field.name, shape = center.shape.name,
                               legend.shape.name = legend.center.shape.name, legend.shape.keys = legend.center.shape.keys, legend.shape.values = legend.center.shape.values,
                               width.field.name = center.shape.width.field.name, width = center.shape.width, width.min.value = center.shape.width.min.value, width.max.value = center.shape.width.max.value,
                               legend.width.name = legend.center.shape.width.name,
                               color.field.name = center.shape.color.field.name, color = center.shape.color,
                               solid = center.shape.solid,

                               color.palette = center.shape.color.palette, na.color = na.color,
                               color.gradient.colors = center.shape.color.gradient.colors, color.gradient.values = center.shape.color.gradient.values, color.gradient.perc.values = center.shape.color.gradient.perc.values,
                               colors.list = center.shape.colors.list,
                               color.min.value = center.shape.color.min.value, color.max.value = center.shape.color.max.value,
                               legend.color.name = legend.center.shape.color.name, legend.keep.na = legend.center.shape.keep.na,
                               legend.color.keys = legend.center.shape.color.keys, legend.color.values = legend.center.shape.color.values
        )
    }

    # add labels
    if (labelsDefined) {
      labelTextSize <- label.font.size
      if (is.null (labelTextSize)) {
        labelTextSize <- round (3 / sqrt(length(labelsFields)), digits = 0)
      }

      updatedMap <- updatedMap %>%
        eneRgymaps::add.labels (data = plotDataWithGeometry, coordinates.identifiers = c("centroid_longitude", "centroid_latitude"),
                               field.names = labelsFields, rounding.ndigits = label.rounding.ndigits,
                               font.color = label.font.color, font.size = labelTextSize, with.frame = label.with.frame)
    }

    # Customize the geographic entities fill
    if (! is.null(value.field.name)) {
      updatedMap <- updatedMap %>%
        eneRgymaps::configure.colors (data = plotData, configure.type = "fill", field.name = value.field.name,
                                     color.palette = color.palette, na.color = na.color,
                                     gradient.colors = color.gradient.colors, gradient.values = color.gradient.values, gradient.perc.values = color.gradient.perc.values,
                                     colors.list = colors.list, min.value = color.min.value, max.value = color.max.value,
                                     legend.name = legend.name, legend.keys = legend.keys, legend.values = legend.values)
    }
  }

  if (save.data) {
    dataSetToSave <- unique(plotData)

    if (nrow (dataSetToSave) > 0) {
      eneRgymaps::save.data (dataset = dataSetToSave, destination.path = data.path)
    }
  }

  return (list ("map" = updatedMap, "frame.coordinates" = fillFrameCoordinates))
}
