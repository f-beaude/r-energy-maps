# Display geographic maps

#' Display a geographical map
#' The map is specified through :
#' - fill information (filling geographic entities, and adding labels and/or shapes)
#' - border information (depicting shapes between geographic entities)
#' - parameters for the general plot display (zoom, title, caption...)
#' - saving the generated data set and/or plot
#'
#' @param geographic.entity.name name of the geographic entities to display ("Bidding zone", "Control area"/"LFC area", "Country"...)
#' @param date date (in order to select the right geographic shapes). NULL means relying on today's date
#' @param fill.data the data for which to fill the map as a data.frame
#' @param fill.id.field.name the name of the column to use when mapping geographic entity to fill the map
#' @param fill.value.field.name the field to use to fill the entities
#' @param fill.label.first.field.name the field to use as a label on top of the filled color
#' @param fill.label.second.field.name the field to use as a second label
#' @param fill.label.font.color the font color to use for the fill label
#' @param fill.label.font.size the font size to use for the fill label (NULL -> automatically-defined size)
#' @param fill.label.rounding.ndigits the number of digits to use when rounding label numbers (NULL -> display numbers as provided)
#' @param fill.label.with.frame whether to draw a frame around the fill label
#' @param fill.legend.name the name to display along the legend for the fill part (NULL -> no legend, "" -> legend with no name)
#' @param fill.legend.values.as.percentages whether to display percentage values in the legend for the fill part
#' @param fill.legend.keys a list of keys to be used to customize the legend for the fill part
#' @param fill.legend.values a list of values to customize the legend for the fill part
#' @param fill.color.palette the palette to use to display the fill colors
#' @param fill.na.color the color to use when filling areas described by an NA
#' @param fill.color.gradient.colors the vector of colors when defining a fill color gradient
#' @param fill.color.gradient.values values to associate to each gradient color (optional). NULL -> values automatically defined
#' @param fill.color.gradient.perc.values values (as percentiles) to associate to each gradient color (optional)
#' @param fill.colors.list a list/vector of colors to use (only with discrete variables) to fill
#' @param fill.color.min.value the value to associate with the minimum color (anything below will be treated as minimum color). Only applies to a palette or a gradient.
#' @param fill.color.max.value the value to associate with the maximum color (anything above will be treated as maximum color). Only applies to a palette or a gradient.
#' @param fill.center.shape.value.field.name the field name to use to depict a shape at the center of the geographic entity (see help.shapes)
#' @param fill.center.shape.color.field.name the field name to use to depict a shape color at the center of the geographic entity
#' @param fill.center.shape.width.field.name the field name to use to depict a shape width at the center of the geographic entity
#' @param fill.center.shape.name the (fixed) shape name/identifier to use to depict a shape at the center of the geographic entity
#' @param fill.center.shape.color the (fixed) shape color to use to depict a shape at the center of the geographic entity
#' @param fill.center.shape.width the (fixed) shape width to use to depict a shape at the center of the geographic entity
#' @param fill.center.shape.solid whether to fill the shape (or to only depict the shape border)
#' @param fill.legend.center.shape.name the name to display along the legend for the center shape part (NULL -> hidden, "" -> displayed without title)
#' @param fill.legend.center.shape.keys a list of keys to be used to customize the legend related to center shapes
#' @param fill.legend.center.shape.values a list of values to be used to customize the legend related to center shapes
#' @param fill.legend.center.shape.width.name the name to display along the legend for the center shape width part (NULL -> hidden, "" -> displayed without title)
#' @param fill.legend.center.shape.color.name the name to display along the legend for the center shape color part (NULL -> hidden, "" -> displayed without title)
#' @param fill.legend.center.shape.color.values.as.percentages whether to display percentage values in the legend for the center shape color part
#' @param fill.legend.center.shape.keep.na whether the legend for fill shape should keep NAs
#' @param fill.legend.center.shape.color.keys a list of keys to be used to customize the legend related to center shapes colors
#' @param fill.legend.center.shape.color.values a list of values to be used to customize the legend related to center shapes colors
#' @param fill.center.shape.color.palette the palette to use to display the center shapes
#' @param fill.center.shape.color.gradient.colors a vector of colors defining a color gradient for the center shapes
#' @param fill.center.shape.color.gradient.values values to associate to each gradient color (optional) for the center shapes
#' @param fill.center.shape.color.gradient.perc.values values (as percentages between min and max, described as 0 and 1) to associate to each gradient color (optional) for the center shapes
#' @param fill.center.shape.color.min.value the value to associate with the minimum color (anything below will be treated as minimum color) for the center shape. Only applies to a palette or a gradient.
#' @param fill.center.shape.color.max.value the value to associate with the maximum color (anything above will be treated as maximum color) for the center shape. Only applies to a palette or a gradient.
#' @param fill.center.shape.colors.list a list/vector of colors to use for the center shapes (only with discrete variables)
#' @param fill.center.shape.width.min.value the value to associate with the minimum width (anything below will be treated as minimum width) for the center shape.
#' @param fill.center.shape.width.max.value the value to associate with the maximum width (anything above will be treated as maximum width) for the center shape.
#' @param fill.borders.color the color to apply to draw borders between areas (if NULL, no border is drawn)
#' @param fill.borders.width the width of the border between areas (if 0, no border is drawn)
#' @param fill.borders.linetype the line type of the border between areas (see help.path.linetypes)

#' @param border.data the border data to display on the map as a data.frame
#' @param border.id.field.name the name of the column to use when mapping borders
#' @param border.in.id.field.name the name of the column to use when mapping the entry side of a border
#' @param border.out.id.field.name the name of the column to use when mapping the exit side of a border
#' @param border.width.value.field.name the field to use to display information as width on the borders
#' @param border.color.value.field.name the field to use to display information as color on the borders
#' @param border.shape.value.field.name the field to use to set shapes on the borders (see help.shapes)
#' @param border.label.value.field.name the field to use to display information as label on the borders
#' @param border.label.font.color the font color to use when writing the border label
#' @param border.label.rounding.ndigits the number of digits to use when rounding label numbers (NULL -> display numbers as provided)
#' @param border.label.with.frame whether to draw a frame around the fill label
#' @param border.legend.shape.name the name to display along the legend for the border shape part (NULL -> no legend, "" -> legend with no name)
#' @param border.legend.width.name the name to display along the legend for the border width part (NULL -> no legend, "" -> legend with no name)
#' @param border.legend.color.name the name to display along the legend for the border color part (NULL -> no legend, "" -> legend with no name)
#' @param border.legend.color.values.as.percentages whether to display percentage values in the legend for the border color part
#' @param border.legend.color.keys a list of keys to be used to customize the legend related to border colors
#' @param border.legend.color.values a list of values to customize the legend related to border colors
#' @param border.legend.shape.keys a list of keys to be used to customize the legend related to border shapes
#' @param border.legend.shape.values a list of values to customize the legend related to border shapes
#' @param border.legend.keep.na whether to keep NAs in the border legend
#' @param border.flip.arrows.negative.values whether to flip arrows (and use positive values) when a negative value is reported for a border
#' @param border.shape.name the name of the shape to use ("line", "arrow", number -> fixed shape defined by integer - see help.shapes -)
#' @param border.shape.color the (fixed) color of the shape
#' @param border.shape.width a fixed shape width
#' @param border.shape.solid whether to fill the border shape (or to only keep the shape border)
#' @param border.color.palette the palette to use to display the border colors
#' @param border.color.gradient.colors the vector of colors when defining a border color gradient
#' @param border.color.gradient.perc.values values (as percentages between min and max, described as 0 and 1) to associate to each gradient color
#' @param border.color.gradient.values values to associate to each gradient color
#' @param border.colors.list a list/vector of colors to use (only with discrete variables) to describe borders
#' @param border.na.color the color to use when describing borders described by an NA
#' @param border.color.min.value the value to associate with the minimum border color (anything below will be treated as minimum color). Only applies to a palette or a gradient.
#' @param border.color.max.value the value to associate with the maximum border color (anything above will be treated as maximum color). Only applies to a palette or a gradient.
#' @param border.width.min.value the value to associate with the minimum border shape width (anything below will be treated as minimum width)
#' @param border.width.max.value the value to associate with the maximum border shape width (anything above will be treated as maximum width)

#' @param geographic.entity.to.label the name of the geographic entity for which to display labels ("Bidding zone", "Control area"/"LFC area", "Country"...)
#' @param geographic.entity.to.label.political.initiatives filter on political initiatives for which to display the geographic entities' labels ("EC" - Energy Community, "EU"). NULL to keep all available entities
#' @param geographic.entity.label.font.color the color of the font to use when labeling geographic entities
#'
#' @param background.iso2s.to.ignore a vector of country ids (ISO2) to ignore when depicting the background, i.e. for which not to display geographic entities

#' @param legend.position the position of the legend. Either a string ("none" -> no legend displayed, "left", "top", "right", "bottom"), or two coordinates (c (x, y), c(0,0) -> bottom left, c(1,1) -> top right)
#' @param legend.text.size the size of the font to use for the legend
#' @param manual.frame.coordinates the custom coordinates defining the plot frame (NULL -> automatically defined).
#' Should be list ("longitude.min" = longitude.min, "longitude.max" = longitude.max, "latitude.min" = latitude.min, "latitude.max" = latitude.max)
#' @param automated.frame.margins margin levels to use around the automated coordinates (NULL -> no margin).
#' Should be list ("longitude.min" = longitude.min.margin, "longitude.max" = longitude.max.margin, "latitude.min" = latitude.min.margin, "latitude.max" = latitude.max.margin)
#' @param aspect.ratio.min minimum aspect ratio of the plot as a float (to configure automated zoom)
#' @param aspect.ratio.max maximum aspect ratio of the plot as a float (to configure automated zoom)
#' @param map.name the name of the map (when saving the plot, without file extension)
#' @param title.text the title of the map to display (NULL -> no title)
#' @param title.size the size of the title to use
#' @param title.color the color of the title to use
#' @param title.face the face of the title (e.g. "italic") (NULL -> standard text)
#' @param caption.text the caption of the map to display (NULL -> no caption)
#' @param caption.size the size of the caption to use
#' @param caption.color the color of the caption to use
#' @param caption.face the face of the caption (e.g. "italic") (NULL -> standard text)
#'
#' @param output.directory the directory in which to save the graphics/data
#' @param save.plot whether to save the generated plots in the output directory
#' @param save.data whether to save the generated data as a csv file
#' @return the map as a ggplot2 plot
#' @importFrom eneRgycache cache.initialised
#' @importFrom ggplot2 theme element_text
#' @importFrom stats na.omit
mapGeographic <- function (geographic.entity.name, date = base::Sys.Date(),
                           fill.data = NULL, fill.id.field.name = NULL, fill.value.field.name = NULL,
                           fill.label.first.field.name = NULL, fill.label.second.field.name = NULL,
                           fill.label.font.color = "black", fill.label.font.size = NULL, fill.label.rounding.ndigits = NULL, fill.label.with.frame = FALSE,
                           fill.legend.name = NULL, fill.legend.values.as.percentages = NULL,
                           fill.legend.keys = NULL, fill.legend.values = NULL,
                           fill.color.palette = NULL, fill.na.color = "lightgrey",
                           fill.color.gradient.colors = NULL, fill.color.gradient.values = NULL, fill.color.gradient.perc.values = NULL,
                           fill.colors.list = NULL,
                           fill.color.min.value = NULL, fill.color.max.value = NULL,
                           fill.center.shape.value.field.name = NULL,
                           fill.center.shape.color.field.name = NULL, fill.center.shape.width.field.name = NULL,
                           fill.center.shape.name = NULL, fill.center.shape.color = NULL, fill.center.shape.width = NULL, fill.center.shape.solid = TRUE,
                           fill.legend.center.shape.name = NULL, fill.legend.center.shape.keys = NULL, fill.legend.center.shape.values = NULL,
                           fill.legend.center.shape.width.name = NULL,
                           fill.legend.center.shape.color.name = NULL, fill.legend.center.shape.color.values.as.percentages = NULL,
                           fill.legend.center.shape.keep.na = NULL,
                           fill.legend.center.shape.color.keys = NULL, fill.legend.center.shape.color.values = NULL,
                           fill.center.shape.color.palette = NULL,
                           fill.center.shape.color.gradient.colors = NULL, fill.center.shape.color.gradient.values = NULL, fill.center.shape.color.gradient.perc.values = NULL,
                           fill.center.shape.color.min.value = NULL, fill.center.shape.color.max.value = NULL,
                           fill.center.shape.colors.list = NULL,
                           fill.center.shape.width.min.value = NULL, fill.center.shape.width.max.value = NULL,
                           fill.borders.color = "white", fill.borders.width = 0.35, fill.borders.linetype = 1,

                           border.data = NULL, border.id.field.name = NULL, border.in.id.field.name = NULL, border.out.id.field.name = NULL,
                           border.width.value.field.name = NULL, border.color.value.field.name = NULL, border.label.value.field.name = NULL,
                           border.shape.value.field.name = NULL,
                           border.label.font.color = "black", border.label.rounding.ndigits = NULL, border.label.with.frame = TRUE,
                           border.legend.shape.name = NULL, border.legend.width.name = NULL,
                           border.legend.color.name = NULL, # no legend for label, because it is not satisfactorily displayed
                           border.legend.color.values.as.percentages = NULL,
                           border.legend.color.keys = NULL, border.legend.color.values = NULL,
                           border.legend.shape.keys = NULL, border.legend.shape.values = NULL,
                           border.legend.keep.na = NULL,
                           border.flip.arrows.negative.values = FALSE,
                           border.shape.name = NULL, border.shape.width = NULL, border.shape.color = NULL, border.shape.solid = TRUE,
                           border.na.color = "lightgrey",
                           border.color.palette = NULL,
                           border.color.gradient.colors = NULL,
                           border.color.gradient.values = NULL, border.color.gradient.perc.values = NULL,
                           border.colors.list = NULL,
                           border.color.min.value = NULL, border.color.max.value = NULL,
                           border.width.min.value = NULL, border.width.max.value = NULL,

                           geographic.entity.to.label = NULL, geographic.entity.to.label.political.initiatives = NULL,
                           geographic.entity.label.font.color = "black",

                           background.iso2s.to.ignore = NULL,

                           legend.position = "bottom", legend.text.size = 10,
                           manual.frame.coordinates = NULL,
                           automated.frame.margins = list ("latitude.min" = 0, "latitude.max" = 0, "longitude.min" = 0, "longitude.max" = 0),
                           aspect.ratio.min = 0.75, aspect.ratio.max = 1.25,
                           map.name = NULL,
                           title.text = NULL, title.size = 20, title.color = "black", title.face = NULL,
                           caption.text = NULL, caption.size = 9, caption.color = "black", caption.face = NULL,
                           output.directory = NULL, save.plot = TRUE, save.data = TRUE)  {

  # Cache should have been initialised
  stopifnot (eneRgycache::cache.initialised())

  # check that data was provided
  stopifnot ((! is.null(border.data)) || (! is.null(fill.data)))

  # Only country, control area and bidding zone maps are currently supported
  stopifnot (eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.name))
  stopifnot ((is.null (geographic.entity.to.label)) || (eneRgymaps::is.relevant.geographic.entity.name(geographic.entity.to.label)))

  # Should only display geographic entity labels when no other fill label is set
  stopifnot ((is.null (geographic.entity.to.label)) || (length (c (fill.label.first.field.name, fill.label.second.field.name)) == 0))

  # Should only filter geographic.entity.to.label.political.initiatives when geographic.entity.to.label is set
  stopifnot ((is.null (geographic.entity.to.label.political.initiatives)) || (! is.null (geographic.entity.to.label)))

  # should only save if output directory is not NULL
  stopifnot ((! is.null(output.directory)) || ((! save.plot) & (! save.data)))

  # variable names should be well formatted (and should not include forbidden characters)
  variableUsedNames <- c(fill.id.field.name, fill.value.field.name, fill.label.first.field.name, fill.label.second.field.name,
                     border.id.field.name, border.in.id.field.name, border.out.id.field.name, border.width.value.field.name,
                     border.color.value.field.name, border.label.value.field.name) %>% stats::na.omit() %>% unique()
  stopifnot(coherent.map.variable.names.formatting (variable.names = variableUsedNames))

  # legend.position should be relevant
  stopifnot (eneRgymaps::is.relevant.legend.position (legend.position))

  # set the map name to use, when saving the map
  mapName <- ''
  if (! is.null (map.name)) {
    mapName <- map.name
  } else {
    mapNames <- c(fill.legend.name, border.legend.width.name, border.legend.color.name, 'map') %>%
      stats::na.omit() %>% unique()
    mapName <- paste (mapNames, collapse = ' ')
  }

  mapPath <- file.path (output.directory, mapName)

  manualFrameCoordinates <- ! is.null (manual.frame.coordinates)
  automatedFrameCoordinates <- ! manualFrameCoordinates

  # the zoom should be set by at exactly one type of coordinates
  stopifnot (sum ( c(manualFrameCoordinates, automatedFrameCoordinates), na.rm = TRUE) == 1)

  # the date should follow the right format
  stopifnot(class(date) == "Date")

  # Create a plot with the world as background
  geographicMap <- eneRgymaps::world.background (fill.color = fill.na.color, border.color = fill.borders.color, only.keep.Europe = TRUE, iso2s.to.ignore = background.iso2s.to.ignore)
  # Fill geographic entities
  mapFillObj <- mapFill (geographicMap = geographicMap, data = fill.data, geographic.entity.name = geographic.entity.name,
                         date = date,
                         id.field.name = fill.id.field.name, value.field.name = fill.value.field.name,
                         label.first.field.name = fill.label.first.field.name, label.second.field.name = fill.label.second.field.name,
                         label.font.color = fill.label.font.color, label.font.size = fill.label.font.size, label.rounding.ndigits = fill.label.rounding.ndigits, label.with.frame = fill.label.with.frame,
                         legend.name = fill.legend.name, legend.values.as.percentages = fill.legend.values.as.percentages,
                         legend.keys = fill.legend.keys, legend.values = fill.legend.values,
                         color.palette = fill.color.palette, na.color = fill.na.color,
                         color.gradient.colors = fill.color.gradient.colors, color.gradient.values = fill.color.gradient.values, color.gradient.perc.values = fill.color.gradient.perc.values,
                         colors.list = fill.colors.list,
                         color.min.value = fill.color.min.value, color.max.value = fill.color.max.value,
                         borders.color = fill.borders.color, borders.width = fill.borders.width, borders.linetype = fill.borders.linetype,
                         center.shape.value.field.name = fill.center.shape.value.field.name,
                         center.shape.color.field.name = fill.center.shape.color.field.name, center.shape.width.field.name = fill.center.shape.width.field.name,
                         center.shape.name = fill.center.shape.name, center.shape.color = fill.center.shape.color,
                         center.shape.width = fill.center.shape.width, center.shape.solid = fill.center.shape.solid,
                         legend.center.shape.name = fill.legend.center.shape.name, legend.center.shape.keys = fill.legend.center.shape.keys, legend.center.shape.values = fill.legend.center.shape.values,
                         legend.center.shape.width.name = fill.legend.center.shape.width.name,
                         legend.center.shape.color.name = fill.legend.center.shape.color.name, legend.center.shape.color.values.as.percentages = fill.legend.center.shape.color.values.as.percentages,
                         legend.center.shape.keep.na = fill.legend.center.shape.keep.na,
                         legend.center.shape.color.keys = fill.legend.center.shape.color.keys, legend.center.shape.color.values = fill.legend.center.shape.color.values,
                         center.shape.color.palette = fill.center.shape.color.palette,
                         center.shape.color.gradient.colors = fill.center.shape.color.gradient.colors, center.shape.color.gradient.values = fill.center.shape.color.gradient.values, center.shape.color.gradient.perc.values = fill.center.shape.color.gradient.perc.values,
                         center.shape.color.min.value = fill.center.shape.color.min.value, center.shape.color.max.value = fill.center.shape.color.max.value,
                         center.shape.colors.list = fill.center.shape.colors.list,
                         center.shape.width.min.value = fill.center.shape.width.min.value, center.shape.width.max.value = fill.center.shape.width.max.value,
                         data.path = paste(mapPath, "fill", sep=" "), save.data = save.data)
  # Describe borders
  mapFillBorderObj <- mapBorder (geographicMap = mapFillObj[["map"]], data = border.data, geographic.entity.name = geographic.entity.name,
                                 id.field.name = border.id.field.name, in.id.field.name = border.in.id.field.name,
                                 out.id.field.name = border.out.id.field.name,
                                 width.value.field.name = border.width.value.field.name, shape.value.field.name = border.shape.value.field.name,
                                 color.value.field.name = border.color.value.field.name, label.value.field.name = border.label.value.field.name,
                                 label.font.color = border.label.font.color, label.rounding.ndigits = border.label.rounding.ndigits, label.with.frame = border.label.with.frame,
                                 legend.shape.name = border.legend.shape.name, legend.width.name = border.legend.width.name,
                                 legend.color.name = border.legend.color.name, legend.color.values.as.percentages = border.legend.color.values.as.percentages,
                                 legend.color.keys = border.legend.color.keys, legend.color.values = border.legend.color.values,
                                 legend.shape.keys = border.legend.shape.keys, legend.shape.values = border.legend.shape.values,
                                 legend.keep.na = border.legend.keep.na,
                                 flip.arrows.negative.values = border.flip.arrows.negative.values,
                                 shape.name = border.shape.name, shape.width = border.shape.width, shape.solid = border.shape.solid,
                                 shape.color = border.shape.color, color.palette = border.color.palette,
                                 color.gradient.colors = border.color.gradient.colors,
                                 color.gradient.values = border.color.gradient.values, color.gradient.perc.values = border.color.gradient.perc.values,
                                 colors.list = border.colors.list, na.color = border.na.color,
                                 color.min.value = border.color.min.value, color.max.value = border.color.max.value,
                                 width.min.value = border.width.min.value, width.max.value = border.width.max.value,
                                 data.path = paste(mapPath, "border", sep=" "), save.data = save.data)

  geographicMap <- mapFillBorderObj [["map"]]

  # Add geographic entity labels when relevant
  if (! is.null (geographic.entity.to.label)) {
    geographicMap <- geographicMap %>%
      eneRgymaps::add.geographic.entity.names (geographic.entity.name = geographic.entity.to.label, political.initiatives = geographic.entity.to.label.political.initiatives,
                                               date = date, font.color = geographic.entity.label.font.color)
  }

  # Set the frame coordinates
  frameCoordinates <- NULL
  if (manualFrameCoordinates) {
    stopifnot (eneRgymaps::geographic.interval.well.defined (coordinates = manual.frame.coordinates, check.bounds.consistency = TRUE))
    frameCoordinates <- manual.frame.coordinates
  } else { # automated frame coordinates
    stopifnot ("frame.coordinates" %in% names (mapFillObj))
    stopifnot ("frame.coordinates" %in% names (mapFillBorderObj))
    stopifnot (eneRgymaps::geographic.interval.well.defined (coordinates = automated.frame.margins, check.bounds.consistency = TRUE))

    automatedFrameCoordinatesList <- list()
    for (fieldName in c ("longitude.min", "latitude.min")) {
      automatedFrameCoordinatesList [[fieldName]] <- min (mapFillObj [["frame.coordinates"]][[fieldName]],
                                                          mapFillBorderObj [["frame.coordinates"]][[fieldName]], na.rm = TRUE) -
                                                      automated.frame.margins [[fieldName]]
    }
    for (fieldName in c ("longitude.max", "latitude.max")) {
      automatedFrameCoordinatesList [[fieldName]] <- max (mapFillObj [["frame.coordinates"]][[fieldName]],
                                                          mapFillBorderObj [["frame.coordinates"]][[fieldName]], na.rm = TRUE) +
                                                      automated.frame.margins [[fieldName]]
    }

    frameCoordinates <- eneRgymaps::fit.geographic.interval.to.aspect.ratio(coordinates = automatedFrameCoordinatesList,
                                                                            aspect.ratio.min = aspect.ratio.min, aspect.ratio.max = aspect.ratio.max)
  }
  geographicMap <- geographicMap %>%
    set.frame.coordinates (longitude.min = frameCoordinates [["longitude.min"]], longitude.max = frameCoordinates [["longitude.max"]],
                           latitude.min = frameCoordinates [["latitude.min"]], latitude.max = frameCoordinates [["latitude.max"]])

  # add title/caption when relevant
  if (length (c (title.text, caption.text)) > 0) {
    geographicMap <- geographicMap %>%
      add.title.caption (title.text = title.text, title.size = title.size, title.color = title.color, title.face = title.face,
                         caption.text = caption.text, caption.size = caption.size, caption.color = caption.color, caption.face = caption.face)
  }

  # Customize the plot settings
  themePlot <- list(ggplot2::theme(legend.text = ggplot2::element_text(size = legend.text.size),
                                   legend.position = legend.position))

  geographicMap <- geographicMap +
    themePlot

  if (save.plot) {
    eneRgymaps::save.plot (plot = geographicMap, destinationpath = mapPath)
  }

  return(geographicMap)
}
