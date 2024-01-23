# Components used to build geographic maps

#' Display a world background (with country borders)
#' @param only.keep.Europe whether to only keep shapes describing countries close to Europe (or all countries included in the shape file)
#' @param fill.color the color to use to fill the background
#' @param border.color the color to use for borders between background countries (NULL = borders not depicted)
#' @param iso2s.to.ignore a vector of country ISO2 ids to ignore when depicting the background, i.e. for which not to display geographic entities
#' @return a ggplot map
#' @importFrom ggplot2 aes ggplot theme
#' @examples
#' eneRgymaps::world.background (only.keep.Europe = FALSE, fill.color = "grey", border.color = "white")
#' eneRgymaps::world.background (only.keep.Europe = TRUE, fill.color = "green", border.color = "white", iso2s.to.ignore = c("DE", "PL"))
#' @export
world.background <- function (only.keep.Europe, fill.color, border.color, iso2s.to.ignore = NULL) {
  stopifnot (eneRgymaps::valid.colors (colors = c(fill.color, border.color), silent = FALSE))

  World <- eneRgymaps::geographic.shapes (geographic.entity.name = "Country", only.keep.Europe = only.keep.Europe) %>%
    dplyr::select(- `continent`)

  # check whether any ISO2 to ignore is missing
  missingISO2s <- setdiff(iso2s.to.ignore, intersect(unique(World[["iso2"]]), iso2s.to.ignore))
  if (length(missingISO2s) > 0) {
    message(paste("The following ISO2 to be ignored are not part of the initial data set: ", paste(missingISO2s, collapse = ", ")))
  }

  World <- World %>%
    # filter out the non-country shapes used for convenience, and the countries to ignore
    dplyr::filter(! `iso2` %in% c("GB", "NI", "IE (SEM)", "DELU", iso2s.to.ignore))

  geographicMap <- eneRgymaps::add.fill.and.border (geographic.map = ggplot2::ggplot(), data = World,
                                         fill.color = fill.color,
                                         border.color = border.color)

  # Customize the plot settings
  themePlot <- list(ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
                                   panel.grid.major = ggplot2::element_blank(),
                                   panel.background = ggplot2::element_blank(),
                                   aspect.ratio = 1.0,
                                   axis.line = ggplot2::element_blank(),
                                   axis.text.x = ggplot2::element_blank(),
                                   axis.text.y = ggplot2::element_blank(),
                                   axis.ticks = ggplot2::element_blank(),
                                   axis.title.x = ggplot2::element_blank(),
                                   axis.title.y = ggplot2::element_blank()))

  geographicMap <- geographicMap +
    themePlot

  return (geographicMap)
}

#' Display names of geographic entities on top of a geographic map
#'
#' @param plot the plot on top of which to add the names of the geographic entities
#' @param geographic.entity.name name of the geographic entities for which to display labels ("Bidding zone", "Control area", "Country"...)
#' @param political.initiatives a vector of political initiatives to filter (either NULL or "EU", "EC" - Energy Commnuity)
#' @inheritParams geographic.shapes date
#' @param items.to.keep a list of item IDs to keep
#' @param items.to.remove a list of items IDs to remove
#' @param font.color the color of the font
#' @param font.size the size of the font
#' @return the labels plotted as a ggplot2 plot
#' @importFrom dplyr filter
#' @examples
#' add.geographic.entity.names (myPlot, "Country")
#' add.geographic.entity.names (plot = myPlot, geographic.entity.name = "Country", political.initiatives = c ("EFTA", "EU"))
#' add.geographic.entity.names (plot = myPlot, geographic.entity.name = "Country", political.initiatives = c ("EC", "EFTA", "EU"), font.color = "blue")
#' @export
add.geographic.entity.names <- function (plot, geographic.entity.name, political.initiatives = NULL,
                                         date = base::Sys.Date(),
                                         items.to.keep = NULL, items.to.remove = NULL,
                                         font.color = "black", font.size = 2) {
  # if size = 0, display nothing
  if (font.size == 0) {
    return (plot)
  }

  # items.to.keep and items.to.remove cannot be set simultaneously
  stopifnot ((is.null (items.to.keep)) || (is.null (items.to.remove)))

  # remove NA labels
  # if items.to.keep is set, only keep these items
  # if items.to.remove is set, only keep the items which are not part of this list
  entityLabels <- eneRgymaps::geographic.entity.labels (geographic.entity.name = geographic.entity.name, political.initiatives = political.initiatives) %>%
    dplyr::filter (! is.na (`label`))

  if (! is.null (items.to.keep)) {
    entityLabels <- entityLabels %>%
      dplyr::filter (`id` %in% items.to.keep)
  } else if (! is.null (items.to.keep)) {
    entityLabels <- entityLabels %>%
      dplyr::filter (! `id` %in% items.to.remove)
  }

  # for countries, remove the labels of NI and GB, as these are not countries
  # (but mapCountry allows to plot information for these geographic entities)
  if (geographic.entity.name == "Country") {
    entityLabels <- entityLabels %>%
      dplyr::filter (! `label` %in% c ("GB", "NI"))
  }

  geographicData <- eneRgymaps::geographic.shapes (geographic.entity.name = geographic.entity.name, date = date,
                                                   only.keep.Europe = TRUE, with.centroids = TRUE)
  idColumnName <- NULL
  if (geographic.entity.name == "Country") {
    idColumnName <- "iso2"
  } else if (geographic.entity.name %in% c("Bidding zone", "Control area")) {
    idColumnName <- "eic_code"
  }
  geometryColumnsToKeep <- c(idColumnName, "centroid_longitude", "centroid_latitude")

  # merge the labels with the geographic data
  dataForPlot <- eneRgymaps::enrich.with.geographic.coordinates(data = entityLabels, data.id.field.name = "id",
                                                                 geographic.coordinates.data = geographicData, geographic.coordinates.data.id.field.name = idColumnName,
                                                                 geographic.coordinates.data.field.names = geometryColumnsToKeep)

  return (eneRgymaps::add.labels(geographic.map = plot, data = dataForPlot,
                                 coordinates.identifiers = c("centroid_longitude", "centroid_latitude"),
                                 field.names = c ("label"),
                                 font.color = font.color, font.size = font.size, with.frame = TRUE))
}


#' Configure the colors of a graph
#'
#' @param geographic.map the geographic map to update
#' @param data the data set from which to extract the data (as a data.frame)
#' @param configure.type the type of scales to use ("color" or "fill")
#' @param field.name the field to use to define colors
#' @param color.palette the palette to use to display the colors
#' @param na.color the color to use for NA values
#' @param gradient.colors a vector of colors defining a color gradient
#' @param gradient.values threshold values to associate to each gradient color
#' @param gradient.perc.values threshold values (as percentages between min and max) to associate to each gradient color
#' @param colors.list a list/vector of colors to use (only with discrete variables)
#' @param min.value the value to associate with the minimum color (anything below will be treated as minimum color). Only applies to a palette or a gradient.
#' @param max.value the value to associate with the maximum color (anything above will be treated as maximum color). Only applies to a palette or a gradient.
#' @param legend.name the name of the legend related to colors
#' @param legend.keys a vector of keys used to customize the legend related to colors
#' @param legend.values a vector of values to customize the legend related to colors
#' @param legend.keep.na whether to keep NAs in the legend
#' @return the updated map
#' @importFrom ggplot2 guide_colorbar guide_legend scale_color_distiller scale_color_gradientn scale_color_manual scale_fill_distiller scale_fill_gradientn scale_fill_manual
#' @importFrom scales squish
#' @importFrom stats na.omit
#' @examples
#' configure.colors (geographic.map = myMap, data = BZPriceChanges, configure.type = "fill", field.name = "price", color.palette = "Blues")
#' @export
configure.colors <- function (geographic.map, data = NULL, configure.type,
                        field.name = NULL,
                        color.palette = NULL, na.color = "lightgrey",
                        gradient.colors = NULL, gradient.values = NULL, gradient.perc.values = NULL,
                        colors.list = NULL, min.value = NULL, max.value = NULL,
                        legend.name = NULL, legend.keys = NULL, legend.values = NULL,
                        legend.keep.na = NULL) {

  # empty data set => nothing to do
  # no color field defined => nothing to do
  if ((length (data) == 0) || (is.null (field.name))) {
    return (geographic.map)
  }

  # color field should be well defined
  stopifnot (field.name %in% names(data))

  # configure type should be relevant
  stopifnot (configure.type %in% c ("color", "fill"))

  # gradient, list and na colors should be relevant
  stopifnot (eneRgymaps::valid.colors(colors = unique(c(gradient.colors, colors.list, na.color)), silent = FALSE))

  # define an object of length one related to a colors list, in order to be able to easily test whether exactly one color set is defined
  # adding a list (of length 2) to another (empty) list does not lead to a list of length 1, but to a list of length 2...
  colorsListOneLength <- NULL
  if (! is.null(colors.list)) {
    colorsListOneLength <- "temp"
  }
  colorsGradientOneLength <- NULL
  if (! is.null (gradient.colors)) {
    colorsGradientOneLength <- "temp"
  }

  # One color option setting should be set
  # (if no color option is set, it should mean that no field.name is set either)
  stopifnot(length(c(color.palette, colorsGradientOneLength, colorsListOneLength)) == 1)

  # colors related to a color gradient should either be empty, or larger than 2
  stopifnot(length(c(gradient.colors)) != 1)

  # minimum and maximum color values should only be set together
  stopifnot (length(c(min.value, max.value)) %in% c (0, 2))

  # the value associated to the minimum color should be lower than the value associated with the maximum color
  if ((! is.null (min.value)) && (all (! is.na (c(min.value, max.value))))) {
    stopifnot (min.value <= max.value)
  }

  # minimum and maximum color values should only be set when either gradient or palette is set
  stopifnot ((! is.null (color.palette)) | (! is.null (gradient.colors)) | (is.null (min.value)))

  # Customized color legend should only be set along with a colors list
  stopifnot ((is.null (legend.keys)) | (! is.null (colors.list)))

  stopifnot (length (legend.keys) == length (legend.values))
  if (is.null (legend.name) && (length (c(legend.keys, legend.values)) > 0)) {
    warning("Legend keys and values will only be displayed if the legend name is not NULL (legend.name='' means displaying the legend without a legend title)")
  }

  if ((configure.type == "fill") && (! is.null(legend.keep.na))) {
    warning("Keeping NAs in the legend does not apply apply for fill")
  }


  # when a colors.list is provided, check that
  # - the value.field.name is set
  # - the right number of colors were provided
  if (! is.null(colors.list)) {
    stopifnot (length(colors.list) >= 1)
    stopifnot (length(colors.list) >= length(unique(stats::na.omit(data[[field.name]]))))
  }

  # when the values to map with palette are all string,
  # then the number of distinct values should be less than the palette size
  # (when numbers are used, ggplot automatically rescales the palette accordingly)
  if ((! is.null (color.palette)) & (! all (is.numeric(stats::na.omit(data[[field.name]]))))) {
      stopifnot (length(unique(stats::na.omit(data[[field.name]]))) <=
                   eneRgymaps::palette.length(palette = color.palette))
  }

  # color gradient values should only be set along with gradient colors
  stopifnot ((! is.null (gradient.colors)) || ((is.null (gradient.values)) & (is.null (gradient.perc.values))))
  stopifnot ((is.null (gradient.values)) || (length (gradient.values) == length (gradient.colors)))
  stopifnot ((is.null (gradient.perc.values)) | (length (gradient.perc.values) == length (gradient.colors)))
  # either gradient.values or gradient.perc.values may be set, but not both
  stopifnot ((is.null (gradient.values)) || (is.null (gradient.perc.values)))
  gradientPercValues <- NULL
  if (! is.null (gradient.colors)) {
    if (! is.null (gradient.values)) {
      minValue <- max (min.value, min (data [[field.name]], na.rm = TRUE), na.rm = TRUE)
      maxValue <- min (max.value, max (data [[field.name]], na.rm = TRUE), na.rm = TRUE)
      stopifnot (maxValue > minValue)
      gradientPercValues <- sapply (X = gradient.values, FUN = function (x) return (((x - minValue) / (maxValue - minValue))))
    } else if (! is.null (gradient.perc.values)) {
      gradientPercValues <- gradient.perc.values
    }

    if (! is.null (gradientPercValues)) {
      if ((min (gradientPercValues) > 0) | (max (gradientPercValues) < 1)) {
        warning("The color gradient does not fully cover the range of values : some border shapes will be grey")
      }
    }
  }

  updatedMap <- geographic.map

  colorGuide <- "none" # derive the right legend based on arguments
  limitValues <- c (min.value, max.value) # set the color limits (when relevant)

  genericParameters <- c (list(name = legend.name, na.value = na.color))
  if (configure.type == "color") {
    genericParameters <- c(genericParameters, list(na.translate = legend.keep.na))
  }
  genericParameters <- genericParameters[! sapply(genericParameters, is.null)]

  if (! is.null (gradient.colors)) {
    if ( ! is.null (legend.name)) {
      colorGuide <- ggplot2::guide_colorbar (title = legend.name, title.vjust = 0.85)
    }

    gradientParameters <- c (genericParameters, list (colours = gradient.colors, values = gradientPercValues,
                                  guide = colorGuide, limits = limitValues, oob = scales::squish)) # add everything, then remove NULL elements
    gradientParameters <- gradientParameters[! sapply(gradientParameters, is.null)]

    gradientFunction <- paste0 ("ggplot2::scale_", configure.type, "_gradientn")

    updatedMap <- updatedMap +
      do.call (what = eval (parse (text = gradientFunction)), args = gradientParameters)# eval(parse(x)) needed because otherwise the function is not properly retrieved
  } else if (! is.null(color.palette)) {
    if ( ! is.null (legend.name)) {
      colorGuide <- ggplot2::guide_legend (title = legend.name)
    }
    distillerParameters <- c (genericParameters, list(palette = color.palette, direction = 1,
                                   guide = colorGuide, limits = limitValues, oob = scales::squish))
    distillerParameters <- distillerParameters[! sapply(distillerParameters, is.null)]
    distillerFunction <- paste0 ("ggplot2::scale_", configure.type, "_distiller")
    updatedMap <- updatedMap +
      do.call (what = eval (parse (text = distillerFunction)), args = distillerParameters)
  } else if (! is.null(colors.list)) {
    if (! is.null (legend.name)) {
      colorGuide <- ggplot2::guide_legend (title = legend.name)
    }

    # setting customised legend names when relevant
    orderedValuesNames <- ordered.legend.names (keys.used = data[[field.name]],
                                                legend.keys = legend.keys, legend.values = legend.values)

    manualParameters <- c (genericParameters, list (values = colors.list, labels = orderedValuesNames, guide = colorGuide)) # add everything, then remove NULL elements
    manualParameters <- manualParameters[! sapply(manualParameters, is.null)]
    manualFunction <- paste0 ("ggplot2::scale_", configure.type, "_manual")
    updatedMap <- updatedMap +
      do.call (what = eval (parse (text = manualFunction)), args = manualParameters)

  } else {
    stop ("Customising colors requires colors definition")
  }

  return (updatedMap)
}




#' Add shapes to a graph
#'
#' @param geographic.map the geographic map to update
#' @param data the data set from which to extract the data
#' @param coordinates.identifiers a vector of shape coordinates identifiers (longitude, latitude for center points, entry/exit points of interconnectors). NA means none
#' @param shape.field.name the field name to use to set the shape
#' @param shape the (fixed) name of shapes to use
#' @param legend.shape.name the name of the legend related to the shapes (NULL -> not displayed)
#' @param legend.shape.keys the keys to use to customise the shapes legend
#' @param legend.shape.values the values to use to customise the shapes legend
#' @param width.field.name the field to use to define shape widths
#' @param width the (fixed) shape width
#' @param width.min.value the value to associate with the minimum width (anything below will be treated as minimum width)
#' @param width.max.value the value to associate with the maximum width (anything above will be treated as maximum width)
#' @param legend.width.name the name of the legend related to the widths (NULL -> not displayed)
#' @param color.field.name the field to use to define shape colours
#' @param color the (fixed) shape color
#' @param solid whether to fill the shape (or to only keep the shape border)
#' @param color.palette the palette to use to display the colors
#' @param na.color the color to use for objects with NA
#' @param color.gradient.colors a vector of colors defining a color gradient
#' @param color.gradient.values threshold values to associate to each gradient color
#' @param color.gradient.perc.values threshold values (as percentages between min and max) to associate to each gradient color
#' @param colors.list a list/vector of colors to use (only with discrete variables)
#' @param color.min.value the value to associate with the minimum color (anything below will be treated as minimum color). Only applies to a palette or a gradient.
#' @param color.max.value the value to associate with the maximum color (anything above will be treated as maximum color). Only applies to a palette or a gradient.
#' @param legend.color.name the name of the legend related to the shape color
#' @param legend.color.keys a list of keys used to customize the legend related to colors
#' @param legend.color.values a list of values to customize the legend related to colors
#' @param legend.keep.na whether the shape and/or color legend should keep NAs
#' @return the updated map
#' @importFrom dplyr select
#' @importFrom ggplot2 aes_string arrow geom_point geom_segment guide_legend scale_shape_discrete scale_size_continuous unit
#' @importFrom scales squish
#' @importFrom stats na.omit
#' @examples
#' add.shapes
#' @export
add.shapes <- function (geographic.map, data = NULL,
                        coordinates.identifiers = NULL,

                        shape.field.name = NULL, shape = NULL,
                        legend.shape.name = NULL, legend.shape.keys = NULL, legend.shape.values = NULL,

                        width.field.name = NULL, width = NULL, width.min.value = NULL, width.max.value = NULL,
                        legend.width.name = NULL,

                        color.field.name = NULL, color = NULL, solid = TRUE,
                        color.palette = NULL, na.color = "lightgrey",
                        color.gradient.colors = NULL, color.gradient.values = NULL, color.gradient.perc.values = NULL,
                        colors.list = NULL, color.min.value = NULL, color.max.value = NULL,
                        legend.color.name = NULL,
                        legend.color.keys = NULL, legend.color.values = NULL,
                        legend.keep.na = NULL) {

  # empty data set => nothing to do
  if (is.null (data)) {
    return (geographic.map)
  }

  # no shape defined => nothing to do
  if (length ( c(shape.field.name, shape)) == 0) {
    if (length ( c(width.field.name, width, color.field.name, color)) > 0) {
      warning ("The shape width and color will be ignored because neither a shape field name nor or (fixed) shape name was defined")
    }
    return (geographic.map)
  }

  # coordinates should be well defined
  stopifnot (all (coordinates.identifiers [!is.na(coordinates.identifiers)] %in% names(data), na.rm = TRUE))

  stopifnot (length ( c(shape.field.name, shape)) == 1)
  stopifnot (length ( c(width.field.name, width)) <= 1) # if no width is set, the default width is used
  stopifnot (length ( c(color.field.name, color)) == 1)

  # legend names should only be set when the related value field is set
  stopifnot (! ((is.null(width.field.name)) && (! is.null(legend.width.name))))
  stopifnot (! ((is.null(color.field.name)) && (! is.null(legend.color.name))))
  stopifnot (! ((is.null(shape.field.name)) && (! is.null(legend.shape.name))))

  # 6 coordinates are needed : long/lat for center, in and out
  stopifnot (length (coordinates.identifiers) == 6)

  # Shape, width and color field names should be in the data set
  stopifnot (all (c(width.field.name, color.field.name, shape.field.name) %in% names(data)))

  # Shape field name should be described by relevant shape identifiers
  # When set, shape name should be among predefined values
  stopifnot ((is.null(shape.field.name)) | (shape.field.name %in% names(data)))
  if (! is.null (shape.field.name)) {
    stopifnot (all (sapply (X = unique (data [[shape.field.name]] %>% stats::na.omit()),
                            FUN = eneRgymaps::is.relevant.shape.identifier )))

    # need to convert the data to integers to extract the values,
    # then to convert it to factors otherwise ggplot2 fails...
    if (! any (class (data [[shape.field.name]]) == "integer")) {
      data [[shape.field.name]] <- as.integer (data [[shape.field.name]])
    }
    data [[shape.field.name]] <- factor (data [[shape.field.name]], ordered = TRUE) # needed because otherwise ggplot2 considers integers as continuous variables and fails...
  } else if (! is.null (shape)) {
    stopifnot (eneRgymaps::is.relevant.shape.identifier (shape))
  }

  shapeArrow <- FALSE
  shapeLine <- FALSE
  fixedShape <- FALSE
  if (! is.null (shape)) {
    shapeArrow <- shape == "arrow"
    shapeLine <- shape == "line"
    fixedShape <- TRUE
  }
  customShape <- ! is.null (shape.field.name)
  nbGeogPoints <- 0
  if (shapeArrow | shapeLine) {
    nbGeogPoints <- 2
  } else {
    nbGeogPoints <- 1
  }
  shapeSetByIntegers <- ! (shapeArrow | shapeLine)

  # only allow to customise the shape legend when shapes are custom
  stopifnot (customShape || (is.null (legend.shape.keys)))

  # shape width should be numeric
  stopifnot (is.null (width) || is.numeric (width))
  stopifnot (is.null (width) || (width > 0))

  # min/max width values should be set together
  widthLimitValues <- c (width.min.value, width.max.value)
  stopifnot (length (widthLimitValues) %in% c (0, 2))
  widthFieldName <- width.field.name
  if (length (c(widthLimitValues)) > 0) {
    # the min/max width values should only be set along with a width value field name
    stopifnot (! is.null (width.field.name))

    # the value associated with the minimum width should be lower than the value associated with the maximum width
    if (all (! is.na (widthLimitValues))) {
      stopifnot (width.min.value <= width.max.value)
    }

    # squish the values for width between the boundaries (oob is unfortunately unavailable for ggplot2::scale_size_continuous)
    widthFieldName <- paste0 (width.field.name, "_for_width_plot")
    stopifnot (! widthFieldName %in% names(data))
    data [[widthFieldName]] <- scales::squish (x = data [[width.field.name]], range = widthLimitValues)
  }

  orderedValuesNames <- NULL
  if ((! is.null (data)) && (! is.null (shape.field.name))) {
    orderedValuesNames <- ordered.legend.names (keys.used = data[[shape.field.name]],
                                                legend.keys = legend.shape.keys, legend.values = legend.shape.values)
  }

  # only keep usefull data from the dataset
  usefulFields <- c (shape.field.name, width.field.name, color.field.name, coordinates.identifiers, widthFieldName) %>% unique()
  usefulFields <- usefulFields [! is.na(usefulFields)]
  plotData <- data %>%
    dplyr::select (dplyr::all_of(usefulFields))

  # Update the map
  updatedMap <- geographic.map

  # The color setting will either be set through aes (palette, gradient...) or geom_segment (fixed color)
  aesColor <- NULL
  segmentColor <- NULL
  if (! is.null (color.field.name)) {
    aesColor <- color.field.name
  } else if (! is.null(color)) {
    segmentColor <- color
  }

  # one (and exactly one) object should not be NULL
  # (it is not posisble to test this aspect through length, because colors.list may have a longer length)
  stopifnot (is.null(aesColor) != is.null(segmentColor))

  # set the variable name for setting the arrow width (NULL means uniform width)
  aesSize <- NULL
  fixedSize <- NULL
  if (! is.null(width.field.name)) {
    aesSize <- widthFieldName
  } else if (! is.null (width)) {
    fixedSize <- width
  }

  # at most one way of setting size should be set (if no width is set, then the default size is used)
  stopifnot (length (c (aesSize, fixedSize)) <= 1)

  # generate the aes attributes
  shapeAesParams <- c (list (color = aesColor, size = aesSize, shape = shape.field.name)) # add everything then remove NULL elements
  if (nbGeogPoints == 1) {
    # centre point coordinates shall be defined
    stopifnot (! any (is.na (c (coordinates.identifiers[[1]], coordinates.identifiers[[2]]))))
    shapeAesParams <- c (shapeAesParams, list (x=coordinates.identifiers[[1]], y=coordinates.identifiers[[2]]))
  } else if (nbGeogPoints == 2) {
    # begin/end point coordinates shall be defined
    stopifnot (! any (is.na (c (coordinates.identifiers[[3]], coordinates.identifiers[[4]],
                                coordinates.identifiers[[5]], coordinates.identifiers[[6]]))))
    shapeAesParams <- c (shapeAesParams, list (x=coordinates.identifiers[[3]], y=coordinates.identifiers[[4]],
                                               xend=coordinates.identifiers[[5]], yend=coordinates.identifiers[[6]]))
  }
  shapeAesParams <- shapeAesParams[! sapply(shapeAesParams, is.null)]

  # rely on do.call, to propagate named arguments
  shapeAes <- do.call (what = ggplot2::aes_string, args = shapeAesParams)

  # generate the (fixed) shape parameters
  shapeParameters <- c (list (col = segmentColor, size = fixedSize)) # add everything, then remove NULL elements
  if (shapeArrow) {
    # see https://stackoverflow.com/a/61383034 for examples about arrow parameters
    arrowObject <- NULL
    if (! is.null(fixedSize)){
      arrowObject <- ggplot2::arrow(length = ggplot2::unit(0.3 * fixedSize, "cm"))
    } else {
      arrowObject <- ggplot2::arrow()
    }
    shapeParameters <- c (shapeParameters, list (arrow = arrowObject, lineend = "round", linejoin = "mitre"))
  }
  if (shapeSetByIntegers) {
    shapeParameters <- c (shapeParameters, list (shape = shape))
  }
  shapeParameters <- shapeParameters[! sapply(shapeParameters, is.null)]

  shapeFunName <- ggplot2::geom_point
  if (nbGeogPoints == 2) {
    shapeFunName <- ggplot2::geom_segment
  }

  updatedMap <- updatedMap +
    do.call (what = shapeFunName, args = c (list (data = plotData, mapping = shapeAes), shapeParameters))

  if (shapeSetByIntegers) {
    shapeGuide <- "none"
    if (! is.null (legend.shape.name)) {
      shapeGuide <- ggplot2::guide_legend (title = legend.shape.name)
    }

    scaleShapeDiscreteParameters <- c (list (name = legend.shape.name, solid = solid, guide = shapeGuide,
                                             labels = orderedValuesNames, na.translate = legend.keep.na)) # add everything, then remove NULL elements
    scaleShapeDiscreteParameters <- scaleShapeDiscreteParameters[! sapply(scaleShapeDiscreteParameters, is.null)]

    updatedMap <- updatedMap +
      do.call (what = ggplot2::scale_shape_discrete, args = scaleShapeDiscreteParameters)
  }

  # customize the segment/arrow width when relevant
  if (! is.null(aesSize)) {
    arrowsSizeRange <- c (0.75, 3.5) # customize the range of allowed arrows' sizes to ease readability
    sizeGuide <- "none"
    if (! is.null(legend.width.name)) {
      sizeGuide <- ggplot2::guide_legend (title = legend.width.name)
    }

    # add all parameters, then remove NULL elements
    scaleSizeContinuousParameters <- c (list (name = legend.width.name, range = arrowsSizeRange, guide = sizeGuide,
                                              limits = widthLimitValues))
    scaleSizeContinuousParameters <- scaleSizeContinuousParameters[! sapply(scaleSizeContinuousParameters, is.null)]
    updatedMap <- updatedMap +
      do.call (what = ggplot2::scale_size_continuous, args = scaleSizeContinuousParameters)
  }

  # when a color field is set, customize the shapes colors accordingly
  if (! is.null (color.field.name)) {
    updatedMap <- updatedMap %>%
      eneRgymaps::configure.colors(data = plotData, configure.type = "color", field.name = color.field.name,
                                   color.palette = color.palette, na.color = na.color,
                                   gradient.colors = color.gradient.colors, gradient.values = color.gradient.values,
                                   gradient.perc.values = color.gradient.perc.values, colors.list = colors.list,
                                   min.value = color.min.value, max.value = color.max.value,
                                   legend.name = legend.color.name, legend.keep.na = legend.keep.na,
                                   legend.keys = legend.color.keys, legend.values = legend.color.values)
  }

  return (updatedMap)
}


#' Add lines to a graph
#' @inheritDotParams add.shapes -shape.field.name -shape -legend.shape.name -legend.shape.keys -legend.shape.values
#' @export
add.lines <- function (...) {
  # specify empty parameters to ensure that they are not set through ...
  return (add.shapes(shape = "line", shape.field.name = NULL,
                     legend.shape.name = NULL, legend.shape.keys = NULL, legend.shape.values = NULL, ...))
}

#' Add arrows to a graph
#' @inheritDotParams add.shapes -shape.field.name -shape -legend.shape.name -legend.shape.keys -legend.shape.values
#' @export
add.arrows <- function (...) {
  return (add.shapes(shape = "arrow", shape.field.name = NULL,
                     legend.shape.name = NULL, legend.shape.keys = NULL, legend.shape.values = NULL, ...))
}

#' Add labels to a geographic map
#'
#' @param geographic.map the geographic map to update
#' @param data the data set from which to extract the data
#' @param coordinates.identifiers a vector of coordinates identifiers (longitude then latitude)
#' @param field.names the field names to use to set the label
#' @param field.separators a vector of separators to use (one value = same separator everywhere)
#' @param rounding.ndigits the number of digits to use when rounding label values (only for numeric values). NULL = keep values as they are
#' @param with.frame whether to add a frame (with white background), or to directly depict the label on top of the map
#' @param font.color the font color to use when writing labels
#' @param font.size the font size to use
#' @return the updated map
#' @importFrom dplyr all_of select
#' @importFrom ggplot2 aes_string geom_label geom_text
#' @importFrom stringi stri_rand_strings
#' @examples
#' eneRgymaps::add.labels (geographic.map = myMap, data = BZPriceChangesWithCoordinates, coordinates.identifiers = c ("longitude", "latitude"), field.names = c ("price"), with.frame = FALSE)
#' @export
add.labels <- function (geographic.map, data = NULL,
                        coordinates.identifiers = NULL,
                        field.names = NULL, field.separators = c ("\n"),
                        rounding.ndigits = NULL,
                        with.frame,
                        font.color = "black", font.size = 2) {

  # empty data set => nothing to do
  # no field defined => nothing to do
  if ((length (data) == 0) || (is.null (field.names))) {
    return (geographic.map)
  }

  # 2 coordinates are needed : long/lat for center
  stopifnot (length (coordinates.identifiers) == 2)
  stopifnot (all (coordinates.identifiers %in% names(data)))

  stopifnot (all (field.names %in% names (data)))

  # warning in case the same label is depicted multiple times
  if (length (unique (field.names)) < length (field.names)) {
    warning("Asked to depict twice the same label field : the display will be duplicated")
  }

  # separators should either have one element, or as many elements as field names
  stopifnot (length (field.separators) %in% c (1, length (field.names) - 1))
  stopifnot(all(sapply(X = field.separators, FUN = is.character, simplify = TRUE)))

  # rounding digits should be an integer
  if (! is.null (rounding.ndigits)) {
    stopifnot (rounding.ndigits == as.integer (rounding.ndigits))
  }

  # color should be valid
  stopifnot (eneRgymaps::valid.colors (colors = font.color, silent = FALSE))

  # font size should be valid
  stopifnot (is.numeric(font.size) && (font.size >= 0))

  # only keep useful data from the dataset
  usefulFields <- unique( c(field.names, coordinates.identifiers))
  plotData <- data %>%
    dplyr::select (dplyr::all_of(usefulFields))

  # Round numerical columns only
  if (! is.null (rounding.ndigits)) {
    columnsToRound <- intersect(field.names, colnames(plotData) [which(sapply(plotData, is.numeric))])
    plotData[[columnsToRound]] <- lapply(plotData[[columnsToRound]], function (y) round(x = y, digits = rounding.ndigits))
  }

  # Generate the label by pasting the various columns
  labelFieldNameInit <- "text_for_label"
  labelFieldName <- labelFieldNameInit
  while (labelFieldName %in% colnames(plotData)) {
    labelFieldName <- paste0(labelFieldNameInit, "_", stringi::stri_rand_strings(n = 1, length = 5))
  }

  if (length (field.separators) == 1) {
    plotData <- data.table::as.data.table(plotData)
    plotData[, (labelFieldName) := do.call(paste, c(.SD, sep = field.separators[[1]])), .SDcols = field.names]
    plotData <- as.data.frame(plotData)
  } else {
    plotData <- data
    plotData[[labelFieldName]] <- plotData[[field.names[[1]]]]
	  for (sepIndex in 1:length(field.separators)) {
	    plotData[[labelFieldName]] <- paste(plotData[[labelFieldName]], plotData[[field.names[[sepIndex + 1]]]], sep = field.separators[[sepIndex]])
	  }
  }

  labelFunction <- NULL
  if (with.frame) {
    labelFunction <- ggplot2::geom_label
  } else {
    labelFunction <- ggplot2::geom_text
  }

  return (geographic.map +
    labelFunction (data = plotData, mapping = ggplot2::aes_string(x = coordinates.identifiers[[1]], y = coordinates.identifiers[[2]],
                                                                  label = labelFieldName),
                   size = font.size, color = font.color, show.legend = FALSE))
}


#' Add filling and borders to a graph
#'
#' @param geographic.map the geographic map to update
#' @param data the data set from which to extract the data
#' @param fill.color.field the field to use to define the fill
#' @param fill.color (fixed) fill color
#' @param fill.keep.NA whether to keep NAs (only applies with fill.field)
#' @param border.color (fixed) border color
#' @param border.width (fixed) border width
#' @param border.linetype (fixed) border linetype
#' @return the updated map
#' @importFrom ggplot2 aes_string geom_sf
#' @examples
#' add.fill.and.border (geographic.map = ggplot2::ggplot(), data = World, fill.color = "red")
#' add.fill.and.border (geographic.map = ggplot2::ggplot(), data = myData, fill.color.field = "price", border.color = "white", border.width = 0.5, border.linetype = 1)
#' @export
add.fill.and.border <- function (geographic.map, data = NULL,
                        fill.color.field = NULL, fill.color = NULL, fill.keep.NA = NULL,
                        border.color = NULL, border.width = NULL, border.linetype = NULL) {

  # empty data set => nothing to do
  if (is.null (data)) {
    return (geographic.map)
  }

  withFill <- TRUE
  withBorder <- TRUE
  if (length (c(fill.color.field, fill.color)) == 0) {
    withFill <- FALSE
  }

  if (is.null(border.color)) {
    withBorder <- FALSE
  } else {
    if (! is.null(border.width)) {
      stopifnot (is.numeric(border.width))
      stopifnot (border.width >= 0)
    }
  }

  if (! withFill) {
    return (geographic.map)
  }

  # only one way to define colors
  stopifnot (length (c(fill.color.field, fill.color)) == 1)

  # removing NAs only applies with fill.color.field
  stopifnot ((is.null (fill.keep.NA)) | (! is.null (fill.color.field)))

  fieldNames <- c (fill.color.field, "geometry")

  # only keep useful data from the dataset
  plotData <- data %>%
    dplyr::select (all_of(fieldNames))

  if (! is.null (fill.keep.NA)) {
    if (! fill.keep.NA) {
      plotData <- plotData[! is.na(fill.color.field)]
    }
  }

  # define mapping
  aesParams <- c (list (fill = fill.color.field, geometry = "geometry")) # add everything then remove NULL elements
  aesParams <- aesParams[! sapply(aesParams, is.null)]
  aes <- do.call (what = ggplot2::aes_string, args = aesParams)

  borderColorParam <- NA
  borderSizeParam <- NULL
  borderLineTypeParam <- NULL
  if (withBorder) {
    borderColorParam <- border.color
    borderLineTypeParam <- border.linetype
    borderSizeParam <- border.width
  }
  componentParams <- c (list (data = plotData, mapping = aes, fill = fill.color,
                              color = borderColorParam, size = borderSizeParam, linetype = borderLineTypeParam))
  componentParams <- componentParams[! sapply(componentParams, is.null)]

  # Update the map
  updatedMap <- geographic.map +
    do.call (what = ggplot2::geom_sf, args = componentParams)

  return (updatedMap)
}


#' Add title and/or caption to a plot
#'
#' @param geographic.map the geographic map to update
#' @param title.text the title of the map to display (NULL -> no title)
#' @param title.size the size of the title to use (0 -> no title)
#' @param title.color the color of the title to use
#' @param title.face a face to use for the title (e.g. "italic", or "bold")
#' @param caption.text the caption of the map to display (NULL -> no caption)
#' @param caption.size the size of the caption to use (0 -> no title)
#' @param caption.color the color of the caption to use
#' @param caption.face a face to use for the caption (e.g. "italic", or "bold")
#' @return the updated map
#' @importFrom ggplot2 element_text labs theme
#' @examples
#' add.title.caption (geographic.map = ggplot2::ggplot(), title = "My plot")
#' add.title.caption (geographic.map = ggplot2::ggplot(), title = "My plot", title.size = 15, title.color = "blue", title.face = "bold")
#' add.title.caption (geographic.map = ggplot2::ggplot(), caption = "Source : eneRgymaps", caption.size = 12, caption.face = "italic")
#' add.title.caption (geographic.map = ggplot2::ggplot(), title = "My plot", title.size = 15, title.color = "blue", caption = "Source : eneRgymaps")
#' @export
add.title.caption <- function (geographic.map, title.text = NULL, title.size = 20, title.color = "black", title.face = NULL,
                               caption.text = NULL, caption.size = 9, caption.color = "black", caption.face = NULL) {
  stopifnot (is.numeric(title.size) && (title.size >= 0))
  stopifnot (is.numeric(caption.size) && (caption.size >= 0))
  stopifnot (all (eneRgymaps::valid.colors (colors = c(title.color, caption.color), silent = FALSE)))

  allowedFaces <- c("plain", "italic", "bold", "bold.italic")
  if (length (c(title.face, caption.face)) > 0) {
    stopifnot (all (c(title.face, caption.face) %in% allowedFaces))
  }

  toDisplayText <- (! is.null (title.text)) && (title.size > 0)
  toDisplayCaption <- (! is.null (caption.text)) && (caption.size > 0)

  # neither title nor caption => nothing to do
  if ((! toDisplayText) && (! toDisplayCaption)) {
    return (geographic.map)
  }

  # generate the arguments for the texts and themes
  titleParams <- c (list (title = title.text, caption = caption.text)) # add everything, then remove NULL elements
  titleParams <- titleParams[! sapply(titleParams, is.null)]
  titleTheme <- c (list (size = title.size, colour = title.color, face = title.face))
  titleTheme <- titleTheme[! sapply(titleTheme, is.null)]
  captionTheme <- c (list (size = caption.size, colour = caption.color, face = caption.face))
  captionTheme <- captionTheme[! sapply(captionTheme, is.null)]

  # add the component to the initial map
  updatedMap <- geographic.map +
    do.call (what = ggplot2::labs, args = titleParams) +
    list(ggplot2::theme(plot.title = do.call (what = ggplot2::element_text, args = titleTheme),
                        plot.caption = do.call (what = ggplot2::element_text, args = captionTheme)))

  return (updatedMap)
}


#' Set the frame coordinates for a given map
#'
#' @param geographic.map the geographic map to update
#' @param longitude.min the minimum longitude
#' @param longitude.max the maximum longitude
#' @param latitude.min the minimum latitude
#' @param latitude.max the maximum latitude
#' @return the updated map
#' @importFrom ggplot2 coord_sf
#' @examples
#' set.frame.coordinates (geographic.map = myMap, longitude.min = -732956.7, longitude.max = 3130773, latitude.min = 3942687, latitude.max = 6788951)
#' @export
set.frame.coordinates  <- function (geographic.map, longitude.min, longitude.max,
                                    latitude.min, latitude.max) {
  stopifnot (length (c (longitude.min, longitude.max, latitude.min, latitude.max)) == 4)
  stopifnot (all (is.numeric (c (longitude.min, longitude.max, latitude.min, latitude.max))))
  stopifnot (longitude.min < longitude.max)
  stopifnot (latitude.min < latitude.max)
  return (geographic.map +
            ggplot2::coord_sf(xlim = c(longitude.min, longitude.max), ylim = c(latitude.min, latitude.max)))
}
