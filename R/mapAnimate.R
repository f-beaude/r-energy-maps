# Animate a given geographic map
# The animation generates a pre-defined number of frames, then displays them one after the other
# Other examples of animated geographic maps may be found at https://mikeyharper.uk/animated-plots-with-r/

#' Generate an animated geographic map
#' @param animation.variable.name the name of the variable to use to animate the map. Does not have to represent a time, can also be a factor or another numeric variable
#' @param animation.variable.order a list of ordered values describing the frames' order to follow (NULL -> standard order of values)
#' @param date.variable.name the variable name to use to define which shapefile to rely on (NULL means rely on data argument of static map)
#' @param animation.time.interval the duration of the display of each frame (in s)
#' @param with.animation.caption whether to display the animation variable in the caption
#' @param animation.width the width of the animation to generate
#' @param animation.height the height of the animation to generate
#' @param animation.format the format of the animation to generate ("gif" or "mp4")
#' @param output.path the path in which to save the generated animation (NULL means not saving the generated animation)
#' @return the animation as a magick object
#' @inheritParams mapGeographic geographic.entity.name fill.data border.data
#' @inheritDotParams mapGeographic -geographic.entity.name -fill.data -border.data -output.directory -save.plot -save.data
#' @importFrom grDevices dev.off
#' @importFrom magick image_animate image_graph image_write
#' @importFrom stringi stri_rand_strings
animate.mapGeographic <- function (geographic.entity.name, animation.variable.name, animation.variable.order = NULL,
                                   date.variable.name = NULL,
                                   animation.time.interval = 1, with.animation.caption = TRUE,
                                   animation.width = 1000, animation.height = 1000,
                                   animation.format = "gif", output.path = NULL,
                                   fill.data = NULL, border.data = NULL, ...) {
  stopifnot(eneRgymaps::animation.format.is.valid(animation.format))

  stopifnot (! is.null (animation.variable.name))
  stopifnot(all (is.numeric (c (animation.time.interval, animation.width, animation.height))) && all(c (animation.time.interval, animation.width, animation.height) > 0))

  # retrieve and check the static plot parameters
  staticPlotParams <- list (...)
  if ((("save.plot" %in% names (staticPlotParams)) && (staticPlotParams[["save.plot"]])) ||
      (("save.data" %in% names (staticPlotParams)) && (staticPlotParams[["save.data"]]))) {
    warning("Forbidden to save plot or data related to static plot while generating an animation")
    staticPlotParams [["save.plot"]] <- NULL
    staticPlotParams [["save.data"]] <- NULL
  }
  stopifnot ((! with.animation.caption) || (! "caption.text" %in% names (staticPlotParams)))

  if (! is.null(date.variable.name)) {
    # date.variable.name shall be in the data set
    stopifnot ((is.null(fill.data)) || (date.variable.name %in% names(fill.data)))
    # the date variable name should not be defined in the parameters of the static plot
    stopifnot(! "date" %in% names(staticPlotParams))
  }

  # name the ordering variable
  animationVariableOrderedNameInit <- paste0 (animation.variable.name, "_ordered_for_animation")
  animationVariableOrderedName <- animationVariableOrderedNameInit
  while ((animationVariableOrderedName %in% colnames(fill.data)) || (animationVariableOrderedName %in% colnames(border.data))) {
    animationVariableOrderedName <- paste0(animationVariableOrderedNameInit, "_", stringi::stri_rand_strings(n = 1, length = 5))
  }

  # prepare fill/border data for animation
  fillData <- fill.data %>%
    prepare.for.animation (variable.name = animation.variable.name, variable.order = animation.variable.order,
                           ordered.variable.name = animationVariableOrderedName)
  borderData <- border.data %>%
    prepare.for.animation (variable.name = animation.variable.name, variable.order = animation.variable.order,
                           ordered.variable.name = animationVariableOrderedName)

  # define which column should be used for the animation
  animationVariableNameToUse <- animation.variable.name
  if (! is.null (animation.variable.order)) {
    animationVariableNameToUse <- animationVariableOrderedName
  }

  # extract the animation variable values, to iterate upon
  animationVariableValues <- sorted.animation.variable.values (animation.variable.name = animationVariableNameToUse,
                                                               fill.data = fillData, border.data = borderData)

  if (length (animationVariableValues) == 0) {
    warning ("No animation variable values found => no animation generated")
    return ()
  }

  # start a frame, then generate each image of the animation, one by one
  frames <- magick::image_graph(width = animation.width, height = animation.height)
  for (currentAnimationValue in animationVariableValues) {
    fillDataCurrent <- fillData %>%
      filter.animation.data (animation.variable.name = animationVariableNameToUse,
                             animation.variable.value = currentAnimationValue)
    borderDataCurrent <- borderData %>%
      filter.animation.data (animation.variable.name = animationVariableNameToUse,
                             animation.variable.value = currentAnimationValue)

    currentStaticPlotParams <- c (staticPlotParams,
                                  list (geographic.entity.name = geographic.entity.name,
                                        fill.data = fillDataCurrent,
                                        border.data = borderDataCurrent,
                                        save.plot = FALSE, save.data = FALSE))
    if (with.animation.caption) {
      currentStaticPlotParams [["caption.text"]] <- currentAnimationValue
    }
    if (! is.null(date.variable.name)) {
      currentDate <- current.animation.date(date.variable.name = date.variable.name,
                                            current.fill.data = fillDataCurrent, current.border.data = borderDataCurrent)
      if (! is.null(currentDate)) {
        currentStaticPlotParams [["date"]] <- currentDate
      }
    }
    staticPlot <- do.call (what = mapGeographic, args = currentStaticPlotParams)

    print (staticPlot)
  }
  # end the collection of frames
  grDevices::dev.off()

  # generate an animation (optimise it if possible)
  tryCatch({
    animatedMap <- magick::image_animate(frames, delay = 100 * animation.time.interval, optimize = TRUE)
  }, error=function(cond) {
    animatedMap <- magick::image_animate(frames, delay = 100 * animation.time.interval, optimize = FALSE)
  })

  # save the file (if needed)
  if (! is.null (output.path)) {
    magick::image_write(image = animatedMap, path = paste0(output.path, ".", animation.format), format = animation.format)
  }

  return (animatedMap)
}

#' Filter the data for a given animation step
#' @param data the data set to filter
#' @param animation.variable.name the name of the variable to use to animate the map
#' @param animation.variable.value the current animation variable value, for which to generate a static frame
#' @return the filtered data set
filter.animation.data <- function (data, animation.variable.name, animation.variable.value) {
  if (is.null (data)) {
    return (data)
  }
  return (data [data[[animation.variable.name]] == animation.variable.value,])
}

#' Prepare a data set for animated display
#' @param data the data set on which to build the display (as a data.frame)
#' @param variable.name the name of the variable to use to animate the display
#' @param variable.order a list of values to define the order of frames
#' @param ordered.variable.name the name of the variable to create to describe the ordered variable (when variable.order is not NULL)
#' @return an updated data set, ready for animation
#' @examples
#' prepare.for.animation (mydata, "year")
#' prepare.for.animation (data = mydata, variable.name = "month",
#'                        variable.order = c ("Jan", "Feb", "Apr", "Mar", "Dec", "May"), ordered.variable.name = "month_ordered")
prepare.for.animation <- function (data, variable.name = NULL, variable.order = NULL, ordered.variable.name = NULL) {
  # no data or no animation variable => nothing to do
  if ((is.null(data)) || (is.null (variable.name))) {
    return (data)
  }

  # the variable name should be in the data set
  stopifnot (variable.name %in% colnames(data))

  # the variable.order should not have duplicate keys
  # the ordered variable name should not be in the data set
  if ((! is.null (variable.order)) && (! is.null (ordered.variable.name))) {
    stopifnot (length (unique (variable.order)) == length (variable.order))
    stopifnot (! ordered.variable.name %in% colnames(data))
  }

  # if an order is set, all the values should be described
  if (! is.null (variable.order)) {
    stopifnot (all (unique(data [[variable.name]]) %in% variable.order))
  }

  # ignore rows with NULL or NA
  updatedData <- data [(! is.na(variable.name)) & (! is.null(variable.name)),]

  if (! is.null (variable.order)) {
    updatedData [[ordered.variable.name]] <- factor (data [[variable.name]], levels = variable.order)
  }

  return (updatedData)
}

#' Generate a sorted vector of animation variable values
#' @param animation.variable.name the name of the variable to use to animate the map
#' @param fill.data the data related to static fill
#' @param border.data the data related to border static map
#' @return the sorted values as a data.frame
sorted.animation.variable.values <- function (animation.variable.name, fill.data, border.data) {
  # determine whether a data.set is NULL
  nullFillData <- is.null (fill.data)
  nullBorderData <- is.null (border.data)

  animationVariableValues <- c()
  if ((! nullFillData) && nullBorderData) {
    animationVariableValues <- unique (fill.data [[animation.variable.name]])
  } else if (nullFillData && (! nullBorderData)) {
    animationVariableValues <- unique (border.data [[animation.variable.name]])
  } else if ((! nullFillData) && (! nullBorderData)) {
    animationVariableValues <- intersect (unique (fill.data [[animation.variable.name]]),
                                          unique (border.data [[animation.variable.name]]))
  }
  animationVariableValues <- sort (animationVariableValues)
  return (animationVariableValues)
}

#' Extract the date to use for a static plot as part of an animation
#' @param date.variable.name the name of the variable used to define the date
#' @param current.fill.data the fill.data used for the current static map
#' @param current.border.data the border.data used for the current static map
#' @return the current date from the filtered data set (NULL if no data set)
current.animation.date <- function (date.variable.name, current.fill.data, current.border.data) {
  stopifnot (! is.null(date.variable.name))
  if (! is.null(current.fill.data)) {
    stopifnot (date.variable.name %in% names(current.fill.data))
    # there shall be only one value, to clearly define which date to use
    stopifnot(length(unique(current.fill.data[[date.variable.name]])) == 1)
    rawDate <- current.fill.data[[date.variable.name]][[1]]
    # if the column is a date, take it directly.
    # If it is an integer, assume that it depicts a year, and that the bidding-zone configuration matches the end of the year
    if (inherits(rawDate, "Date")) {
      return (rawDate)
    } else if (inherits(rawDate, "integer")) {
      return (as.Date(ISOdate(year = rawDate, month = 12, day = 31)))
    } else {
      # unable to process other formats of dates
      stopifnot (FALSE)
    }
    # date currently does not apply to border data
  } else {
    return (NULL)
  }
}
