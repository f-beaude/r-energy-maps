# managing geographic coordinates

#' generate a uniform geographic interval
#' @param value the value to apply to all boundaries (NA means no value)
#' @return the geographic interval with latitude/longitude min/max set to value
#' @examples
#' eneRgymaps::uniform.geographic.interval (0)
#' eneRgymaps::uniform.geographic.interval (NA)
#' @export
uniform.geographic.interval <- function (value) {
  stopifnot (is.na (value) || is.numeric (value))
  return (list ("longitude.min" = value, "longitude.max" = value,
                "latitude.min" = value, "latitude.max" = value))
}

#' Expand latitude/longitude min/max values to fit aspect ratio criteria
#' @details
#' When the aspect ratio is too high, i.e. the plot is too wide, increase the height.
#' When the aspect ratio is too low, i.e. the plot is too narrow, increase the width
#'
#' @param coordinates the geographic coordinates describing the interval
#' @param aspect.ratio.min minimum aspect ratio of the plot as a float
#' @param aspect.ratio.max maximum aspect ratio of the plot as a float
#' @return the updated geographic interval (if the initial aspect ratio is between min and max, nothing changes)
#' @examples
#' eneRgymaps::fit.geographic.interval.to.aspect.ratio (coordinates, 0, 1.5)
#' @export
fit.geographic.interval.to.aspect.ratio <- function(coordinates, aspect.ratio.min, aspect.ratio.max) {
  stopifnot (all (is.numeric (c(aspect.ratio.min, aspect.ratio.max))))
  stopifnot (aspect.ratio.min <= aspect.ratio.max)
  stopifnot (aspect.ratio.min >= 0)
  stopifnot (eneRgymaps::geographic.interval.well.defined (coordinates, check.bounds.consistency = TRUE))
  # unable to compute aspect ratio if latitude.max == latitude.min
  stopifnot (coordinates [["latitude.max"]] > coordinates [["latitude.min"]])

  aspectRatio <- (coordinates [["longitude.max"]] - coordinates [["longitude.min"]]) /
    (coordinates [["latitude.max"]] - coordinates [["latitude.min"]])

  # plot is too narrow => enlarge it
  if (aspectRatio < aspect.ratio.min) {
    stopifnot (coordinates [["longitude.max"]] - coordinates [["longitude.min"]] > 0)

    # delta to enlarge both at min and max longitude value (therefore divide by 2)
    longitudeDelta <- (coordinates [["longitude.max"]] - coordinates [["longitude.min"]]) * (aspect.ratio.min - aspectRatio) / (2 * aspectRatio)
    coordinates [["longitude.max"]] <- coordinates [["longitude.max"]] + longitudeDelta
    coordinates [["longitude.min"]] <- coordinates [["longitude.min"]] - longitudeDelta
    stopifnot (abs ((coordinates [["longitude.max"]] - coordinates [["longitude.min"]]) /
                        (coordinates [["latitude.max"]] - coordinates [["latitude.min"]]) - aspect.ratio.min) <= 0.1)

    # plot is too wide, increase its height
  } else if (aspectRatio > aspect.ratio.max) {
    stopifnot (coordinates [["latitude.max"]] - coordinates [["latitude.min"]] > 0)
    latitudeDelta <- (coordinates [["latitude.max"]] - coordinates [["latitude.min"]]) * (aspectRatio - aspect.ratio.max) / (2 * aspect.ratio.max)
    coordinates [["latitude.max"]] <- coordinates [["latitude.max"]] + latitudeDelta
    coordinates [["latitude.min"]] <- coordinates [["latitude.min"]] - latitudeDelta

    stopifnot (abs((coordinates [["longitude.max"]] - coordinates [["longitude.min"]]) /
                        (coordinates [["latitude.max"]] - coordinates [["latitude.min"]]) - aspect.ratio.max) <= 0.1)
  }

  return (coordinates)
}


#' Check if a geographic interval is well defined
#' @param coordinates the geographic coordinates describing the interval
#' @param check.bounds.consistency whether to check that min.value <= max.value
#' @return whether the list is well defined
#' @importFrom stats na.omit
#' @examples
#' eneRgymaps::geographic.interval.well.defined (list ("longitude.min" = -1000, "longitude.max" = 1500, "latitude.min" = 100.5, "latitude.max" = 5000))
#' eneRgymaps::geographic.interval.well.defined (list ("longitude.min" = -1000, "longitude.max" = -2000, "latitude.min" = 100.5, "latitude.max" = 5000), check.bounds.consistency = FALSE)
#' eneRgymaps::geographic.interval.well.defined (list ("longitude.min" = -1000, "longitude.max" = -500, "latitude.min" = 100.5, "latitude.max" = 5000, "altitude" = 500))
#' @export
geographic.interval.well.defined <- function (coordinates, check.bounds.consistency = TRUE) {
  # exactly 4 keys and numeric values should be provided (long/lat * min/max)
  if ((length (stats::na.omit(unlist(coordinates, use.names = FALSE))) != 4) ||
      (length (names (coordinates)) != 4)){
    return (FALSE)
  }

  # only the following keys are allowed
  if (! all.equal (sort(x = names (coordinates), decreasing = FALSE),
      c ("latitude.max", "latitude.min", "longitude.max", "longitude.min"))) {
    return (FALSE)
  }

  # all values should be numeric
  if (! is.numeric(unlist(coordinates, use.names = FALSE))) {
    return (FALSE)
  }

  # min/max values should be consistent
  if (check.bounds.consistency && ((coordinates [["latitude.min"]] > coordinates [["latitude.max"]]) ||
      (coordinates [["longitude.min"]] > coordinates [["longitude.max"]]))) {
    return (FALSE)
  }

  # all checks passed => fine
  return (TRUE)
}
