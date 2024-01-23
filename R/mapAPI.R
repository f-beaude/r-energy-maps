# API to display geographic maps

#' draw a map related to bidding zone-level information
#' @inheritDotParams mapGeographic -geographic.entity.name
#' @return the map as a plot
#' @export
mapBiddingZone <- function (...)  {
 return (mapGeographic (geographic.entity.name = "Bidding zone", ...))
}

#' draw a map related to country-level information
#' @inheritDotParams mapGeographic -geographic.entity.name
#' @return the map as a plot
#' @export
mapCountry <- function (...)  {
  return (mapGeographic (geographic.entity.name = "Country", ...))
}

#' draw a map related to control area-level information
#' @inheritDotParams mapGeographic -geographic.entity.name
#' @return the map as a plot
#' @export
mapControlArea <- function (...)  {
  return (mapGeographic (geographic.entity.name = "Control area", ...))
}

#' draw a map related to LFC area-level information
#' @inheritDotParams mapGeographic -geographic.entity.name
#' @return the map as a plot
#' @export
mapLFCArea <- mapControlArea


#' draw an animated map related to bidding zone-level information
#' @inheritDotParams animate.mapGeographic -geographic.entity.name
#' @return the animated map
#' @export
animate.mapBiddingZone <- function (...)  {
  return (animate.mapGeographic (geographic.entity.name = "Bidding zone", ...))
}

#' draw an animated map related to country-level information
#' @inheritDotParams animate.mapGeographic -geographic.entity.name
#' @return the animated map
#' @export
animate.mapCountry <- function (...)  {
  return (animate.mapGeographic (geographic.entity.name = "Country", ...))
}

#' draw an animated map related to control area-level information
#' @inheritDotParams animate.mapGeographic -geographic.entity.name
#' @return the animated map
#' @export
animate.mapControlArea <- function (...)  {
  return (animate.mapGeographic (geographic.entity.name = "Control area", ...))
}

#' draw a animated map related to LFC area-level information
#' @inheritDotParams animate.mapGeographic -geographic.entity.name
#' @return the animated map
#' @export
animate.mapLFCArea <- animate.mapControlArea
