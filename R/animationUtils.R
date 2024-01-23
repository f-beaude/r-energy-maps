# Utility functions related to animations

#' List of valid animation formats
#' @return the list of valid animation formats as a vector
#' @export
valid.animation.formats <- function() {
  return (c("gif", "mp4"))
}

#' Check whether a given animation format is valid
#' @param format an animation format as a string
#' @return whether the format is valid (as a boolean)
one.animation.format.is.valid <- function (format) {
  return (format %in% eneRgymaps::valid.animation.formats())
}

#' Check whether animation formats are valid
#' @inheritParams one.animation.format.is.valid
#' @return whether each format is valid, as a vector of booleans
#' @export
animation.format.is.valid <- base::Vectorize(one.animation.format.is.valid, USE.NAMES = FALSE)
