#' Generic utility functions related to file paths
#' Using base::system.file instead of system.file leads to failures, e.g. when running automated tests

#' Retrieve the directory where the geographic coordinates unrlying the maps are located
#'
#' @return the full path to the directory as a file.path
#' @examples
#' maps.coordinates.dir ()
#' @export
maps.coordinates.dir <- function() {
  genericCoordinatesDir <- system.file("extdata", "geographic", package = "eneRgymaps", mustWork = TRUE)
  return (genericCoordinatesDir)
}

#' Retrieve the directory where example data sets are located
#'
#' @return the full path to the directory as a file.path
#' @examples
#' example.data.dir ()
#' @export
example.data.dir <- function() {
  exampleDataDir <- system.file("extdata", "examples", package = "eneRgymaps", mustWork = TRUE)
  return (exampleDataDir)
}

#' Retrieve the directory where code examples are located
#'
#' @return the full path to the directory as a file.path
#' @examples
#' example.code.dir ()
#' @export
example.code.dir <- function() {
  exampleCodeDir <- system.file("examples", package = "eneRgymaps", mustWork = TRUE)
  return (exampleCodeDir)
}
