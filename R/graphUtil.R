#' Generic graph util functions

#' Save a generated plot as an image
#'
#' Save a generated graphic as a .png/.svg image for later use
#' @param plot a ggplot graphic
#' @param destinationpath the path in which to save the graph (without file extension)
#' @param dpi the desired resolution for the .png image (the .svg image may be infinitely scaled up/down)
#' @inheritDotParams ggplot2::ggsave -filename -plot -path -device -dpi
#' @importFrom ggplot2 ggsave
#' @examples
#' myPlot <- ggplot2::ggplot()
#' eneRgymaps::save.plot (plot = myPlot, destinationpath = file.path("C:", "data"))
#' @export
save.plot <- function (plot, destinationpath, dpi = 500, ...) {

  outputFileName <- base::basename (destinationpath)
  outputDirectory <- base::dirname (destinationpath)
  if ((is.null(outputDirectory)) | (! dir.exists(outputDirectory))) {
    stop(paste("Unable to save plot : the output directory does not exist", outputDirectory, sep=" "))
  }

  ggplot2::ggsave(filename = paste(outputFileName, ".png", sep=""), plot = plot, path = outputDirectory, device = "png", dpi = dpi, ...)
  ggplot2::ggsave(filename = paste(outputFileName, ".svg", sep=""), plot = plot, path = outputDirectory, device = "svg", ...)
}
