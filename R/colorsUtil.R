# Utilities related to colors

#' Get a given color palette length
#'
#' @param palette the color palette (if of length 1, assumed to be the name of a palette)
#' @return the size of the palette as an integer
#' @importFrom testit has_warning
#' @export
palette.length <- function (palette) {
  if (length(palette) > 1) {
    return (length(palette))
  }

  # otherwise, the palette is assumed to be a name
  paletteName <- palette

  i <- 3
  while (! testit::has_warning (expr = RColorBrewer::brewer.pal(n = i, name = paletteName))) {
    i <- i + 1
  }

  i <- i - 1 # last while loop iteration led to failure, so it needs to be offset
  return (i)
}


#' Resize a palette to a given length
#'
#' @param palette the color palette (if of length 1, assumed to be the name of a palette)
#' @param length the final palette length as an integer
#' @return the resized palette
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal
#' @export
palette.resize <- function (palette, length) {
  paletteLength <- eneRgymaps::palette.length (palette = palette)

  colorPalette <- palette
  if (length(palette) == 1) { # if the length is one, the length is likely not to actually describe the palette
    colorPalette <- RColorBrewer::brewer.pal(n = paletteLength, name = palette)
  }
  return (grDevices::colorRampPalette(colors = colorPalette) (length))
}



#' Create a palette describing a random sample of different colours
#'
#' @param length the palette length as an integer
#' @return the colour palette as a vector
#' @importFrom grDevices colorRampPalette
#' @importFrom RColorBrewer brewer.pal brewer.pal.info
#' @export
palette.colors.sample <- function (length) {

  qual_col_pals = RColorBrewer::brewer.pal.info[RColorBrewer::brewer.pal.info$category == 'qual',]
  col_vector = unlist(mapply(RColorBrewer::brewer.pal, qual_col_pals$maxcolors, rownames(qual_col_pals)))

  return (sample (col_vector, length))
}


#' Create a palette with colours describing red, orange and green (and possibly grey for NAs)
#'
#' @param forward whether to generate a list describing red-orange-green, or green-orange-red
#' @param add.grey whether to add a color for "non-applicable" at the end
#' @return the colour palette as a vector
#' @importFrom grDevices rgb
red.orange.green.palette.raw <- function (forward = TRUE, add.grey = FALSE) {
  red <- grDevices::rgb (red = 209, green = 32, blue = 40, maxColorValue = 255)
  orange <- grDevices::rgb (red = 254, green = 184, blue = 17, maxColorValue = 255)
  green <- grDevices::rgb (red = 68, green = 167, blue = 71, maxColorValue = 255)
  gray <- grDevices::rgb (red = 100, green = 100, blue = 102, maxColorValue = 255)
  rog <- c (red, orange, green)

  if (! forward) {
    rog <- rev (rog)
  }
  if (add.grey) {
    rog <- c(rog, gray)
  }

  return (rog)
}

#' Create a palette with colours describing red, orange and green
#'
#' @inherit red.orange.green.palette.raw return
#' @inheritParams red.orange.green.palette.raw
#' @export
red.orange.green.palette <- function (add.grey = FALSE) {
  return (red.orange.green.palette.raw (forward = TRUE, add.grey = add.grey))
}

#' Create a palette with colours describing green, orange and red
#'
#' @inherit red.orange.green.palette.raw return
#' @inheritParams red.orange.green.palette.raw
#' @export
green.orange.red.palette <- function (add.grey = FALSE) {
  return (red.orange.green.palette.raw (forward = FALSE, add.grey = add.grey))
}

#' Create a color palette in line with the visual identity of the EU agency ACER
#' (copyright ACER)
#'
#' @param add.greys whether the palette should include the two ACER grey colors (e.g. for NAs)
#' @return the color palette as a vector
#' @export
acer.colors.palette <- function (add.greys = FALSE) {
  pal <- c("#CAF1FC","#FFF5B3","#C0F9CE","#DBCAFC","#FCC0C0","#FFD885","#BAD1FF","#9DE0D0")

  if (add.greys) {
    grey1 <- grDevices::rgb (red = 230, green = 230, blue = 230, maxColorValue = 255)
    grey2 <- grDevices::rgb (red = 200, green = 200, blue = 200, maxColorValue = 255)
    pal <- c(pal, grey1, grey2)
  }

  return (pal)
}

#' Check whether a given color is valid
#'   # a color is valid if
# - it is in the list of predefined color strings (e.g. "red")
# - it is a valid hexadecimal color description (e.g. "#FF2400FF")
#' @param color a color
#' @return a boolean describing whether the color is valid
#' @importFrom grDevices col2rgb
#' @export
#' @examples
#' valid.color ("blue") # TRUE
#' valid.color (color = "#FF2400FF") # TRUE
#' valid.color ("myNewColor") # FALSE
valid.color <- function (color) {
  colorTest <- try(expr = grDevices::col2rgb (col = color), silent = TRUE)
  isValidColor <- ! base::inherits (colorTest, "try-error")
  return (isValidColor)
}

#' Check whether given colors are valid
#' @param colors a vector of colors
#' @param silent whether to display a message for invalid colors
#' @return a vector of booleans describing which colors are valid
#' @export
#' @examples
#' eneRgymaps::valid.colors(colors = c ("Blue", "green", "YELLOW", "#FF2400FF"), silent = TRUE)
#' eneRgymaps::valid.colors(colors = c ("Blue", "myOwnColor"), silent = FALSE)
valid.colors <- function (colors, silent = TRUE) {
  colorsAssessment <- sapply (X = colors, FUN = eneRgymaps::valid.color, USE.NAMES = FALSE)

  if ((! silent) & (FALSE %in% colorsAssessment)) {
    invalidColors <- colors [which (! colorsAssessment, arr.ind = TRUE)]
    warning (paste ("Invalid colors :", paste (invalidColors, collapse = ", ")))
  }
  return (colorsAssessment)
}
