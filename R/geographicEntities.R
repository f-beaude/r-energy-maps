#' managing geographic entities


#' Check whether a geographic entity name is relevant
#'
#' @param geographic.entity.name the geographic entity name
#' @return whether it is a relevant name
#' @examples
#' is.relevant.geographic.entity.name ("Bidding zone")
#' is.relevant.geographic.entity.name (geographic.entity.name = "bathtub")
#' @export
is.relevant.geographic.entity.name <- function (geographic.entity.name) {
  relevantNames <- c("Bidding zone", "Control area", "Country")
  return (geographic.entity.name %in% c(relevantNames))
}

#' Reverse a given border name
#' @param border.name the border name to reverse
#' @param separator the separator between the two entities of the border
#' @return the reverse border name as a string
#' @examples
#' reverse.one.border.name ("BE-FR", "-") # "FR-BE"
reverse.one.border.name <- function (border.name, separator) {
  stopifnot (grepl (pattern = separator, x = border.name))
  reverseBorders <- paste (rev(base::strsplit(x = border.name, split = separator, fixed = TRUE) [[1]]), collapse = separator)
  stopifnot (length (reverseBorders) == length (border.name))
  return (reverseBorders)
}

#' Reverse one or more border names
#' @inheritParams reverse.one.border.name
#' @examples
#' reverse.border.name ("BE-FR", "-")
#' reverse.border.name (border.name = c("BE-FR", "DE-NL"), separator = "-")
#' @export
reverse.border.name <- base::Vectorize(FUN = reverse.one.border.name, SIMPLIFY = TRUE, USE.NAMES = FALSE)
#'

#' EIC code of Germany/Luxembourg
#' @export
#' @return the EIC code as a string
eic.code.DELU <- function() {
  return ("10Y1001A1001A82H")
}

#' EIC code of Austria
#' @export
#' @return the EIC code as a string
eic.code.AT <- function() {
  return ("10YAT-APG------L")
}

#' EIC codes describing the split bidding-zones Austria, and Germany/Luxembourg
#' bidding-zones
#' @export
#' @return the list of EIC codes as a vector
eic.codes.split.AT.DE <- function () {
  return (c (eneRgymaps::eic.code.AT(), eneRgymaps::eic.code.DELU()))
}

#' EIC code describing the merged Austria - Germany/Luxembourg bidding-zone
#' @export
#' @return the list of EIC codes as a string
eic.code.merged.AT.DE <- function () {
  return ("10Y1001A1001A63L")
}

#' EIC codes of the German TSOs
#' @export
#' @return the list of EIC codes as a vector
eic.codes.DE.TSOs <- function() {
  return(c("10YDE-VE-------2", "10YDE-RWENET---I", "10YDE-EON------1", "10YDE-ENBW-----N"))
}

#' EIC code of the Luxembourgian TSO
#' @export
#' @return the list of EIC codes as a string
eic.code.LU.TSO <- function() {
  return("10YLU-CEGEDEL-NQ")
}


#' Check whether a given vector relates to
#' split and/or merged AT - DE/LU bidding-zones
#' @param data the data to scan (as a vector)
#' @return whether the split and merged bidding-zone EIC codes are included in the data
#' @export
merged.split.AT.DE.in.data <- function (data) {
  return (list (split = any (eneRgymaps::eic.codes.split.AT.DE() %in% data),
                merged = eneRgymaps::eic.code.merged.AT.DE() %in% data))
}
