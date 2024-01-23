#' Generic help functions

#' Display the various possible shapes
#'
#' @importFrom ggplot2 aes geom_point geom_text ggplot scale_shape_identity scale_x_continuous scale_y_continuous
#' @examples
#' eneRgymaps::help.shapes ()
#' @export
help.shapes <- function() {
  d = data.frame (p = c(0:25,32:127))
  ggplot2::ggplot() +
    ggplot2::scale_y_continuous (name = "") +
    ggplot2::scale_x_continuous (name = "") +
    ggplot2::scale_shape_identity() +
    ggplot2::geom_point(data = d, mapping = ggplot2::aes (x = p %% 16, y = p %/% 16, shape = p), size = 5, fill = "red") +
    ggplot2::geom_text(data = d, mapping = ggplot2::aes (x = p %% 16, y = p %/% 16 + 0.25, label = p), size = 3)
}


#' Display the various possible linetypes for paths
#'
#' @importFrom ggplot2 aes geom_segment ggplot scale_linetype_discrete scale_x_continuous scale_y_discrete
#' @examples
#' eneRgymaps::help.path.linetypes ()
#' @export
help.path.linetypes <- function() {
  linetypes = data.frame(types = c(1, 2, "dashed", "dotdash", "3313", "33", "a","b","c","d","e","f","g"))
  base::message ('The string "33" specifies three units on followed by three off and "3313" specifies three units on followed by three off followed by one on and finally three off')
  base::suppressWarnings(ggplot2::ggplot() +
    ggplot2::scale_x_continuous (limits = c(0,1), breaks = NULL, name = "") +
    ggplot2::scale_y_discrete (name = "") +
    ggplot2::scale_linetype_discrete () +
    ggplot2::geom_path (data = linetypes, mapping = ggplot2::aes(x = 0, xend = 1, y = types, yend = types, linetype = types)))
}
