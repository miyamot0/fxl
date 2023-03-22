#' draw_label_facet
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
draw_label_facet <- function(core_frame, current_layer, facet_name) {
  current_label <- current_layer$labels[[as.character(facet_name)]]

  label <- facet_name
  custom_label <- FALSE

  if ("label" %in% names(current_label)) {
    label <- current_label[["label"]]
    custom_label <- TRUE
  }

  if (label == "" || label == facet_name || custom_label == TRUE) {

    temp_x <- ifelse(
      !is.null(current_label[["x"]]),
      current_label[["x"]], current_layer$x
    )

    temp_y <- ifelse(
      !is.null(current_label[["y"]]),
      current_label[["y"]], current_layer$y
    )

    font_c <- ifelse(
      "font" %in% names(current_label),
      current_label[["font"]], 1
    )

    text(
      x = temp_x,
      y = temp_y,
      cex = current_layer[["cex"]],
      adj = current_layer[["adj"]],
      font = font_c,
      labels = label
    )
  }
}
