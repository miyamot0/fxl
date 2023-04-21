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
#' @returns nothing, run for side effects
#'
#' @export
draw_label_facet <- function(core_frame, current_layer, facet_name) {
  if (is.na(facet_name) & length(names(current_layer$labels)) != 1) {
    stop("No facet name provided")
  }

  current_label <- ""

  if (is.na(facet_name) & length(names(current_layer$labels)) != 1) {
    current_label <- current_layer$labels[[1]]
  } else {
    current_label <- current_layer$labels[[as.character(facet_name)]]
  }

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

    font <- current_layer[["face"]]

    text(
      x = temp_x,
      y = temp_y,
      cex = current_layer[["cex"]],
      adj = current_layer[["adj"]],
      font = font,
      labels = label
    )
  }
}
