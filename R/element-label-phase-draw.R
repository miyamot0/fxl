#' draw_label_phase
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
draw_label_phase <- function(core_frame, current_layer, facet_name) {

  is_facet_name_na <- (is.na(facet_name))

  if (current_layer$facet == facet_name | is_facet_name_na) {
    for (lindex in seq_len(length(current_layer$labels))) {

      label <- names(current_layer$labels)[lindex]

      current_label <- current_layer$labels[[lindex]]

      label <- ifelse(
        is.null(label),
        current_label, label
      )

      temp_x <- ifelse(
        "x" %in% names(current_label),
        current_label[["x"]], current_layer$x
      )

      temp_y <- ifelse(
        "y" %in% names(current_label),
        current_label[["y"]], current_layer$y
      )

      font_c <- ifelse(
        "font" %in% names(current_label),
        current_label[["font"]], 1
      )

      srt <- ifelse(
        "srt" %in% names(current_label),
        current_label[["srt"]], 0
      )

      temp_color <- ifelse(
        "color" %in% names(current_label),
        current_label[["color"]], "black"
      )

      temp_label <- ifelse(
        "label" %in% names(current_label),
        current_label[["label"]], label
      )

      text(
        x = temp_x,
        y = temp_y,
        cex = current_layer[["cex"]],
        adj = current_layer[["adj"]],
        font = font_c,
        col = temp_color,
        srt = srt,
        labels = temp_label
      )
    }
  }
}
