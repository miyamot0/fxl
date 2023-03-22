#' draw_guide_line
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @export
draw_guide_line <- function(core_frame,  current_layer, facet_name) {
  is_facet_name_na <- is.na(facet_name)

  if (current_layer$facet == facet_name | is_facet_name_na) {

    for (gindex in seq_len(length(current_layer$coords))) {
      current_coords <- current_layer$coords[[gindex]]

      l_col <- as.character(current_layer[["col"]])
      l_lty <- as.numeric(current_layer[["lty"]])
      l_lwd <- as.numeric(current_layer[["lwd"]])

      if ("col" %in% names(current_coords))
        l_col <- current_coords$col

      if ("lty" %in% names(current_coords))
        l_lty <- current_coords$lty

      if ("lwd" %in% names(current_coords))
        l_lwd <- current_coords$lwd

      temp_y2 <- ifelse("y1" %in% names(current_coords),
                        current_coords$y1,
                        current_coords$y0)

      segments(
        x0  = current_coords$x0,
        x1  = current_coords$x1,
        y0  = current_coords$y0,
        y1  = temp_y2,
        lty = l_lty,
        lwd = l_lwd,
        col = l_col
      )
    }
  }
}
