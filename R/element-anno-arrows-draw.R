#' draw_arrows
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
draw_arrows <- function(core_frame, current_layer, facet_name) {
  is_facet_name_na <- is.na(facet_name)

  if (is_facet_name_na) {
    for (aindex in seq_len(length(current_layer$arrows))) {
      current_arrow <- current_layer$arrows[[aindex]]

      arrows(
        x0 = as.numeric(current_arrow[["x0"]]),
        x1 = as.numeric(current_arrow[["x1"]]),
        y0 = as.numeric(current_arrow[["y0"]]),
        y1 = as.numeric(current_arrow[["y1"]]),
        length = as.numeric(current_layer[["length"]]),
        angle = as.numeric(current_layer[["angle"]]),
        code = as.numeric(current_layer[["code"]]),
        col = as.character(current_layer[["color"]]),
        lty = as.numeric(current_layer[["lty"]]),
        lwd = as.numeric(current_layer[["lwd"]])
      )
    }
  } else if (current_layer$facet == as.character(facet_name)) {
    for (aindex in seq_len(length(current_layer$arrows))) {
      current_arrow <- current_layer$arrows[[aindex]]

      arrows(
        x0 = as.numeric(current_arrow[["x0"]]),
        x1 = as.numeric(current_arrow[["x1"]]),
        y0 = as.numeric(current_arrow[["y0"]]),
        y1 = as.numeric(current_arrow[["y1"]]),
        length = as.numeric(current_layer[["length"]]),
        angle = as.numeric(current_layer[["angle"]]),
        code = as.numeric(current_layer[["code"]]),
        col = as.character(current_layer[["color"]]),
        lty = as.numeric(current_layer[["lty"]]),
        lwd = as.numeric(current_layer[["lwd"]])
      )
    }
  }

}
