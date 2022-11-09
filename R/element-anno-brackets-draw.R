#' draw_brackets
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
draw_brackets <- function(core_frame, current_layer, facet_name) {
  if (current_layer$facet == as.character(facet_name)) {

    for (bindex in seq_along(length(current_layer$brackets))) {
      current_bracket <- current_layer$brackets[[bindex]]

      l_lty <- as.numeric(current_layer[["lty"]])

      if ("lty" %in% names(current_bracket))
        l_lty <- current_bracket$lty

      segments(
        x0          = as.numeric(current_bracket[["x0"]]),
        x1          = as.numeric(current_bracket[["x1"]]),
        y0          = as.numeric(current_bracket[["y0"]]),
        y1          = as.numeric(current_bracket[["y0"]]),

        col         = as.character(current_layer[["color"]]),
        lty         = l_lty,
        lwd         = as.numeric(current_layer[["lwd"]]))

      arrows(x0     = as.numeric(current_bracket[["x0"]]),
             x1     = as.numeric(current_bracket[["x0"]]),
             y0     = as.numeric(current_bracket[["y0"]]),
             y1     = as.numeric(current_bracket[["y1"]]),

             length = as.numeric(current_layer[["length"]]),
             angle  = as.numeric(current_layer[["angle"]]),
             code   = as.numeric(current_layer[["code"]]),
             col    = as.character(current_layer[["color"]]),
             lty    = l_lty,
             lwd    = as.numeric(current_layer[["lwd"]]))

      arrows(x0     = as.numeric(current_bracket[["x1"]]),
             x1     = as.numeric(current_bracket[["x1"]]),
             y0     = as.numeric(current_bracket[["y0"]]),
             y1     = as.numeric(current_bracket[["y1"]]),
             length = as.numeric(current_layer[["length"]]),
             angle  = as.numeric(current_layer[["angle"]]),
             code   = as.numeric(current_layer[["code"]]),
             col    = as.character(current_layer[["color"]]),
             lty    = l_lty,
             lwd    = as.numeric(current_layer[["lwd"]]))
    }
  }
}
