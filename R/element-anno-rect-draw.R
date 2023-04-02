#' draw_rect
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#' @param zero_axis filter out all but zeros
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
draw_rect <- function(core_frame, current_layer, facet_name, zero_axis = FALSE) {
  for (rname in names(current_layer$rects)) {
    if (rname == facet_name) {
      current_rect <- current_layer$rects[[rname]]
      current_col <- current_layer$color

      if ("color" %in% names(current_rect)) {
        current_col <- current_rect[["color"]]
      }

      current_fill <- current_layer$fill

      if ("fill" %in% names(current_rect)) {
        current_fill <- current_rect[["fill"]]
      }

      rect(
        xleft = as.numeric(current_rect[["x0"]]),
        xright = as.numeric(current_rect[["x1"]]),
        ybottom = as.numeric(current_rect[["y0"]]),
        ytop = as.numeric(current_rect[["y1"]]),
        col = current_fill,
        border = current_col
      )
    }
  }
}
