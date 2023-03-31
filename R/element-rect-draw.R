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
  if (facet_name == current_layer$facet) {

    l_x1 <- current_layer$coords[["x1"]]
    l_x2 <- current_layer$coords[["x2"]]

    l_y1 <- current_layer$coords[["y1"]]
    l_y2 <- current_layer$coords[["y2"]]

    rect(xleft   = l_x2,
         xright  = l_x1,
         ybottom = l_y2,
         ytop    = l_y1,
         col     = current_layer$fill,
         border  = current_layer$color)

  } else if (is.na(facet_name)) {
    l_x1 <- current_layer$coords[["x1"]]
    l_x2 <- current_layer$coords[["x2"]]

    l_y1 <- current_layer$coords[["y1"]]
    l_y2 <- current_layer$coords[["y2"]]

    rect(xleft   = l_x2,
         xright  = l_x1,
         ybottom = l_y2,
         ytop    = l_y1,
         col     = current_layer$fill,
         border  = current_layer$color)
  }
}
