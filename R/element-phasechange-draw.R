#' draw_scr_plines
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_scr_plines <- function(core_frame, current_layer, facet_name) {
  if (as.character(facet_name) %in% names(current_layer$lines)) {
    for (key in names(current_layer$lines[[facet_name]])) {

      l_lty <- current_layer[["lty"]]
      l_x1 <- current_layer$lines[[facet_name]][[key]][["x1"]]
      l_x2 <- ifelse(
        is.null(current_layer$lines[[facet_name]][[key]][["x2"]]),
        l_x1, current_layer$lines[[facet_name]][[key]][["x2"]]
      )

      l_y1 <- ifelse(
        is.null(current_layer$lines[[facet_name]][[key]][["y1"]]),
        0, current_layer$lines[[facet_name]][[key]][["y1"]]
      )
      l_y2 <- ifelse(
        is.null(current_layer$lines[[facet_name]][[key]][["y2"]]),
        0, current_layer$lines[[facet_name]][[key]][["y2"]]
      )

      temp_y1 <- ifelse(
        l_y1 == 0, -((as.numeric(core_frame$dims[["max.local.y"]]) -
                        as.numeric(core_frame$dims[["min.local.y"]])) *
                       0.04), current_layer$lines[[facet_name]][[key]][["y1"]]
      )

      temp_y2 <- ifelse(
        l_y2 == 0, -((as.numeric(core_frame$dims[["max.local.y"]]) -
                        as.numeric(core_frame$dims[["min.local.y"]])) *
                       0.04), l_y2
      )

      if ("lty" %in% names(current_layer$lines[[facet_name]][[key]]))
        l_lty <- current_layer$lines[[facet_name]][[key]]$lty

      lines(
        c(l_x1, l_x2),
        c(temp_y1, temp_y2),
        lty = l_lty
      )
    }
  }
}
