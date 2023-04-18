#' draw_scr_criterion
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
draw_scr_criterion <- function(core_frame, current_layer, facet_name) {
  if (as.character(facet_name) %in% names(current_layer$lines)) {
    for (key in names(current_layer$lines[[facet_name]])) {

      if (!("level" %in% names(current_layer$lines[[facet_name]][[key]]))) {
        stop(paste("level not found for key:", key))
      }

      if (!("x1" %in% names(current_layer$lines[[facet_name]][[key]]))) {
        stop(paste("x1 not found for key:", key))
      }

      if (!("x2" %in% names(current_layer$lines[[facet_name]][[key]]))) {
        stop(paste("x2 not found for key:", key))
      }

      if ("lty" %in% names(current_layer$lines[[facet_name]][[key]])) {
        l_lty <- current_layer$lines[[facet_name]][[key]]$lty
      }

      l_lty <- current_layer[["lty"]]
      l_col <- current_layer[["color"]]
      l_siz <- current_layer[["size"]]

      l_y <- current_layer$lines[[facet_name]][[key]][["level"]]
      l_x1 <- current_layer$lines[[facet_name]][[key]][["x1"]]
      l_x2 <- current_layer$lines[[facet_name]][[key]][["x2"]]

      lines(
        c(l_x1, l_x2),
        c(l_y, l_y),
        lty = l_lty,
        col = l_col,
        lwd = l_siz
      )
    }
  }

  if (is.na(facet_name)) {
    for (key in names(current_layer$lines)) {
      if (!("level" %in% names(current_layer$lines[[key]]))) {
        stop(paste("level not found for key:", key))
      }

      if (!("x1" %in% names(current_layer$lines[[key]]))) {
        stop(paste("x1 not found for key:", key))
      }

      if (!("x2" %in% names(current_layer$lines[[key]]))) {
        stop(paste("x2 not found for key:", key))
      }

      if ("lty" %in% names(current_layer$lines[[facet_name]][[key]])) {
        l_lty <- current_layer$lines[[facet_name]][[key]]$lty
      }

      l_lty <- current_layer[["lty"]]
      l_col <- current_layer[["color"]]
      l_siz <- current_layer[["size"]]

      l_y <- current_layer$lines[[key]][["level"]]
      l_x1 <- current_layer$lines[[key]][["x1"]]
      l_x2 <- current_layer$lines[[key]][["x2"]]

      lines(
        c(l_x1, l_x2),
        c(l_y, l_y),
        lty = l_lty,
        col = l_col,
        lwd = l_siz
      )
    }
  }
}
