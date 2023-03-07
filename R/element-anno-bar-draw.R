#' draw_bar_support
#'
#' Draw bars, but on a secondary axis
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#' @param max_y top of y axis to match
#'
#' @export
draw_bar_support <- function(core_frame, current_layer, facet_name, max_y) {

  #TODO: throw error if values aren't bounded between 0 and 100
  #TODO: need to reference supplied width

  if (is.na(facet_name))  current_data   <- core_frame$data
  else                   current_data   <- core_frame$data[which(
    core_frame$data[, as.character(core_frame$aes["facet"])] == facet_name), ]

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] <- "p"
    current_data[, "p"] <- "0"
  }

  local_aesthetics <- list(
    "x" = as.character(core_frame$aes["x"]),
    "y" = as.character(core_frame$aes["y"])
  )

  if (!is.na(current_layer["aesthetics"])) {
    local_aesthetics <- list(
      "x" = as.character(current_layer$aesthetics["x"]),
      "y" = as.character(current_layer$aesthetics["y"])
    )
  }

  label_y <- as.character(local_aesthetics["y"])

  if (current_layer$label != "")  label_y <- as.character(current_layer$label)

  y_axis_ticks <- c(0 * max_y,
                    0.25 * max_y,
                    0.5 * max_y,
                    0.75 * max_y,
                    1 * max_y)

  y_axis_draw <- c("0", "25", "50", "75", "100")

  axis(4,
       labels = y_axis_draw,
       las    = 1,
       at     = y_axis_ticks)

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {

    current_data_slice <- current_data[which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    for (row in seq_len(nrow(current_data_slice))) {

      mod_y = current_data_slice[row, as.character(local_aesthetics["y"])] / 100
      mod_y = mod_y * max_y

      rect(current_data_slice[row, as.character(local_aesthetics["x"])] - 0.25,
           0,
           current_data_slice[row, as.character(local_aesthetics["x"])] + 0.25,
           mod_y,
           col = current_layer$color)
    }
  }

#  axis(side = 4,
#       las = 1,
#       at = pretty(range(c(0, current_data_slice[, as.character(local_aesthetics["y"])]))))

  mtext(label_y,
        side = 4,
        outer = TRUE)
}
