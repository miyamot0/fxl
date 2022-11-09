#' draw_bar_support
#'
#' Draw bars, but on a secondary axis
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @return
#' @export
draw_bar_support <- function(core_frame,  current_layer, facet_name) {

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

  opar <- par()

  # Add on to "new" plot
  par(new = TRUE,
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  plot(NULL,
       axes = FALSE,
       xlim = c(core_frame$dims[["min.local.x"]],
                core_frame$dims[["max.local.x"]]),
       ylim = c(0, 100),
       xlab = "",
       ylab = "",
       frame.plot = FALSE,
       las = 1,
       xaxt = "n",
       yaxt = "n")

  box(bty = "U")

  p_off <- current_layer$width / 2

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {

    current_data_slice <- current_data[which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    for (row in seq_len(nrow(current_data_slice))) {

      rect(current_data_slice[row, as.character(local_aesthetics["x"])] - p_off,
           0,
           current_data_slice[row, as.character(local_aesthetics["x"])] + p_off,
           current_data_slice[row, as.character(local_aesthetics["y"])],
           col = current_layer$color)
    }
  }

  axis(side = 4,
       las = 1,
       at = pretty(range(c(0, current_data_slice[, as.character(local_aesthetics["y"])]))))

  mtext(label_y,
        side = 4,
        outer = TRUE)

  par(opar)
}
