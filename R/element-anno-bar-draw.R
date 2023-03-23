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

  # In case no phases are included?
  if (!("g" %in% names(core_frame$aes))) {
    core_frame$aes["g"] <- "g"
    current_data[, "g"] <- "0"
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

  #abline()

  box(bty = "U")

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {

    current_data_slice <- current_data[which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    mod_y = (current_data_slice[, as.character(local_aesthetics["y"])] / 100) * max_y

    plot_frame = data.frame(
      Y = current_data_slice[, as.character(local_aesthetics["y"])],
      X = current_data_slice[, as.character(local_aesthetics["x"])],
      mod_y = mod_y,
      max_y = max_y,
      pct = mod_y / max_y
    )

    if (!is.na(current_layer["styler"])) {
      current_layer[["styler"]](plot_frame = plot_frame,
                                bg = current_layer$color,
                                col = current_layer$color)
    } else {
      rect(plot_frame$X - 0.25,
           0,
           plot_frame$X + 0.25,
           plot_frame$mod_y,
           col = current_layer$color)
    }

    # for (row in seq_len(nrow(current_data_slice))) {
    #
    #   mod_y = current_data_slice[row, as.character(local_aesthetics["y"])] / 100
    #   mod_y = mod_y * max_y
    #
    #   if (!is.na(current_layer["styler"])) {
    #
    #     plot_frame = data.frame()
    #     plot_frame$mod_y <- mod_y
    #
    #     current_layer[["styler"]](plot_frame = plot_frame,
    #                               bg = current_layer$color,
    #                               col = current_layer$color)
    #   } else {
    #     rect(current_data_slice[row, as.character(local_aesthetics["x"])] - 0.25,
    #          0,
    #          current_data_slice[row, as.character(local_aesthetics["x"])] + 0.25,
    #          mod_y,
    #          col = current_layer$color)
    #   }
    # }

    if (!is.null(current_layer[["guide_line"]])) {
      guide_line <- current_layer[["guide_line"]]
      guide_line_color <- current_layer[["guide_line_color"]]
      guide_line_type <- current_layer[["guide_line_type"]]
      guide_line_size <- current_layer[["guide_line_size"]]

      # Note: have to work from primary y-axis

      pre_clip_pars <- par("usr")

      x1 <- pre_clip_pars[1]
      x2 <- pre_clip_pars[2]
      y1 <- pre_clip_pars[3]
      y2 <- pre_clip_pars[4]

      clip(x1, x2, y1, y2)

      abline(h = (guide_line / 100) * max_y,
             lwd = guide_line_size,
             lty = guide_line_type,
             col = guide_line_color)

      clip(-1000000, 1000000,
           -1000000, 1000000)
    }
  }

  mtext(label_y,
        side = 4,
        outer = TRUE)
}
