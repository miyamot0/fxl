#' draw_cumsum_lines
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
draw_cumsum_lines <- function(core_frame, current_layer, facet_name) {
  if (is.na(facet_name)) {
    current_data <- core_frame$data
  } else {
    current_data <- core_frame$data[which(
      core_frame$data[, as.character(core_frame$aes["facet"])] ==
        facet_name
    ), ]
  }

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] <- "p"
    current_data[, "p"] <- "0"
  }

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {
    current_data_slice <- current_data[which(
      current_data[, as.character(core_frame$aes["p"])] ==
        p
    ), ]

    local_aesthetics <- list(
      x = as.character(core_frame$aes["x"]),
      y = as.character(core_frame$aes["y"])
    )

    if (!is.na(current_layer["aesthetics"])) {
      local_aesthetics <- list(
        x = as.character(current_layer$aesthetics["x"]),
        y = as.character(current_layer$aesthetics["y"])
      )
    }

    cumsumy <- current_data_slice[, as.character(local_aesthetics["y"])]
    cumsumy[is.na(cumsumy)] <- 0
    cumsumy <- cumsum(cumsumy)

    lines(
      current_data_slice[, as.character(local_aesthetics["x"])],
      cumsumy,
      lty = current_layer$lty,
      col = current_layer$color,
      lwd = current_layer$size
    )
  }
}
