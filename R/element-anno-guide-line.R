#' scr_anno_guide_line
#'
#' This is an annotation illustrating an aim/reduction line
#'
#' @param core_frame fxl object
#' @param coords start and finish coords for aim line
#' @param facet panel to draw upon
#' @param color from base
#' @param lty line type
#' @param lwd line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns a layer to the core plotting object
#'
#' @export
scr_anno_guide_line <- function(core_frame,
                           coords,
                           facet = NA,
                           color = "black",
                           lty = 1,
                           lwd = 1) {

  newlayer <- list()
  newlayer[["type"]] <- "guide_line"

  # TODO: check for appropriate facet options
  newlayer[["facet"]] <- facet

  # TODO: check for custom objects
  newlayer[["coords"]] <- coords

  assert_input_type(color, "character", "color")
  newlayer[["col"]] <- color

  assert_input_type(lwd, "numeric", "lwd")
  newlayer[["lwd"]] <- lwd

  assert_input_type(lty, "numeric", "lty")
  newlayer[["lty"]] <- lty

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
