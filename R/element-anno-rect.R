#' scr_anno_rect
#'
#' @param core_frame fxl object
#' @param rects list of keyed entries to be drawn on respective facets
#' @param color from base
#' @param fill from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns a layer to the core plotting object
#'
#' @export
#' @importFrom rlang enexprs
scr_anno_rect <- function(core_frame,
                     rects = NULL,
                     color = "black",
                     fill = "black") {

  newlayer <- list()
  newlayer[["type"]] <- "rectangle"

  # TODO: check for appropriate facet options
  # newlayer[["facet"]] <- facet

  newlayer[["rects"]] <- rects

  assert_input_type(color, "character", "color")
  newlayer[["color"]] <- color

  assert_input_type(fill, "character", "fill")
  newlayer[["fill"]] <- fill

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
