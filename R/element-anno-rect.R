#' scr_rect
#'
#' @param core_frame fxl object
#' @param rects list of keyed entries to be drawn on respective facets
#' @param color from base
#' @param fill from base
#' @param x1 from base
#' @param x2 from base
#' @param y1 from base
#' @param y2 from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
#' @importFrom rlang enexprs
scr_rect <- function(core_frame,
                     rects = NULL,
                     color = "black",
                     fill = "black") {
  newlayer <- list()
  newlayer[["type"]] <- "rectangle"
  newlayer[["rects"]] <- rects
  newlayer[["color"]] <- color
  newlayer[["fill"]] <- fill

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
