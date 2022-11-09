#' scr_guide_line
#'
#' This is an annotation illustrating an aim/reduction line
#'
#' @param core_frame fxl object
#' @param coords start and finish coords for aim line
#' @param color from base
#' @param lty line type
#' @param lwd line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_guide_line <- function(core_frame,
                           coords,
                           facet = NA,
                           color = "red",
                           lty = 1,
                           lwd = 1) {

  newlayer <- list()
  newlayer[["type"]] <- "guide_line"
  newlayer[["coords"]] <- coords
  newlayer[["col"]] <- color
  newlayer[["facet"]] <- facet
  newlayer[["lty"]] <- lty
  newlayer[["lwd"]] <- lwd

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
