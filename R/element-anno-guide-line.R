#' scr_guide_line
#'
#' This is an annotation illustrating an aim/reduction line
#'
#' @param core_frame fxl object
#' @param coords start and finish coords for aim line
#' @param color from base
#' @param lty line type
#' @param lwd line width
#' @param facet panel to draw upon
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_guide_line <- function(core_frame,
                           coords,
                           facet = NA,
                           color = "red",
                           lty = 1,
                           lwd = 1) {
  # TODO: Tests for remaining params

  newlayer <- list()
  newlayer[["type"]] <- "guide_line"
  newlayer[["coords"]] <- coords
  newlayer[["col"]] <- color
  newlayer[["facet"]] <- facet

  isValidNumericVector(
    object = lty,
    name = "lty"
  )

  newlayer[["lty"]] <- lty

  isValidNumericVector(
    object = lwd,
    name = "lwd"
  )

  newlayer[["lwd"]] <- lwd

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
