#' scr_plines
#'
#' @param core_frame fxl object
#' @param lines phase lines to be drawn
#' @param lwd from base
#' @param lty from base
#' @param col from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_plines <- function(core_frame,
                       lines = NULL,
                       lwd = 1,
                       lty = 1,
                       col = "black") {

  newlayer <- list()
  newlayer[["type"]] <- "phase_lines"
  newlayer[["lines"]] <- lines
  newlayer[["lwd"]] <- lwd
  newlayer[["lty"]] <- lty
  newlayer[["col"]] <- col

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
