#' scr_plines_mbd
#'
#' @param core_frame fxl object
#' @param lines phase lines to be drawn
#' @param lty phase lines types
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns a layer to the core plotting object
#'
#' @export
scr_plines_mbd <- function(core_frame,
                           lty = 1,
                           lines = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "mbd_phase_lines"

  newlayer[["lines"]] <- lines

  assert_input_type(lty, "numeric", "lty")
  newlayer[["lty"]] <- lty

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
