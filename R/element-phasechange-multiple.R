#' scr_plines_mbd
#'
#' @param core_frame fxl object
#' @param lines phase lines to be drawn
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_plines_mbd <- function(core_frame,
                           lines = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "mbd_phase_lines"
  newlayer[["lines"]] <- lines

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
