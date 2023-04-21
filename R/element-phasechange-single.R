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
#' @returns a layer to the core plotting object
#'
#' @export
scr_plines <- function(core_frame,
                       lines = NULL,
                       lwd = 1,
                       lty = 1,
                       col = "black") {

  newlayer <- list()
  newlayer[["type"]] <- "phase_lines"

  assert_input_type(lwd, "numeric", "lwd")
  newlayer[["lwd"]] <- lwd

  assert_input_type(lty, "numeric", "lty")
  newlayer[["lty"]] <- lty

  assert_input_type(col, "character", "col")
  newlayer[["col"]] <- col

  # TODO: error checking
  newlayer[["lines"]] <- lines

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
