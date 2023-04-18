#' scr_criterion_lines
#'
#' @param core_frame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param lines lines to draw
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
#' @importFrom rlang enexprs
scr_criterion_lines <- function(core_frame,
                      lty = 1,
                      color = "black",
                      size = 1,
                      lines = NULL) {
  newlayer <- list()
  newlayer[["type"]] <- "criterion_line"

  assert_input_type(lty, "numeric", "lty")
  newlayer[["lty"]] <- lty

  assert_input_type(color, "character", "color")
  newlayer[["color"]] <- color

  assert_input_type(size, "numeric", "size")
  newlayer[["size"]] <- size

  # TODO: error checking
  newlayer[["lines"]] <- lines

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
