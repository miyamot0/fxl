#' scr_anno_brackets
#'
#' Add a layer with brackets on plot
#'
#' @param core_frame fxl class
#' @param brackets list of keyed entries to be drawn on respective facets
#' @param facet the facet which will be drawn upon
#' @param color from base
#' @param length from base
#' @param angle from base
#' @param code from base
#' @param lwd from base
#' @param lty from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns a layer to the core plotting object
#'
#' @export
scr_anno_brackets <- function(core_frame,
                         brackets = NULL,
                         facet = NULL,
                         color = "black",
                         length = 0.25,
                         angle = 30,
                         code = 2,
                         lwd = 1,
                         lty = 1) {

  newlayer <- list()
  newlayer[["type"]] <- "brackets"

  newlayer[["facet"]] <- facet

  newlayer[["brackets"]] <- brackets

  assert_input_type(color, "character", "color")
  newlayer[["color"]] <- color

  assert_input_type(length, "numeric", "length")
  newlayer[["length"]] <- length

  assert_input_type(angle, "numeric", "angle")
  newlayer[["angle"]] <- angle

  assert_input_type(code, "numeric", "code")
  newlayer[["code"]] <- code

  assert_input_type(lwd, "numeric", "lwd")
  newlayer[["lwd"]] <- lwd

  assert_input_type(lty, "numeric", "lty")
  newlayer[["lty"]] <- lty

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
