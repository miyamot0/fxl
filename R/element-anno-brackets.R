#' scr_brackets
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
#' @export
scr_brackets <- function(core_frame,
                         brackets = NULL,
                         facet = NULL,
                         color = "black",
                         length = 0.25,
                         angle = 30,
                         code = 2,
                         lwd = 1,
                         lty = 1) {
  # TODO: Tests for remaining params

  newlayer <- list()
  newlayer[["type"]] <- "brackets"
  newlayer[["facet"]] <- facet
  newlayer[["brackets"]] <- brackets
  newlayer[["color"]] <- color
  newlayer[["length"]] <- length

  isValidNumericVector(
    object = length,
    name = "length"
  )

  newlayer[["angle"]] <- angle

  isValidNumericVector(
    object = angle,
    name = "angle"
  )

  newlayer[["code"]] <- code

  isValidNumericVector(
    object = code,
    name = "code"
  )

  newlayer[["lwd"]] <- lwd

  isValidNumericVector(
    object = lwd,
    name = "lwd"
  )

  newlayer[["lty"]] <- lty

  isValidNumericVector(
    object = lty,
    name = "lty"
  )

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
