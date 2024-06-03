#' scr_cumsum_points
#'
#' @param core_frame fxl object
#' @param pch from base
#' @param color from base
#' @param fill from base
#' @param cex from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns a layer to the core plotting object
#'
#' @export
#' @importFrom rlang enexprs
scr_cumsum_points <- function(core_frame,
                              pch = 21,
                              color = "black",
                              fill = "black",
                              cex = 1,
                              mapping) {

  newlayer <- list()
  newlayer[["type"]] <- "cum_sum_points"

  assert_input_type(pch, "numeric", "pch")
  newlayer[["pch"]] <- pch

  assert_input_type(color, "character", "color")
  newlayer[["color"]] <- color

  assert_input_type(fill, "character", "fill")
  newlayer[["fill"]] <- fill

  assert_input_type(cex, "numeric", "cex")
  newlayer[["cex"]] <- cex

  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
