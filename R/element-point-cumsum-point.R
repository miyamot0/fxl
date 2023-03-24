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
  newlayer[["pch"]] <- pch
  newlayer[["color"]] <- color
  newlayer[["fill"]] <- fill
  newlayer[["cex"]] <- cex
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
