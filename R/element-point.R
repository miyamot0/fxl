#' scr_points
#'
#' @param core_frame fxl object
#' @param pch from base
#' @param color from base
#' @param fill from base
#' @param cex from base
#' @param styler a lambda function that returns dynamic styling parameters
#' @param data (optional) if overriding data
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
#' @importFrom rlang enexprs
scr_points <- function(core_frame,
                       pch = 21,
                       color = "black",
                       fill = "black",
                       cex = 1,
                       styler = NA,
                       data = NA,
                       mapping) {
  newlayer <- list()
  newlayer[["type"]] <- "point"
  newlayer[["pch"]] <- pch
  newlayer[["color"]] <- color
  newlayer[["fill"]] <- fill
  newlayer[["cex"]] <- cex
  newlayer[["styler"]] <- styler
  newlayer[["data"]] <- data
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
