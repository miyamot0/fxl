#' scr_cumsum
#'
#' Draw lines, but as a cumulative and rolling sum
#'
#' @param core_frame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param mapping from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_cumsum_lines <- function(core_frame,
                             lty = 1,
                             color = "black",
                             size = 1,
                             mapping) {

  newlayer <- list()
  newlayer[["type"]] <- "cum_sum_lines"
  newlayer[["lty"]] <- lty
  newlayer[["color"]] <- color
  newlayer[["size"]] <- size
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}