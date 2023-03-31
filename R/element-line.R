#' scr_lines
#'
#' @param core_frame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param mapping from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
#' @importFrom rlang enexprs
scr_lines <- function(core_frame,
                      lty = 1,
                      color = "black",
                      size = 1,
                      mapping) {
  newlayer <- list()
  newlayer[["type"]] <- "line"

  isValidNumericVector(
    object = lty,
    name = "lty"
  )

  newlayer[["lty"]] <- lty
  newlayer[["color"]] <- color

  isValidNumericVector(
    object = size,
    name = "size"
  )

  newlayer[["size"]] <- size
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
