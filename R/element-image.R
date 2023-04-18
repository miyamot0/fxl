#' scr_images
#'
#' @param core_frame fxl object
#' @param image should be RGML image
#' @param cex from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
#' @importFrom rlang enexprs
scr_images <- function(core_frame,
                       image,
                       cex = 1,
                       mapping) {
  if (missing(image)) {
    stop("Must specify an image to draw.")
  }

  newlayer <- list()
  newlayer[["type"]] <- "image"

  # TODO: check for custom objects
  newlayer[["image"]] <- image

  assert_input_type(cex, "numeric", "cex")
  newlayer[["cex"]] <- cex

  # TODO: check for custom objects
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
