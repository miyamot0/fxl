#' scr_bar_support
#'
#' Adds a supplemental bar to the figure, if relevant to the data
#'
#' @param core_frame fxl object
#' @param color from base
#' @param alpha from base
#' @param guide_line (optional) aim line for bars
#' @param guide_line_type (optional) aim line type for bars
#' @param guide_line_size (optional) aim line size for bars
#' @param guide_line_color (optional) aim line color for bars
#' @param mapping (optional) if overriding draw (i.e., different response)
#' @param label description for bar
#' @param width width of bar
#'
#' @export
#' @importFrom rlang enexprs
scr_bar_support <- function(core_frame,
                            color = rgb(.8, .8, .8,
                                        alpha = 0.25),
                            alpha = 1,
                            guide_line = NULL,
                            guide_line_type = 1,
                            guide_line_size = 1,
                            guide_line_color = 'black',
                            mapping = NULL,
                            label = "",
                            width = 0.8) {

  #TODO: Tests for remaining params

  newlayer <- list()
  newlayer[["type"]] <- "bar_support"

  isValidNumericVector(
    object = alpha,
    name = "alpha"
  )

  newlayer[["alpha"]] <- alpha
  newlayer[["color"]] <- color
  newlayer[["guide_line"]] <- guide_line
  newlayer[["guide_line_type"]] <- guide_line_type
  newlayer[["guide_line_size"]] <- guide_line_size
  newlayer[["guide_line_color"]] <- guide_line_color

  isValidCharacterVector(
    object = label,
    name = "label"
  )

  newlayer[["label"]] <- label

  isValidNumericVector(
    object = width,
    name = "width"
  )

  newlayer[["width"]] <- width
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}
