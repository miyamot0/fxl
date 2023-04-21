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
#' @param styler a lambda function that returns dynamic styling parameters
#' @param width width of bar
#'
#' @returns a layer to the core plotting object
#'
#' @export
#' @importFrom rlang enexprs
scr_bar_support <- function(core_frame,
                            color = rgb(.8, .8, .8,
                              alpha = 0.25
                            ),
                            alpha = 1,
                            guide_line = NULL,
                            guide_line_type = 1,
                            guide_line_size = 1,
                            guide_line_color = "black",
                            mapping = NULL,
                            label = "",
                            styler = NA,
                            width = 0.8) {

  newlayer <- list()
  newlayer[["type"]] <- "bar_support"

  # TODO: error check
  newlayer[["guide_line"]] <- guide_line

  assert_input_type(alpha, "numeric", "alpha")
  newlayer[["alpha"]] <- alpha

  assert_input_type(color, "character", "color")
  newlayer[["color"]] <- color

  assert_input_type(guide_line_type, "numeric", "guide_line_type")
  newlayer[["guide_line_type"]] <- guide_line_type

  assert_input_type(guide_line_size, "numeric", "guide_line_size")
  newlayer[["guide_line_size"]] <- guide_line_size

  assert_input_type(guide_line_color, "character", "guide_line_color")
  newlayer[["guide_line_color"]] <- guide_line_color

  assert_input_type(label, "character", "label")
  newlayer[["label"]] <- label

  assert_input_type(width, "numeric", "width")
  newlayer[["width"]] <- width

  # TODO: error check
  newlayer[["styler"]] <- styler

  # TODO: error check
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
