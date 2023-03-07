#' scr_bar_support
#'
#' Adds a supplemental bar to the figure, if relevant to the data
#'
#' @param core_frame fxl object
#' @param color from base
#' @param alpha from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#' @param label description for bar
#' @param width width of bar
#'
#' @export
scr_bar_support <- function(core_frame,
                            color = rgb(.8, .8, .8,
                                        alpha = 0.25),
                            alpha = 1,
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

  if (!missing(mapping))  newlayer[["aesthetics"]] <- rlang::enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}
