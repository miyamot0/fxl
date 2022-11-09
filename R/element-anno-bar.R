#' scr_bar_support
#'
#' Adds a supplemental bar to the figure, if relevant to the data
#'
#' @param core_frame fxl object
#' @param color from base
#' @param alpha from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @return
#' @export
scr_bar_support <- function(core_frame,
                            color = rgb(.8, .8, .8,
                                        alpha = 0.25),
                            alpha = 1,
                            mapping = NULL,
                            label = "",
                            width = 0.8) {

  newlayer <- list()
  newlayer[["type"]] <- "bar_support"
  newlayer[["alpha"]] <- alpha
  newlayer[["color"]] <- color
  newlayer[["label"]] <- label
  newlayer[["width"]] <- width
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}
