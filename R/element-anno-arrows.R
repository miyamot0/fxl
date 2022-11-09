#' scr_arrows
#'
#' Add a layer with arrows to direct attention on the plot
#'
#' Generally useful for avoiding a legend
#'
#' @param core_frame fxl class
#' @param arrows list of keyed entries to be drawn on respective facets
#' @param facet the facet which will be drawn upon
#' @param color from base
#' @param length from base
#' @param angle from base
#' @param code from base
#' @param lwd from base
#' @param lty from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_arrows <- function(core_frame,
                       arrows = NULL,
                       facet = NULL,
                       color = "black",
                       length = 0.25,
                       angle = 30,
                       code = 2,
                       lwd = 1,
                       lty = 1) {

  newlayer <- list()
  newlayer[["type"]] <- "arrows"
  newlayer[["facet"]] <- facet
  newlayer[["arrows"]] <- arrows
  newlayer[["color"]] <- color
  newlayer[["length"]] <- length
  newlayer[["angle"]] <- angle
  newlayer[["code"]] <- code
  newlayer[["lwd"]] <- lwd
  newlayer[["lty"]] <- lty

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
