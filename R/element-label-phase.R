#' scr_label_phase
#'
#' labels to be drawn on plots (typically for phases/conditions, but not necessarily)
#'
#' @param core_frame fxl object
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param facet facet of interest
#' @param labels as stated
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_label_phase <- function(core_frame,
                            color  = "black",
                            cex    = 1,
                            adj    = 0.5,
                            x      = NULL,
                            y      = NULL,
                            facet  = NULL,
                            labels = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "phase_label"
  newlayer[["color"]] <- color
  newlayer[["cex"]] <- cex
  newlayer[["adj"]] <- adj
  newlayer[["x"]] <- x
  newlayer[["y"]] <- y
  newlayer[["facet"]] <- facet
  newlayer[["labels"]] <- labels

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}