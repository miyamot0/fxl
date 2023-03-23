#' scr_label_phase
#'
#' labels to be drawn on plots (typically for phases/conditions, but not necessarily)
#'
#' @param core_frame fxl object
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param face like 'font' from base
#' @param facet facet of interest
#' @param labels as stated
#' @param x location
#' @param y location
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_label_phase <- function(core_frame,
                            color  = "black",
                            cex    = 1,
                            adj    = 0.5,
                            face   = 1,
                            x      = NULL,
                            y      = NULL,
                            facet  = NULL,
                            labels = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "phase_label"
  newlayer[["color"]] <- color

  isValidNumericVector(
    object = cex,
    name = "cex"
  )

  newlayer[["cex"]] <- cex

  isValidNumericVector(
    object = adj,
    name = "adj"
  )

  newlayer[["adj"]] <- adj

  isValidNumericVector(
    object = face,
    name = "face",
    length = 1
  )

  newlayer[["face"]]  <- face

  newlayer[["x"]] <- x
  newlayer[["y"]] <- y
  newlayer[["facet"]] <- facet
  newlayer[["labels"]] <- labels

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
