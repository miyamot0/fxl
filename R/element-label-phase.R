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
                            color = "black",
                            cex = 1,
                            adj = 0.5,
                            face = 1,
                            x = NULL,
                            y = NULL,
                            facet = NULL,
                            labels = NULL) {
  newlayer <- list()
  newlayer[["type"]] <- "phase_label"

  assert_input_type(color, "character", "color")
  newlayer[["color"]] <- color

  assert_input_type(cex, "numeric", "cex")
  newlayer[["cex"]] <- cex

  assert_input_type(adj, "numeric", "adj")
  newlayer[["adj"]] <- adj

  assert_input_type(face, "numeric", "face")
  newlayer[["face"]] <- face

  if (!is.null(x)) assert_input_type(x, "numeric", "x")
  newlayer[["x"]] <- x

  if (!is.null(y)) assert_input_type(y, "numeric", "y")
  newlayer[["y"]] <- y

  if (!is.null(facet)) assert_input_type(facet, "character", "facet")
  newlayer[["facet"]] <- facet

  # TODO: need to handle vector AND named lists
  if (!is.list(labels)) {
    isValidCharacterVector(
      object = labels,
      name = "labels"
    )
  }

  newlayer[["labels"]] <- labels

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
