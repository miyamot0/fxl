#' scr_label_facet
#'
#' @param core_frame fxl object
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param face like 'font' from base
#' @param x global x position for labels
#' @param y global y position for labels
#' @param labels as stated
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns nothing, run for side effects
#'
#' @export
scr_label_facet <- function(core_frame,
                            color = "black",
                            cex = 1,
                            adj = 0.5,
                            face = 1,
                            x = NULL,
                            y = NULL,
                            labels = NULL) {
  newlayer <- list()
  newlayer[["type"]] <- "facet_label"

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

  if (!is.list(labels)) {
    assert_input_type(labels, "character", "labels")
  }

  newlayer[["labels"]] <- labels

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
