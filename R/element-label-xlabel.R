#' xlabel
#'
#' Override the x axis label
#'
#' @param core_frame fxl object
#' @param var string
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param face like 'font' from base
#' @param line line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_xlabel <- function(core_frame,
                       var,
                       color = "black",
                       cex = 1,
                       adj = 0.5,
                       face = 1,
                       line = 0) {
  core_frame$labs[["xlab"]] <- {{ var }}
  core_frame$labs[["xlab_color"]] <- color
  core_frame$labs[["xlab_cex"]] <- cex
  core_frame$labs[["xlab_adj"]] <- adj
  core_frame$labs[["xlab_face"]] <- face

  isValidNumericVector(
    object = line,
    name = "line"
  )

  core_frame$labs[["outer.x.line"]] <- line

  core_frame
}
