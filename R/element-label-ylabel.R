#' ylabel
#'
#' Override the y axis label
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
scr_ylabel <- function(core_frame,
                       var,
                       color = "black",
                       cex = 1,
                       adj = 0.5,
                       face = 1,
                       line = 0) {
  core_frame$labs[["ylab"]] <- {{ var }}
  core_frame$labs[["ylab_color"]] <- color
  core_frame$labs[["ylab_cex"]] <- cex
  core_frame$labs[["ylab_adj"]] <- adj
  core_frame$labs[["ylab_face"]] <- face

  isValidNumericVector(
    object = line,
    name = "line"
  )

  core_frame$labs[["outer.y.line"]] <- line

  core_frame
}
