#' scrtitle
#'
#' Override the title
#'
#' @param core_frame fxl object
#' @param var string
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param face like 'font' from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_title <- function(core_frame,
                      var,
                      color = "black",
                      cex = 1,
                      adj = 0.5,
                      face = 1) {
  core_frame$labs[["title"]] <- {{ var }}
  core_frame$labs[["title_color"]] <- color
  core_frame$labs[["title_cex"]] <- cex
  core_frame$labs[["title_adj"]] <- adj
  core_frame$labs[["title_face"]] <- face

  core_frame
}
