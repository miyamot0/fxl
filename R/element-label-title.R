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
#' @returns nothing, run for side effects
#'
#' @export
scr_title <- function(core_frame,
                      var,
                      color = "black",
                      cex = 1,
                      adj = 0.5,
                      face = 1) {

  assert_input_type({{ var }}, "character", "title")
  core_frame$labs[["title"]] <- {{ var }}

  assert_input_type(color, "character", "color")
  core_frame$labs[["title_color"]] <- color

  assert_input_type(cex, "numeric", "cex")
  core_frame$labs[["title_cex"]] <- cex

  assert_input_type(adj, "numeric", "adj")
  core_frame$labs[["title_adj"]] <- adj

  assert_input_type(face, "numeric", "face")
  core_frame$labs[["title_face"]] <- face

  core_frame
}
