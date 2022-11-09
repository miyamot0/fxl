#' scrtitle
#'
#' Override the title
#'
#' @param core_frame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_title <- function(core_frame,
                      var) {

  core_frame$labs[["title"]] <- {{ var }}

  core_frame
}
