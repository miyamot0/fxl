#' xlabel
#'
#' Override the x axis label
#'
#' @param core_frame fxl object
#' @param var string
#' @param line line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_xlabel <- function(core_frame,
                       var,
                       line = 0) {

  core_frame$labs[["xlab"]] <- {{ var }}
  core_frame$labs[["outer.x.line"]] <- line

  core_frame
}
