#' ylabel
#'
#' Override the y axis label
#'
#' @param core_frame fxl object
#' @param var string
#' @param line line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_ylabel <- function(core_frame,
                       var,
                       line = 0) {

  core_frame$labs[["ylab"]] <- {{ var }}
  core_frame$labs[["outer.y.line"]] <- line

  core_frame
}
