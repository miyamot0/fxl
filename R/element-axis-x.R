#' xoverride
#'
#' Override the x axis limits
#'
#' @param core_frame fxl object
#' @param var string for title
#' @param xdelta skips between ticks (can override)
#' @param xticks specify ticks, vector or named list
#' @param xdraws which x axes to draw
#' @param xtickslabs custom x axis labels
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_xoverride <- function(core_frame,
                          var,
                          xdelta = 1,
                          xticks = NULL,
                          xdraws = NULL,
                          xtickslabs = NULL) {

  core_frame$dims[["global.min.x"]] <- {{var[1]}}
  core_frame$dims[["global.max.x"]] <- {{var[2]}}
  core_frame$dims[["xdelta"]] <- xdelta
  core_frame$dims[["xticks"]] <- xticks
  core_frame$dims[["xdraws"]] <- xdraws
  core_frame$dims[["xticklabs"]] <- xtickslabs

  core_frame
}
