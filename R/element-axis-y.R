#' yoverride
#'
#' Override the y axis (or axes) limits
#'
#' @param core_frame fxl object
#' @param var from base
#' @param ydelta skips between ticks (can override)
#' @param ydraws specify axes manual
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_yoverride <- function(core_frame,
                          var,
                          ydelta = 1,
                          yticks = NULL,
                          ydraws = NULL,
                          ytickslabs = NULL) {

  # Check if a vector and not multi-facet list
  if (is.vector(var) && !is.list(var)) {
    core_frame$dims[["global.min.y"]] <- {{var[1]}}
    core_frame$dims[["global.max.y"]] <- {{var[2]}}

  } else {
    core_frame$dims[["local.dims"]] <- var
  }

  core_frame$dims[["ydelta"]] <- ydelta
  core_frame$dims[["yticks"]] <- yticks
  core_frame$dims[["ydraws"]] <- ydraws
  core_frame$dims[["yticklabs"]] <- ytickslabs

  core_frame
}
