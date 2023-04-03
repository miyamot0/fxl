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
#' @param xrotation degree to rotate positioned labels
#' @param xtickscex expansion factor for labels
#' @param xlabeloffset offset to push labels downward
#' @param xtickslabs labels for x axis
#' @param xticksadj alignment for custom labels
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_xoverride <- function(core_frame,
                          var,
                          xdelta = 1,
                          xticks = NULL,
                          xdraws = NULL,
                          xrotation = NULL,
                          xtickscex = 1,
                          xlabeloffset = NULL,
                          xtickslabs = NULL,
                          xticksadj = 1) {
  if (is.null(var)) {
    stop(paste("scr_xoverride: var must not be set to null."))
  }

  # Check if a vector and not multi-facet list
  if (is.vector(var)) {
    isValidNumericVector(
      object = var,
      length = 2,
      name = "scr_xoverride"
    )

    core_frame$dims[["global.min.x"]] <- {{ var[1] }}
    core_frame$dims[["global.max.x"]] <- {{ var[2] }}
  }

  assert_input_type(xdelta, "numeric", "xdelta")
  core_frame$dims[["xdelta"]] <- xdelta

  if (!is.null(xticks) && is.numeric(xticks)) {
    isValidNumericVector(
      object = xticks,
      name = "xticks"
    )
  }

  core_frame$dims[["xticks"]] <- xticks

  if (!is.null(xtickslabs)) {
    isValidCharacterVector(
      object = xtickslabs,
      name = "xtickslabs"
    )
  }

  core_frame$dims[["xticklabs"]] <- xtickslabs

  if (!is.null(xdraws)) {
    isValidCharacterVector(
      object = xdraws,
      name = "xdraws"
    )
  }

  core_frame$dims[["xdraws"]] <- xdraws

  if (!is.null(xrotation)) assert_input_type(xrotation, "numeric", "xrotation")
  core_frame$dims[["xlab.rotation"]] <- xrotation

  if (!is.null(xlabeloffset))  assert_input_type(xlabeloffset, "numeric", "xlabeloffset")
  core_frame$dims[["xlab.offset"]] <- xlabeloffset

  if (!is.null(xtickscex))  assert_input_type(xtickscex, "numeric", "xtickscex")
  core_frame$dims[["xlab.cex"]] <- xtickscex

  if (!is.null(xticksadj))  assert_input_type(xticksadj, "numeric", "xticksadj")
  core_frame$dims[["xticklabs.offset"]] <- xticksadj

  core_frame
}
