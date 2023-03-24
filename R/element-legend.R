#' scrlegend
#'
#' Information for drawing legend onto plots
#'
#' @param core_frame fxl object
#' @param panel facet to be drawn on
#' @param legend from base
#' @param col from base
#' @param lty from base
#' @param pch from base
#' @param box_lty from base
#' @param bty from base
#' @param cex from base
#' @param horiz from base
#' @param position from base
#' @param pt_cex from base
#' @param text_col from base
#' @param bg from base
#' @param pt_bg color, for point
#' @param adj alignment
#' @param border border status (from base)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_legend <- function(core_frame,
                       panel = NA,
                       legend,
                       bg = NULL,
                       col = NULL,
                       pt_bg = NULL,
                       lty,
                       pch,
                       box_lty = 0,
                       adj = c(0, 0.5),
                       bty = "n",
                       cex = 1,
                       horiz = FALSE,
                       position = "topright",
                       pt_cex = 1,
                       text_col = "black",
                       border = "black") {
  # TODO: lots to add

  core_frame$legendpars <- list()
  core_frame$legendpars[["panel"]] <- panel
  core_frame$legendpars[["adj"]] <- adj
  core_frame$legendpars[["legend"]] <- legend
  core_frame$legendpars[["col"]] <- col
  core_frame$legendpars[["bg"]] <- bg
  core_frame$legendpars[["pt.bg"]] <- pt_bg
  core_frame$legendpars[["border"]] <- border
  core_frame$legendpars[["lty"]] <- lty
  core_frame$legendpars[["pch"]] <- pch
  core_frame$legendpars[["bty"]] <- bty
  core_frame$legendpars[["box.lty"]] <- box_lty
  core_frame$legendpars[["cex"]] <- cex
  core_frame$legendpars[["horiz"]] <- horiz
  core_frame$legendpars[["position"]] <- position
  core_frame$legendpars[["pt.cex"]] <- pt_cex
  core_frame$legendpars[["text.col"]] <- text_col

  core_frame
}
