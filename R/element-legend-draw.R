#' draw_legend
#'
#' drawing function
#'
#' @param core_frame fxl object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
draw_legend <- function(core_frame) {

  if (is.list(core_frame$legendpars[["position"]])) {

    legend(
      x = core_frame$legendpars[["position"]]$x,
      y = core_frame$legendpars[["position"]]$y,

      legend    = as.character(core_frame$legendpars[["legend"]]),
      adj       = as.numeric(core_frame$legendpars[["adj"]]),
      text.col  = as.character(core_frame$legendpars[["text.col"]]),
      lty       = as.numeric(core_frame$legendpars[["lty"]]),
      box.lty   = as.numeric(core_frame$legendpars[["box.lty"]]),
      pch       = as.numeric(core_frame$legendpars[["pch"]]),
      border    = as.numeric(core_frame$legendpars[["border"]]),
      bty       = as.character(core_frame$legendpars[["bty"]]),
      pt.cex    = as.numeric(core_frame$legendpars[["pt.cex"]]),
      cex       = as.numeric(core_frame$legendpars[["cex"]]),
      bg        = as.character(core_frame$legendpars[["bg"]]),
      col       = as.character(core_frame$legendpars[["col"]]),
      pt.bg     = as.character(core_frame$legendpars[["pt.bg"]]),
      horiz     = as.logical(core_frame$legendpars[["horiz"]])
    )

    return(NA)
  }

  legend(
    core_frame$legendpars[["position"]],

    legend    = as.character(core_frame$legendpars[["legend"]]),
    adj       = as.numeric(core_frame$legendpars[["adj"]]),
    text.col  = as.character(core_frame$legendpars[["text.col"]]),
    lty       = as.numeric(core_frame$legendpars[["lty"]]),
    box.lty   = as.numeric(core_frame$legendpars[["box.lty"]]),
    pch       = as.numeric(core_frame$legendpars[["pch"]]),
    bty       = as.character(core_frame$legendpars[["bty"]]),
    pt.cex    = as.numeric(core_frame$legendpars[["pt.cex"]]),
    cex       = as.numeric(core_frame$legendpars[["cex"]]),
    bg        = as.character(core_frame$legendpars[["bg"]]),
    col       = as.character(core_frame$legendpars[["col"]]),
    pt.bg     = as.character(core_frame$legendpars[["pt.bg"]]),
    horiz     = as.logical(core_frame$legendpars[["horiz"]])
  )
}
