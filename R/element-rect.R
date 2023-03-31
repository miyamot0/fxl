#' scr_rect
#'
#' @param core_frame fxl object
#' @param x1 from base
#' @param x2 from base
#' @param y1 from base
#' @param y2 from base
#' @param facet from base
#' @param color from base
#' @param fill from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
#' @importFrom rlang enexprs
scr_rect <- function(core_frame,
                     x1 = NA,
                     x2 = NA,
                     y1 = NA,
                     y2 = NA,
                     facet = NA,
                     color = "black",
                     fill = "black") {
  newlayer <- list()
  newlayer[["type"]] <- "rectangle"
  newlayer[["color"]] <- color
  newlayer[["fill"]] <- fill
  newlayer[["facet"]] <- facet
  newlayer[["coords"]] <- list(
    x1 = x1,
    x2 = ifelse(is.na(x2), x1, x2),
    y1 = y1,
    y2 = ifelse(is.na(y2), y1, y2)
  )

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}
