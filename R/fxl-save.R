#' scrsave
#'
#' Function for outputting fxl object at preset size (certain journal are opinionated on size, format, and density)
#'
#' @param core_frame fxl object
#' @param units from base
#' @param name from base
#' @param width from base
#' @param height from base
#' @param res from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_save <- function(core_frame,
                     units = "in",
                     name = "test.tiff",
                     format = "tiff",
                     width = 8,
                     height = 4,
                     res = 600) {

  if (format == "tiff") {
    tiff(name,
         units = units,
         width = width,
         height = height,
         res = res)
  } else if (format == "png") {
    png(name,
        units = units,
        width = width,
        height = height,
        res = res)
  } else if (format == "svg") {
    svg(name,
        width = width,
        height = height)
  } else {
    pdf(name,
        width = width,
        height = height)
  }

  print(core_frame)

  dev.off()

  core_frame
}
