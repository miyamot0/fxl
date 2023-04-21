#' scrsave
#'
#' Function for outputting fxl object at preset size (certain journal are opinionated on size, format, and density)
#'
#' @importFrom graphics abline arrows axis box layout legend lines mtext par points rect segments text
#' @importFrom grDevices dev.off pdf png rgb svg tiff
#' @param core_frame fxl object
#' @param units from base
#' @param name from base
#' @param format type of image to save in
#' @param width from base
#' @param height from base
#' @param res from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns no return, executed for side effects
#'
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
      res = res
    )
  } else if (format == "png") {
    png(name,
      units = units,
      width = width,
      height = height,
      res = res
    )
  } else if (format == "svg") {
    svg(name,
      width = width,
      height = height
    )
  } else {
    pdf(name,
      width = width,
      height = height
    )
  }

  print(core_frame)

  dev.off()

  core_frame
}
