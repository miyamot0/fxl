#' print.fxlsemilog
#'
#' Override the final call to print the fxl object.
#' catches the obj and prints out layers in the sequence laid out by the user
#'
#' @param core_frame fxlsemilog object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export print.fxl
#' @export
print.fxlsemilog <- function(core_frame, ...) {
  core_frame$dims[["min.local.x"]] <- min(
    core_frame$data[[
      as.character(core_frame$aes["x"])]],
    na.rm = TRUE)

  core_frame$dims[["max.local.x"]] <- max(
    core_frame$data[[
      as.character(core_frame$aes["x"])]],
    na.rm = TRUE)

  # X Overrides

  if (!is.null(core_frame$dims[["global.min.x"]]))
    core_frame$dims[["min.local.x"]] <- core_frame$dims[["global.min.x"]]

  if (!is.null(core_frame$dims[["global.max.x"]]))
    core_frame$dims[["max.local.x"]] <- core_frame$dims[["global.max.x"]]

  # X axis
  x_axis_ticks <- seq(core_frame$dims[["global.min.x"]],
                      core_frame$dims[["global.max.x"]],
                      by = core_frame$dims[["xdelta"]])

  if (!is.null(core_frame$dims[["xticks"]]) && !is.list(core_frame$dims[["xticks"]])) {
    x_axis_ticks <- as.integer(core_frame$dims[["xticks"]])
  }

  # Y axis

  core_frame$dims[["min.local.y"]] <- ifelse(is.null(core_frame$dims[["global.min.y"]]),
                                             min(core_frame$data[[as.character(core_frame$aes["y"])]]),
                                             core_frame$dims[["global.min.y"]])
  core_frame$dims[["max.local.y"]] <- ifelse(is.null(core_frame$dims[["global.min.y"]]),
                                             max(core_frame$data[[as.character(core_frame$aes["y"])]]),
                                             core_frame$dims[["global.max.y"]])

  # Hack:
  core_frame$dims[["min.local.y"]] <- 0.1

  par(family = "serif",
      omi    = core_frame[["dims"]][["omi"]],
      mai    = core_frame[["dims"]][["mai"]],
      xaxs   = "r",
      yaxs   = "r",
      xpd    = FALSE)

  # Set layouts
  layout(matrix(c(1, 1, 1, 1, 1, 2),
                nrow = 6,
                ncol = 1,
                byrow = TRUE))

  # Top plot
  plot(NULL,
       ylim = c(core_frame$dims[["min.local.y"]],
                core_frame$dims[["max.local.y"]]),
       xlim = c(core_frame$dims[["min.local.x"]],
                core_frame$dims[["max.local.x"]]),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       frame.plot = FALSE,
       log = "y",
       las = 1)

  mtext(core_frame$labs[["title"]],
        side = 3,
        outer = TRUE,
        adj = 0.04,
        line = 0)

  breaks  <- as.vector(c(2:10) %o% 10^(log10(core_frame$dims[["min.local.y"]]):log10(core_frame$dims[["max.local.y"]])))

  label_logicals <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  labels <- as.character(breaks * label_logicals)
  labels <- gsub("^0$", "", labels)

  axis(1,
       at     = x_axis_ticks,
       labels = NA)

  # TODO: remove hard codes?
  axis(2,
       at     = c(0.1, breaks),
       las    = 1,
       tcl    = par("tcl") * 0.33,
       labels = c("0.1", labels))
  axis(2,
       at = c(0.1,
              as.vector(c(1) %o%
                          10^(
                            log10(
                              core_frame$dims[["min.local.y"]]):log10(
                                core_frame$dims[["max.local.y"]])))),
       las    = 1,
       tcl    = par("tcl"),
       labels = c(0.1,
                  as.vector(c(1) %o%
                              10^(
                                log10(
                                  core_frame$dims[["min.local.y"]]):log10(
                                    core_frame$dims[["max.local.y"]])))))

  abline(h = c(0.1, breaks),
         lty = 1,
         col = "cadetblue")

  abline(h = c(0.1,
               as.vector(c(1) %o%
                           10^(
                             log10(
                               core_frame$dims[["min.local.y"]]):log10(
                                 core_frame$dims[["max.local.y"]])))),
         lty = 1,
         col = "darkblue")

  abline(h = c(0.1,
               as.vector(c(5) %o%
                           10^(
                             log10(
                               core_frame$dims[["min.local.y"]]):log10(
                                 core_frame$dims[["max.local.y"]])))),
         lty = 3,
         col = "darkblue")

  abline(v   = core_frame$dims[["min.local.x"]]:core_frame$dims[["max.local.x"]],
         lty = 1,
         col = "cadetblue")

  if (length(core_frame[["layers"]]) > 0) {
    for (i in seq_len(length(core_frame[["layers"]]))) {

      current_layer <- core_frame$layers[[i]]
      current_layer$facet <- "hack"

      if (current_layer$type == "arrows")      draw_arrows(core_frame,
                                                           current_layer,
                                                           "hack")
      if (current_layer$type == "brackets")    draw_brackets(core_frame,
                                                             current_layer,
                                                             "hack")
      if (current_layer$type == "guide_line")  draw_guide_line(core_frame,
                                                               current_layer,
                                                               "hack")
      if (current_layer$type == "line")        draw_lines(core_frame,
                                                          current_layer,
                                                          NA)
      if (current_layer$type == "phase_label") draw_label_phase(core_frame,
                                                                current_layer,
                                                                "hack")
      if (current_layer$type == "point")       draw_points(core_frame,
                                                           current_layer,
                                                           NA)
    }
  }

  box(bty = "l")

  if (!is.null(core_frame[["legendpars"]]))  draw_legend(core_frame)

  par(
    omi    = c(0.2, 0.25, 0.1, 0.25),
    mai    = c(0.4, 0.35, 0.1, 0.25),
    xaxs   = "r",
    yaxs   = "r",
    xpd    = FALSE,
    new    = TRUE)

  plot(NULL,
       ylim = c(0, 0),
       xlim = c(core_frame$dims[["min.local.x"]],
                core_frame$dims[["max.local.x"]]),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       frame.plot = FALSE,
       las = 1)

  axis(1,
       labels = core_frame$dims[["min.local.x"]]:core_frame$dims[["max.local.x"]],
       at     = core_frame$dims[["min.local.x"]]:core_frame$dims[["max.local.x"]],
       pos = 0)

  axis(2,
       labels = c(0),
       las    = 1,
       tcl    = 0,
       at     = c(0))

  abline(h = 0,
         lty = 1,
         col = "black")

  if (length(core_frame[["layers"]]) > 0)
    for (i in seq_along(length(core_frame[["layers"]])))
      if (core_frame$layers[[i]]$type == "point")
        draw_points(core_frame, core_frame$layers[[i]], NA, zero_axis = TRUE)

  mtext(core_frame$labs[["ylab"]],
        side = 2,
        outer = TRUE)

  mtext(core_frame$labs[["xlab"]],
        side = 1,
        outer = TRUE)
}
