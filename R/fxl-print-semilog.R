#' print.fxlsemilog
#'
#' Override the final call to print the fxl object.
#' catches the obj and prints out layers in the sequence laid out by the user
#'
#' @param x fxlsemilog object
#' @param ... inherits from generic
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export print.fxlsemilog
#' @export
print.fxlsemilog <- function(x, ...) {
  x$dims[["min.local.x"]] <- min(
    x$data[[
      as.character(x$aes["x"])]],
    na.rm = TRUE)

  x$dims[["max.local.x"]] <- max(
    x$data[[
      as.character(x$aes["x"])]],
    na.rm = TRUE)

  # X Overrides

  if (!is.null(x$dims[["global.min.x"]]))
    x$dims[["min.local.x"]] <- x$dims[["global.min.x"]]

  if (!is.null(x$dims[["global.max.x"]]))
    x$dims[["max.local.x"]] <- x$dims[["global.max.x"]]

  # X axis
  x_axis_ticks <- seq(x$dims[["global.min.x"]],
                      x$dims[["global.max.x"]],
                      by = x$dims[["xdelta"]])

  if (!is.null(x$dims[["xticks"]]) && !is.list(x$dims[["xticks"]])) {
    x_axis_ticks <- as.integer(x$dims[["xticks"]])
  }

  # X axes
  x_axis_draw  <- TRUE

  # Y axis

  x$dims[["min.local.y"]] <- ifelse(is.null(x$dims[["global.min.y"]]),
                                             min(x$data[[as.character(x$aes["y"])]]),
                                             x$dims[["global.min.y"]])
  x$dims[["max.local.y"]] <- ifelse(is.null(x$dims[["global.min.y"]]),
                                             max(x$data[[as.character(x$aes["y"])]]),
                                             x$dims[["global.max.y"]])

  # Hack:
  x$dims[["min.local.y"]] <- 0.1

  par(family = "serif",
      omi    = x[["dims"]][["omi"]],
      mai    = x[["dims"]][["mai"]],
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
       ylim = c(x$dims[["min.local.y"]],
                x$dims[["max.local.y"]]),
       xlim = c(x$dims[["min.local.x"]],
                x$dims[["max.local.x"]]),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       frame.plot = FALSE,
       log = "y",
       las = 1)

  mtext(x$labs[["title"]],
        side = 3,
        outer = TRUE,
        adj = 0.04,
        line = 0)

  breaks  <- as.vector(c(2:10) %o% 10^(log10(x$dims[["min.local.y"]]):log10(x$dims[["max.local.y"]])))

  label_logicals <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
  labels <- as.character(breaks * label_logicals)
  labels <- gsub("^0$", "", labels)

  axis(1,
       at     = x_axis_ticks,
       labels = NA)

  axis(2,
       at = c(0.1,
              as.vector(c(1) %o%
                          10^(
                            log10(
                              x$dims[["min.local.y"]]):log10(
                                x$dims[["max.local.y"]])))),
       las    = 1,
       tcl    = par("tcl"),
       labels = c(0.1,
                  as.vector(c(1) %o%
                              10^(
                                log10(
                                  x$dims[["min.local.y"]]):log10(
                                    x$dims[["max.local.y"]])))))

  abline(h = c(0.1, breaks),
         lty = 1,
         col = "cadetblue")

  abline(h = c(0.1,
               as.vector(c(1) %o%
                           10^(
                             log10(
                               x$dims[["min.local.y"]]):log10(
                                 x$dims[["max.local.y"]])))),
         lty = 1,
         col = "darkblue")

  abline(h = c(0.1,
               as.vector(c(5) %o%
                           10^(
                             log10(
                               x$dims[["min.local.y"]]):log10(
                                 x$dims[["max.local.y"]])))),
         lty = 3,
         col = "darkblue")

  abline(v   = x_axis_ticks,
         lty = 1,
         col = "cadetblue")

  if (length(x[["layers"]]) > 0) {
    for (i in seq_len(length(x[["layers"]]))) {

      current_layer <- x$layers[[i]]
      current_layer$facet <- "hack"

      if (current_layer$type == "arrows")      draw_arrows(x,
                                                           current_layer,
                                                           "hack")
      if (current_layer$type == "brackets")    draw_brackets(x,
                                                             current_layer,
                                                             "hack")
      if (current_layer$type == "guide_line")  draw_guide_line(x,
                                                               current_layer,
                                                               "hack")
      if (current_layer$type == "line") {
        draw_lines(x, current_layer, NA)
      }

      if (current_layer$type == "phase_label") draw_label_phase(x,
                                                                current_layer,
                                                                "hack")
      if (current_layer$type == "point") {
        draw_points(x, current_layer, NA)
      }
    }
  }

  box(bty = "l")

  if (!is.null(x$dims[["xticklabs"]]) &&
      !is.list(x$dims[["xticklabs"]]) &&
      x_axis_draw) {

    x_axis_draw <- x$dims[["xticklabs"]]
  }

  if (!is.null(x[["legendpars"]]))  draw_legend(x)

  par(
    omi    = c(0.2, 0.25, 0.1, 0.25),
    mai    = c(0.4, 0.35, 0.1, 0.25),
    xaxs   = "r",
    yaxs   = "r",
    xpd    = FALSE,
    new    = TRUE)

  plot(NULL,
       ylim = c(0, 0),
       xlim = c(x$dims[["min.local.x"]],
                x$dims[["max.local.x"]]),
       ylab = "",
       xlab = "",
       xaxt = "n",
       yaxt = "n",
       frame.plot = FALSE,
       las = 1)

  axis(1,
       labels = x_axis_draw,
       at     = x_axis_ticks,
       pos = 0)

  axis(2,
       labels = c(0),
       las    = 1,
       tcl    = 0,
       at     = c(0))

  abline(h = 0,
         lty = 1,
         col = "black")

  if (length(x[["layers"]]) > 0)
    for (i in seq_len(length(x[["layers"]])))
      if (x$layers[[i]]$type == "point")
        draw_points(x, x$layers[[i]], NA, zero_axis = TRUE)

  mtext(x$labs[["ylab"]],
        side = 2,
        outer = TRUE)

  mtext(x$labs[["xlab"]],
        side = 1,
        outer = TRUE)
}
