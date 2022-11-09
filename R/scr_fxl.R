##
## Copyright 2021 Shawn Gilroy
##
## This file is part of fxl.
##
## fxl is free software: you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation, version 2.
##
## fxl is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License
## along with fxl  If not, see <http://www.gnu.org/licenses/gpl-2.0.html>.

library(rlang)
library(dplyr)

### EXPORTS ###

#' scrplot
#'
#' Core object for establishing fxl object and layers
#'
#' @param data submitted data (not opinionated on naming)
#' @param aesthetics references for data in frame
#' @param mai margins in inches
#' @param omi outer margins in inches
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_plot <- function(data, aesthetics,
                     mai     = c(0.375, 0.375, 0.25, 0.25),
                     omi     = c(0.25, 0.25, 0.25, 0.25),
                     xaxs    = "i",
                     yaxs    = "i",
                     ncol    = 1,
                     family  = NULL,
                     semilog = FALSE) {

  core_frame <- list()                             # Primary plotting object
  core_frame[["layers"]] <- list()              # Layers for drawing
  core_frame[["aes"]] <- enexpr(aesthetics)  # Mappings
  core_frame[["data"]] <- data                # Stored data
  core_frame[["dims"]] <- list(# Global dimensions
    global.max.x = max(data[[as.character(core_frame$aes["x"])]], na.rm = TRUE),
    global.min.x = min(data[[as.character(core_frame$aes["x"])]], na.rm = TRUE),
    global.max.y = max(data[[as.character(core_frame$aes["y"])]], na.rm = TRUE),
    global.min.y = min(data[[as.character(core_frame$aes["y"])]], na.rm = TRUE),
    mai          = mai,
    omi          = omi,
    xaxs         = xaxs,
    yaxs         = yaxs,
    ncol         = ncol,
    xdelta       = 1,
    ydelta       = 1)

  core_frame[["labs"]] <- list(# Presumed labels, blank title by default
    xlab         = as.character(core_frame$aes["x"]),
    ylab         = as.character(core_frame$aes["y"]),
    outer        = TRUE,
    outer.x.line = 0,
    outer.y.line = 0,
    title        = "")

  core_frame[["family"]] <- family

  class(core_frame) <- c("fxl")                   # Apply a class name (to override print)

  if (semilog) class(core_frame) <- c("fxlsemilog")

  core_frame
}

#' scr_arrows
#'
#' Add a layer with arrows on plot
#'
#' @param core_frame fxl class
#' @param arrows list of keyed entries to be drawn on respective facets
#' @param facet the facet which will be drawn upon
#' @param color from base
#' @param length from base
#' @param angle from base
#' @param code from base
#' @param lwd from base
#' @param lty from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_arrows <- function(core_frame,
                       arrows = NULL,
                       facet = NULL,
                       color = "black",
                       length = 0.25,
                       angle = 30,
                       code = 2,
                       lwd = 1,
                       lty = 1) {
  newlayer <- list()
  newlayer[["type"]] <- "arrows"
  newlayer[["facet"]] <- facet
  newlayer[["arrows"]] <- arrows
  newlayer[["color"]] <- color
  newlayer[["length"]] <- length
  newlayer[["angle"]] <- angle
  newlayer[["code"]] <- code
  newlayer[["lwd"]] <- lwd
  newlayer[["lty"]] <- lty

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_brackets
#'
#' Add a layer with brackets on plot
#'
#' @param core_frame fxl class
#' @param brackets list of keyed entries to be drawn on respective facets
#' @param facet the facet which will be drawn upon
#' @param color from base
#' @param length from base
#' @param angle from base
#' @param code from base
#' @param lwd from base
#' @param lty from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_brackets <- function(core_frame,
                         brackets = NULL,
                         facet = NULL,
                         color = "black",
                         length = 0.25,
                         angle = 30,
                         code = 2,
                         lwd = 1,
                         lty = 1) {

  newlayer <- list()
  newlayer[["type"]] <- "brackets"
  newlayer[["facet"]] <- facet
  newlayer[["brackets"]] <- brackets
  newlayer[["color"]] <- color
  newlayer[["length"]] <- length
  newlayer[["angle"]] <- angle
  newlayer[["code"]] <- code
  newlayer[["lwd"]] <- lwd
  newlayer[["lty"]] <- lty

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_bar_support
#'
#' Adds a supplemental bar to the figure, if relevant to the data
#'
#' @param core_frame fxl object
#' @param color from base
#' @param alpha from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @return
#' @export
scr_bar_support <- function(core_frame,
                            color = rgb(.8, .8, .8,
                                        alpha = 0.25),
                            alpha = 1,
                            mapping = NULL,
                            label = "",
                            width = 0.8) {

  newlayer <- list()
  newlayer[["type"]] <- "bar_support"
  newlayer[["alpha"]] <- alpha
  newlayer[["color"]] <- color
  newlayer[["label"]] <- label
  newlayer[["width"]] <- width
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}

#' scr_cumsum
#'
#' Draw lines, but as a cumulative and rolling sum
#'
#' @param core_frame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param mapping from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_cumsum_lines <- function(core_frame,
                             lty = 1,
                             color = "black",
                             size = 1,
                             mapping) {

  newlayer <- list()
  newlayer[["type"]] <- "cum_sum_lines"
  newlayer[["lty"]] <- lty
  newlayer[["color"]] <- color
  newlayer[["size"]] <- size
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}

#' scr_cumsum_points
#'
#' @param core_frame fxl object
#' @param pch from base
#' @param color from base
#' @param fill from base
#' @param cex from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_cumsum_points <- function(core_frame,
                              pch = 21,
                              color = "black",
                              fill = "black",
                              cex = 1,
                              mapping) {

  newlayer <- list()
  newlayer[["type"]] <- "cum_sum_points"
  newlayer[["pch"]] <- pch
  newlayer[["color"]] <- color
  newlayer[["fill"]] <- fill
  newlayer[["cex"]] <- cex
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame

}

#' scr_lines
#'
#' @param core_frame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param mapping from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_lines <- function(core_frame,
                      lty = 1,
                      color = "black",
                      size = 1,
                      mapping) {

  newlayer <- list()
  newlayer[["type"]] <- "line"
  newlayer[["lty"]] <- lty
  newlayer[["color"]] <- color
  newlayer[["size"]] <- size
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_label_phase
#'
#' labels to be drawn on plots (typically for phases/conditions, but not necessarily)
#'
#' @param core_frame fxl object
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param facet facet of interest
#' @param labels as stated
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_label_phase <- function(core_frame,
                            color  = "black",
                            cex    = 1,
                            adj    = 0.5,
                            x      = NULL,
                            y      = NULL,
                            facet  = NULL,
                            labels = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "phase_label"
  newlayer[["color"]] <- color
  newlayer[["cex"]] <- cex
  newlayer[["adj"]] <- adj
  newlayer[["x"]] <- x
  newlayer[["y"]] <- y
  newlayer[["facet"]] <- facet
  newlayer[["labels"]] <- labels

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_label_facet
#'
#' @param core_frame fxl object
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param x global x position for labels
#' @param y global y position for labels
#' @param labels as stated
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_label_facet <- function(core_frame,
                            color  = "black",
                            cex    = 1,
                            adj    = 0.5,
                            x      = NULL,
                            y      = NULL,
                            labels = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "facet_label"
  newlayer[["color"]] <- color
  newlayer[["cex"]] <- cex
  newlayer[["adj"]] <- adj
  newlayer[["x"]] <- x
  newlayer[["y"]] <- y
  newlayer[["labels"]] <- labels

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_guide_line
#'
#' This is an annotation illustrating an aim/reduction line
#'
#' @param core_frame fxl object
#' @param coords start and finish coords for aim line
#' @param color from base
#' @param lty line type
#' @param lwd line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_guide_line <- function(core_frame,
                           coords,
                           facet = NA,
                           color = "red",
                           lty = 1,
                           lwd = 1) {

  newlayer <- list()
  newlayer[["type"]] <- "guide_line"
  newlayer[["coords"]] <- coords
  newlayer[["col"]] <- color
  newlayer[["facet"]] <- facet
  newlayer[["lty"]] <- lty
  newlayer[["lwd"]] <- lwd

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_plines
#'
#' @param core_frame fxl object
#' @param lines phase lines to be drawn
#' @param lwd from base
#' @param lty from base
#' @param col from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_plines <- function(core_frame,
                       lines = NULL,
                       lwd = 1,
                       lty = 1,
                       col = "black") {

  newlayer <- list()
  newlayer[["type"]] <- "phase_lines"
  newlayer[["lines"]] <- lines
  newlayer[["lwd"]] <- lwd
  newlayer[["lty"]] <- lty
  newlayer[["col"]] <- col

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_plines_mbd
#'
#' @param core_frame fxl object
#' @param lines phase lines to be drawn
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_plines_mbd <- function(core_frame,
                           lines = NULL) {

  newlayer <- list()
  newlayer[["type"]] <- "mbd_phase_lines"
  newlayer[["lines"]] <- lines

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' scr_points
#'
#' @param core_frame fxl object
#' @param pch from base
#' @param color from base
#' @param fill from base
#' @param cex from base
#' @param mapping (optional) if overriding draw (i.e., different response)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_points <- function(core_frame,
                       pch = 21,
                       color = "black",
                       fill = "black",
                       cex = 1,
                       mapping) {

  newlayer <- list()
  newlayer[["type"]] <- "point"
  newlayer[["pch"]] <- pch
  newlayer[["color"]] <- color
  newlayer[["fill"]] <- fill
  newlayer[["cex"]] <- cex
  newlayer[["aesthetics"]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  core_frame$layers[[(length(core_frame[["layers"]]) + 1)]] <- newlayer

  core_frame
}

#' xlabel
#'
#' Override the x axis label
#'
#' @param core_frame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_xlabel <- function(core_frame,
                       var,
                       line = 0) {

  core_frame$labs[["xlab"]] <- {{ var }}
  core_frame$labs[["outer.x.line"]] <- line

  core_frame
}


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

#' ylabel
#'
#' Override the y axis label
#'
#' @param core_frame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_ylabel <- function(core_frame,
                       var,
                       line = 0) {

  core_frame$labs[["ylab"]] <- {{ var }}
  core_frame$labs[["outer.y.line"]] <- line

  core_frame
}


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
                          ydraws = NULL) {

  # Check if a vector and not multi-facet list
  if (is.vector(var) && !is.list(var)) {
    core_frame$dims[["global.min.y"]] <- {{var[1]}}
    core_frame$dims[["global.max.y"]] <- {{var[2]}}

  } else {
    core_frame$dims[["local.dims"]] <- var

  }

  core_frame$dims[["ydelta"]] <- ydelta
  core_frame$dims[["ydraws"]] <- ydraws

  core_frame
}

#' scrtitle
#'
#' Override the title
#'
#' @param core_frame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_title <- function(core_frame,
                      var) {

  core_frame$labs[["title"]] <- {{ var }}

  core_frame
}

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
#' @param box.lty from base
#' @param bty from base
#' @param cex from base
#' @param horiz from base
#' @param position from base
#' @param pt.cex from base
#' @param text.col from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
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

### OVERRIDES ###

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

#' print.fxl
#'
#' Override the final call to print the fxl object. catches the obj and
#' prints out layers in the sequence laid out by the user
#'
#' @param core_frame fxl object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export print.fxl
#' @export
print.fxl <- function(core_frame, ...) {

  # Holders for phase coords
  plot_tops <- list()
  plot_bots <- list()
  index_num <- list()

  facets <- NULL
  n_facets <- 1
  n_facets_draw <- n_facets
  n_cols <- 1
  lookup <- FALSE
  req_draw <- FALSE

  if ("facet" %in% names(core_frame$aes)) {
    facets <- unique(core_frame$data[[as.character(core_frame$aes["facet"])]])
    n_facets <- length(facets)
    n_facets_draw <- n_facets
    n_cols <- core_frame[["dims"]][["ncol"]]
    n_facets_draw <- as.integer(n_facets / n_cols)
    lookup <- TRUE
  }

  font_family <- ifelse(is.null(core_frame[["family"]]),
                       "serif",
                       core_frame[["family"]])

  par(mfrow  = c(n_facets_draw, n_cols), # Dynamic facet numbers/cols
      family = font_family,
      omi    = core_frame[["dims"]][["omi"]],
      mai    = core_frame[["dims"]][["mai"]],
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  print(paste("N Facets", n_facets))

  for (facetIndex in 1:n_facets) { # Print placeholders

    # Defaults, per data
    current_facet <- NA

    core_frame$dims[["min.local.x"]] <- min(
      core_frame$data[[as.character(core_frame$aes["x"])]],
      na.rm = TRUE)
    core_frame$dims[["max.local.x"]] <- max(
      core_frame$data[[as.character(core_frame$aes["x"])]],
      na.rm = TRUE)

    # Facet override
    if (lookup)  current_facet <- facets[facetIndex]

    # X overrides
    if (!is.null(core_frame$dims[["global.min.x"]]))
      core_frame$dims[["min.local.x"]] <- core_frame$dims[["global.min.x"]]

    if (!is.null(core_frame$dims[["global.max.x"]]))
      core_frame$dims[["max.local.x"]] <- core_frame$dims[["global.max.x"]]

    # X axes
    x_axis_draw  <- (facetIndex == n_facets)

    if (!is.null(core_frame$dims[["xdraws"]])) {
      x_axis_draw <-  current_facet %in% core_frame$dims[["xdraws"]]
    }

    x_axis_ticks <- seq(core_frame$dims[["global.min.x"]],
                       core_frame$dims[["global.max.x"]],
                       by = core_frame$dims[["xdelta"]])

    if (!is.null(core_frame$dims[["xticks"]]) && !is.list(
      core_frame$dims[["xticks"]])) {

      x_axis_ticks <- as.integer(core_frame$dims[["xticks"]])
    }

    if (!is.null(core_frame$dims[["xticks"]]) && is.list(
      core_frame$dims[["xticks"]])) {

      x_axis_ticks <- core_frame$dims[["xticks"]][[ current_facet]]

      core_frame$dims[["min.local.x"]] <- min(
        as.numeric(core_frame$dims[["xticks"]][[ current_facet]]))
      core_frame$dims[["max.local.x"]] <- max(
        as.numeric(core_frame$dims[["xticks"]][[ current_facet]]))
    }

    # Y axes

    y_axis_draw  <- TRUE

    if (!is.null(core_frame$dims[["ydraws"]])) {
      y_axis_draw <-  current_facet %in% core_frame$dims[["ydraws"]]
    }

    y_axis_ticks <- seq(core_frame$dims[["global.min.y"]],
                       core_frame$dims[["global.max.y"]],
                       by = core_frame$dims[["ydelta"]])

    if (!is.null(core_frame$dims[["local.dims"]])) {
      core_frame$dims[["min.local.y"]] <- core_frame$dims[["local.dims"]][[ current_facet]]$y0
      core_frame$dims[["max.local.y"]] <- core_frame$dims[["local.dims"]][[ current_facet]]$y1

      y_axis_ticks <- seq(core_frame$dims[["min.local.y"]],
                         core_frame$dims[["max.local.y"]],
                         by = core_frame$dims[["ydelta"]])

      if ("yticks" %in% names(core_frame$dims[["local.dims"]][[ current_facet]]))
        y_axis_ticks <- core_frame$dims[["local.dims"]][[ current_facet]]$yticks

    } else {
      core_frame$dims[["min.local.y"]] <- ifelse(is.null(
        core_frame$dims[["global.min.y"]]),
        min(core_frame$data[[as.character(core_frame$aes["y"])]]),
        core_frame$dims[["global.min.y"]])

      core_frame$dims[["max.local.y"]] <- ifelse(is.null(
        core_frame$dims[["global.min.y"]]),
        max(core_frame$data[[as.character(core_frame$aes["y"])]]),
        core_frame$dims[["global.max.y"]])

      y_axis_ticks <- seq(core_frame$dims[["min.local.y"]],
                         core_frame$dims[["max.local.y"]],
                         by = core_frame$dims[["ydelta"]])
    }

    plot(NULL,
         xlim = c(core_frame$dims[["min.local.x"]], core_frame$dims[["max.local.x"]]),
         ylim = c(core_frame$dims[["min.local.y"]], core_frame$dims[["max.local.y"]]),
         ylab = "",
         xlab = "",
         frame.plot = FALSE,
         las = 1,
         xaxt = "n",
         yaxt = "n")

    box(bty = "l")

    if (!is.null(core_frame$dims[["xticklabs"]]) &&
        !is.list(core_frame$dims[["xticklabs"]]) &&
        x_axis_draw) {
      x_axis_draw <- core_frame$dims[["xticklabs"]]
    }

    axis(1,
         labels = x_axis_draw,
         at     = x_axis_ticks)

    axis(2,
         labels = y_axis_draw,
         las    = 1,
         at     = y_axis_ticks)

    if (length(core_frame[["layers"]]) > 0) {
      for (i in seq_len(length(core_frame[["layers"]]))) {

        current_layer <- core_frame$layers[[i]]

        print(current_layer)

        if (current_layer$type == "arrows")
          draw_arrows(core_frame,
                      current_layer,
                      current_facet)

        if (current_layer$type == "brackets")
          draw_brackets(core_frame,
                        current_layer,
                        current_facet)

        if (current_layer$type == "bar_support")
          draw_bar_support(core_frame,
                           current_layer,
                           current_facet)

        if (current_layer$type == "cum_sum_lines")
          draw_cumsum_lines(core_frame,
                            current_layer,
                            current_facet)

        if (current_layer$type == "cum_sum_points")
          draw_cumsum_points(core_frame,
                             current_layer,
                             current_facet)

        if (current_layer$type == "facet_label")
          draw_label_facet(core_frame,
                           current_layer,
                           current_facet)

        if (current_layer$type == "guide_line")
          draw_guide_line(core_frame,
                          current_layer,
                          current_facet)

        if (current_layer$type == "line")
          draw_lines(core_frame,
                     current_layer,
                     current_facet)

        if (current_layer$type == "phase_label")
          draw_label_phase(core_frame,
                           current_layer,
                           current_facet)

        if (current_layer$type == "phase_lines")
          draw_scr_plines(core_frame,
                          current_layer,
                          current_facet)

        if (current_layer$type == "point")
          draw_points(core_frame,
                      current_layer,
                      current_facet)

        if (current_layer$type == "mbd_phase_lines") {

          plines <- names(current_layer$lines)

          for (pname in plines) {

            # start of index for named list
            if (!(pname %in% names(index_num))) index_num[[pname]] <- 1

            current_index <- which(
              names(current_layer$lines[[pname]]) ==  current_facet)

            if (length(current_index) == 0) next

            tmp_x1 <- current_layer$lines[[pname]][[current_index]][["x1"]]

            tmp_x2 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["x2"]]),
              tmp_x1,
              current_layer$lines[[pname]][[current_index]][["x2"]])

            tmp_y1 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["y1"]]),
              0,
              current_layer$lines[[pname]][[current_index]][["y1"]])

            tmp_y2 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["y2"]]),
              0,
              current_layer$lines[[pname]][[current_index]][["y2"]])

            current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
              tmp_x1,
              core_frame$dims[["max.local.y"]])

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              -((core_frame$dims[["max.local.y"]] -
                   core_frame$dims[["min.local.y"]]) * 0.04))

            tmp_point_top_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["topDraw"]]$dev,
              input = "dev")

            tmp_point_bot_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["botDraw"]]$dev,
              input = "dev")

            segments(tmp_point_top_dev$usr$x,
                     tmp_point_top_dev$usr$y,
                     tmp_point_bot_dev$usr$x,
                     tmp_point_bot_dev$usr$y,
                     col = "black")

            current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
              tmp_x1,
              tmp_y2)$dev

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2)$dev

            plot_tops[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
              tmp_x1,
              tmp_y1)

            plot_bots[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2)

            index_num[[pname]] <- index_num[[pname]] + 1
          }

          req_draw <- TRUE
        }
      }
    }

    if (!is.null(core_frame[["legendpars"]])) {
      if (lookup && core_frame$legendpars[["panel"]] ==  current_facet) {
        draw_legend(core_frame)
      } else if (lookup && is.na(core_frame$legendpars[["panel"]])) {
        draw_legend(core_frame)
      }
    }
  }

  # Note: final overlays, once facets are drawn/coords cached
  if (req_draw) {

    n_phase_lines <- unique(names(plot_bots))

    for (pl in seq_len(length(n_phase_lines))) {

      n_facets <- length(plot_tops[[n_phase_lines[pl]]])

      for (plfacet in 2:n_facets) {

        #pts.pre <- plot_tops[[n_phase_lines[pl]]][[plfacet - 1]]
        pbs_pre <- plot_bots[[n_phase_lines[pl]]][[plfacet - 1]]

        pts <- plot_tops[[n_phase_lines[pl]]][[plfacet]]
        pbs <- plot_bots[[n_phase_lines[pl]]][[plfacet]]

        #tmp.point.top.pre.dev <- cnvrt_coords(pts.pre$dev, input = "dev")
        tmp_point_bot_pre_dev <- cnvrt_coords(pbs_pre$dev, input = "dev")

        tmp_point_top_dev <- cnvrt_coords(pts$dev, input = "dev")
        tmp_point_bot_dev <- cnvrt_coords(pbs$dev, input = "dev")

        segments(tmp_point_bot_pre_dev$usr$x, tmp_point_bot_pre_dev$usr$y,
                 tmp_point_bot_pre_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                                 tmp_point_top_dev$usr$y) / 2,
                 col = "black")

        segments(tmp_point_bot_pre_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                                 tmp_point_top_dev$usr$y) / 2,
                 tmp_point_top_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                             tmp_point_top_dev$usr$y) / 2,
                 col = "black")

        segments(tmp_point_top_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                             tmp_point_top_dev$usr$y) / 2,
                 tmp_point_top_dev$usr$x, tmp_point_top_dev$usr$y,
                 col = "black")
      }
    }
  }

  if (!lookup && !is.null(core_frame[["legendpars"]]))  draw_legend(core_frame)

  mtext(core_frame$labs[["title"]],
        side = 3,
        outer = core_frame$labs[["outer"]])
  mtext(core_frame$labs[["ylab"]],
        side = 2,
        outer = core_frame$labs[["outer"]],
        line = core_frame$labs[["outer.y.line"]])
  mtext(core_frame$labs[["xlab"]],
        side = 1,
        outer = core_frame$labs[["outer"]],
        line = core_frame$labs[["outer.x.line"]])
}
