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
                     mai  = c(0.375, 0.375, 0.25, 0.25),
                     omi  = c(0.25, 0.25, 0.25, 0.25),
                     ncol = 1) {

  coreFrame = list()                             # Primary plotting object
  coreFrame[[ "layers" ]] <- list()              # Layers for drawing
  coreFrame[[ "aes"    ]] <- enexpr(aesthetics)  # Mappings
  coreFrame[[ "data"   ]] <- data                # Stored data
  coreFrame[[ "dims"   ]] <- list(               # Global dimensions
    global.max.x = max(data[[as.character(coreFrame$aes['x'])]], na.rm = TRUE),
    global.min.x = min(data[[as.character(coreFrame$aes['x'])]], na.rm = TRUE),
    global.max.y = max(data[[as.character(coreFrame$aes['y'])]], na.rm = TRUE),
    global.min.y = min(data[[as.character(coreFrame$aes['y'])]], na.rm = TRUE),
    mai          = mai,
    omi          = omi,
    ncol         = ncol,
    xdelta       = 1,                            # General defaults, for now
    ydelta       = 1)

  coreFrame[[ "labs"   ]] <- list(               # Presumed labels, blank title by default
    xlab         = as.character(coreFrame$aes['x']),
    ylab         = as.character(coreFrame$aes['y']),
    title        = "")

  class(coreFrame) <- c("fxl")                   # Apply a class name (to override print)

  coreFrame
}

#' scr_arrows
#'
#' Add a layer with arrows on plot
#'
#' @param coreFrame fxl class
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
scr_arrows <- function(coreFrame, arrows = NULL, facet = NULL,
                       color = 'black', length = 0.25, angle = 30,
                       code = 2, lwd = 1, lty = 1) {
  newlayer = list()
  newlayer[[ "type"   ]] = "arrows"
  newlayer[[ "facet"  ]] = facet
  newlayer[[ "arrows" ]] = arrows
  newlayer[[ "color"  ]] = color
  newlayer[[ "length" ]] = length
  newlayer[[ "angle"  ]] = angle
  newlayer[[ "code"   ]] = code
  newlayer[[ "lwd"    ]] = lwd
  newlayer[[ "lty"    ]] = lty

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_brackets
#'
#' Add a layer with brackets on plot
#'
#' @param coreFrame fxl class
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
scr_brackets <- function(coreFrame, brackets = NULL, facet = NULL,
                         color = 'black', length = 0.25,
                         angle = 30, code = 2, lwd = 1, lty = 1) {

  newlayer = list()
  newlayer[[ "type"     ]] = "brackets"
  newlayer[[ "facet"    ]] = facet
  newlayer[[ "brackets" ]] = brackets
  newlayer[[ "color"    ]] = color
  newlayer[[ "length"   ]] = length
  newlayer[[ "angle"    ]] = angle
  newlayer[[ "code"     ]] = code
  newlayer[[ "lwd"      ]] = lwd
  newlayer[[ "lty"      ]] = lty

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_cumsum
#'
#' Draw lines, but as a cumulative and rolling sum
#'
#' @param coreFrame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param mapping from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_cumsum_lines <- function(coreFrame, lty = 1, color = 'black',
                       size = 1, mapping) {

  newlayer = list()
  newlayer[[ "type"       ]] <- "cum_sum_lines"
  newlayer[[ "lty"        ]] <- lty
  newlayer[[ "color"      ]] <- color
  newlayer[[ "size"       ]] <- size
  newlayer[[ "aesthetics" ]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame

}

#' scr_cumsum_points
#'
#' @param coreFrame fxl object
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
scr_cumsum_points <- function(coreFrame, pch = 21, color = 'black',
                              fill = 'black', cex = 1, mapping) {

  newlayer = list()
  newlayer[[ "type"       ]] <- "cum_sum_points"
  newlayer[[ "pch"        ]] <- pch
  newlayer[[ "color"      ]] <- color
  newlayer[[ "fill"       ]] <- fill
  newlayer[[ "cex"        ]] <- cex
  newlayer[[ "aesthetics" ]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame

}

#' scr_lines
#'
#' @param coreFrame fxl object
#' @param lty from base
#' @param color from base
#' @param size from base
#' @param mapping from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_lines <- function(coreFrame, lty = 1, color = 'black',
                      size = 1, mapping) {

  newlayer = list()
  newlayer[[ "type"       ]] <- "line"
  newlayer[[ "lty"        ]] <- lty
  newlayer[[ "color"      ]] <- color
  newlayer[[ "size"       ]] <- size
  newlayer[[ "aesthetics" ]] <- NA

  if (!missing(mapping)) newlayer[["aesthetics"]] <- enexpr(mapping)

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_label_phase
#'
#' labels to be drawn on plots (typically for phases/conditions, but not necessarily)
#'
#' @param coreFrame fxl object
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
scr_label_phase <- function(coreFrame, color = 'black',
                            cex = 1, adj = 0.5,
                            facet = NULL, labels = NULL) {

  newlayer = list()
  newlayer[[ "type"   ]] <- "phase_label"
  newlayer[[ "color"  ]] <- color
  newlayer[[ "cex"    ]] <- cex
  newlayer[[ "adj"    ]] <- adj
  newlayer[[ "facet"  ]] <- facet
  newlayer[[ "labels" ]] <- labels

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_label_facet
#'
#' @param coreFrame fxl object
#' @param color from base
#' @param cex from base
#' @param adj from base
#' @param labels as stated
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_label_facet <- function(coreFrame, color = 'black',
                            cex = 1, adj = 0.5,
                            labels = NULL) {

  newlayer = list()
  newlayer[[ "type"   ]] <- "facet_label"
  newlayer[[ "color"  ]] <- color
  newlayer[[ "cex"    ]] <- cex
  newlayer[[ "adj"    ]] <- adj
  newlayer[[ "labels" ]] <- labels

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_guide_line
#'
#' This is an annotation illustrating an aim/reduction line
#'
#' @param coreFrame fxl object
#' @param coords start and finish coords for aim line
#' @param color from base
#' @param lty line type
#' @param lwd line width
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_guide_line <- function(coreFrame, coords, facet = NA,
                           color = 'red', lty = 1, lwd = 1) {

  newlayer = list()
  newlayer[[ "type"   ]] <- "guide_line"
  newlayer[[ "coords" ]] <- coords
  newlayer[[ "col"    ]] <- color
  newlayer[[ "facet"  ]] <- facet
  newlayer[[ "lty"    ]] <- lty
  newlayer[[ "lwd"    ]] <- lwd

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_plines
#'
#' @param coreFrame fxl object
#' @param lines phase lines to be drawn
#' @param lwd from base
#' @param lty from base
#' @param col from base
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_plines <- function(coreFrame, lines = NULL,
                       lwd = 1, lty = 1, col = 'black') {

  newlayer = list()
  newlayer[[ "type"  ]] <- "phase_lines"
  newlayer[[ "lines" ]] <- lines
  newlayer[[ "lwd"   ]] <- lwd
  newlayer[[ "lty"   ]] <- lty
  newlayer[[ "col"   ]] <- col

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_plines_mbd
#'
#' @param coreFrame fxl object
#' @param lines phase lines to be drawn
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_plines_mbd <- function(coreFrame, lines = NULL) {

  newlayer = list()
  newlayer[[ "type"  ]] <- "mbd_phase_lines"
  newlayer[[ "lines" ]] <- lines

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' scr_points
#'
#' @param coreFrame fxl object
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
scr_points <- function(coreFrame, pch = 21, color = 'black',
                       fill = 'black', cex = 1, mapping) {

  newlayer = list()
  newlayer[[ "type"       ]] <- "point"
  newlayer[[ "pch"        ]] <- pch
  newlayer[[ "color"      ]] <- color
  newlayer[[ "fill"       ]] <- fill
  newlayer[[ "cex"        ]] <- cex
  newlayer[[ "aesthetics" ]] <- NA

  if (!missing(mapping))  newlayer[["aesthetics"]] <- enexpr(mapping)

  coreFrame$layers[[(length(coreFrame[["layers"]]) + 1)]] <- newlayer

  coreFrame
}

#' xlabel
#'
#' Override the x axis label
#'
#' @param coreFrame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_xlabel <- function(coreFrame, var) {

  coreFrame$labs[["xlab"]] = {{ var }}

  coreFrame
}

#' xoverride
#'
#' Override the x axis limits
#'
#' @param coreFrame fxl object
#' @param var string for title
#' @param xdelta skips between ticks (can override)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_xoverride <- function(coreFrame, var, xdelta = 1) {

  coreFrame$dims[[ "global.min.x" ]] = {{ var[1] }}
  coreFrame$dims[[ "global.max.x" ]] = {{ var[2] }}
  coreFrame$dims[[ "xdelta"       ]] = xdelta

  coreFrame
}

#' ylabel
#'
#' Override the y axis label
#'
#' @param coreFrame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_ylabel <- function(coreFrame, var) {

  coreFrame$labs[["ylab"]] = {{ var }}

  coreFrame
}

#' yoverride
#'
#' Override the y axis (or axes) limits
#'
#' @param coreFrame fxl object
#' @param var from base
#' @param ydelta skips between ticks (can override)
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_yoverride <- function(coreFrame, var, ydelta = 1) {

  # Check if a vector and not multi-facet list
  if (is.vector(var) & !is.list(var)) {
    coreFrame$dims[[ "global.min.y" ]] = {{ var[1] }}
    coreFrame$dims[[ "global.max.y" ]] = {{ var[2] }}

  } else {
    coreFrame$dims[[ "local.dims"   ]] = var

  }

  coreFrame$dims[["ydelta"]] = ydelta

  coreFrame
}

#' scrtitle
#'
#' Override the title
#'
#' @param coreFrame fxl object
#' @param var string
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
scr_title <- function(coreFrame, var) {

  coreFrame$labs[["title"]] = {{ var }}

  coreFrame
}

#' scrlegend
#'
#' Information for drawing legend onto plots
#'
#' @param coreFrame fxl object
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
scr_legend <- function(coreFrame, panel = NA, legend,
                       col, lty, pch, box.lty = 0,
                       bty = "n", cex = 1, horiz = FALSE,
                       position = "topright", pt.cex = 1,
                       text.col = "black") {

  coreFrame$legendpars = list()
  coreFrame$legendpars[[ "panel"    ]] = panel
  coreFrame$legendpars[[ "legend"   ]] = legend
  coreFrame$legendpars[[ "col"      ]] = text.col
  coreFrame$legendpars[[ "bg"       ]] = col
  coreFrame$legendpars[[ "lty"      ]] = lty
  coreFrame$legendpars[[ "pch"      ]] = pch
  coreFrame$legendpars[[ "bty"      ]] = bty
  coreFrame$legendpars[[ "box.lty"  ]] = box.lty
  coreFrame$legendpars[[ "cex"      ]] = cex
  coreFrame$legendpars[[ "horiz"    ]] = horiz
  coreFrame$legendpars[[ "position" ]] = position
  coreFrame$legendpars[[ "pt.cex"   ]] = pt.cex
  coreFrame$legendpars[[ "text.col" ]] = text.col

  coreFrame
}

#' scrsave
#'
#' Function for outputting fxl object at preset size (certain journal are opinionated on size, format, and density)
#'
#' @param coreFrame fxl object
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
scr_save <- function(coreFrame, units = "in",
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

  print.fxl(coreFrame)

  dev.off()

  coreFrame
}

### OVERRIDES ###

#' print.fxl
#'
#' Override the final call to print the fxl object. catches the obj and prints out layers in the sequence laid out by the user
#'
#' @param coreFrame fxl object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export print.fxl
#' @export
print.fxl <- function(coreFrame, ...) {

  # Holders for phase coords
  plotTops   = list()
  plotBots   = list()
  indexNum   = list()

  facets     = NULL
  n.facets   = 1
  n.cols     = 1
  lookup     = FALSE
  reqDraw    = FALSE

  if ("facet" %in% names(coreFrame$aes)) {
    facets         = unique(coreFrame$data[[as.character(coreFrame$aes['facet'])]])
    n.facets       = length(facets)
    n.facets.draw  = n.facets
    n.cols         = coreFrame[["dims"]][["ncol"]]
    n.facets.draw  = as.integer(n.facets / n.cols)
    lookup         = TRUE
  }

  par(mfrow  = c(n.facets.draw, n.cols),                 # Dynamic facet numbers/cols
      family = "serif",
      omi    = coreFrame[["dims"]][["omi"]],
      mai    = coreFrame[["dims"]][["mai"]],
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  for (facetIndex in 1:n.facets) {                # Print placeholders

    # Defaults, per data
    currentFacet                      = NA
    coreFrame$dims[["min.local.x"]]   = min(coreFrame$data[[as.character(coreFrame$aes['x'])]], na.rm = TRUE)
    coreFrame$dims[["max.local.x"]]   = max(coreFrame$data[[as.character(coreFrame$aes['x'])]], na.rm = TRUE)

    # Facet override
    if (lookup) currentFacet          = facets[facetIndex]

    # X overrides
    if (!is.null(coreFrame$dims[["global.min.x"]]))
      coreFrame$dims[["min.local.x"]] = coreFrame$dims[["global.min.x"]]

    if (!is.null(coreFrame$dims[["global.max.x"]]))
      coreFrame$dims[["max.local.x"]] = coreFrame$dims[["global.max.x"]]

    # Y overrides
    if (!is.null(coreFrame$dims[["local.dims"]])) {
      coreFrame$dims[["min.local.y"]] = coreFrame$dims[["local.dims"]][[facetIndex]]$y0
      coreFrame$dims[["max.local.y"]] = coreFrame$dims[["local.dims"]][[facetIndex]]$y1
    } else {
      coreFrame$dims[["min.local.y"]] = ifelse(is.null(coreFrame$dims[["global.min.y"]]),
                                               min(coreFrame$data[[as.character(coreFrame$aes['y'])]]),
                                               coreFrame$dims[["global.min.y"]])
      coreFrame$dims[["max.local.y"]] = ifelse(is.null(coreFrame$dims[["global.min.y"]]),
                                               max(coreFrame$data[[as.character(coreFrame$aes['y'])]]),
                                               coreFrame$dims[["global.max.y"]])
    }

    plot(NULL,
         xlim = c(coreFrame$dims[["min.local.x"]], coreFrame$dims[["max.local.x"]]),
         ylim = c(coreFrame$dims[["min.local.y"]], coreFrame$dims[["max.local.y"]]),
         ylab = "",
         xlab = "",
         frame.plot = FALSE,
         las = 1,
         xaxt = 'n',
         yaxt = 'n')

    box(bty = "l")

    axis(1,
         labels = (facetIndex == n.facets),
         at     = seq(coreFrame$dims[["global.min.x"]],
                      coreFrame$dims[["global.max.x"]],
                      by = coreFrame$dims[['xdelta']]))

    axis(2,
         labels = TRUE,
         las    = 1,
         at     = seq(coreFrame$dims[["min.local.y"]],
                      coreFrame$dims[["max.local.y"]],
                      by = coreFrame$dims[['ydelta']]))

    if (!is.null(coreFrame[["legendpars"]])) {
      if (lookup & coreFrame$legendpars[["panel"]] == currentFacet)
        draw_legend(coreFrame)
      else
        draw_legend(coreFrame)
    }

    if (length(coreFrame[["layers"]]) > 0) {
      for (i in 1:length(coreFrame[["layers"]])) {

        currentLayer = coreFrame$layers[[i]]

        if (currentLayer$type == "arrows")         draw_arrows(        coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "brackets")       draw_brackets(      coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "cum_sum_lines")  draw_cumsum_lines(  coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "cum_sum_points") draw_cumsum_points( coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "facet_label")    draw_label_facet(   coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "guide_line")     draw_guide_line(    coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "line")           draw_lines(         coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "phase_label")    draw_label_phase(   coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "phase_lines")    draw_scr_plines(    coreFrame,  currentLayer,  currentFacet)
        if (currentLayer$type == "point")          draw_points(        coreFrame,  currentLayer,  currentFacet)

        if (currentLayer$type == "mbd_phase_lines") {

          plines <- names(currentLayer$lines)

          for (pname in plines) {

            if (!(pname %in% names(indexNum))) indexNum[[pname]] <- 1

            currentLayer$lines[[pname]][[facetIndex]][["topDraw"]] <- cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["x1"]],
              coreFrame$dims[["max.local.y"]])

            currentLayer$lines[[pname]][[facetIndex]][["botDraw"]] <- cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["x2"]],
              - ((coreFrame$dims[["max.local.y"]] - coreFrame$dims[["min.local.y"]]) * 0.04))

            tmp.point.top.dev <- cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["topDraw"]]$dev,
              input = 'dev')

            tmp.point.bot.dev <- cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["botDraw"]]$dev,
              input = 'dev')

            segments(tmp.point.top.dev$usr$x,
                     tmp.point.top.dev$usr$y,
                     tmp.point.bot.dev$usr$x,
                     tmp.point.bot.dev$usr$y,
                     col = 'black')

            currentLayer$lines[[pname]][[facetIndex]][["topDraw"]] <- cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["x1"]],
              currentLayer$lines[[pname]][[facetIndex]][["y1"]])$dev

            currentLayer$lines[[pname]][[facetIndex]][["botDraw"]] <- cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["x2"]],
              currentLayer$lines[[pname]][[facetIndex]][["y2"]])$dev

            plotTops[[pname]][[indexNum[[pname]]]] = cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["x1"]],
              currentLayer$lines[[pname]][[facetIndex]][["y1"]])

            plotBots[[pname]][[indexNum[[pname]]]] = cnvrt.coords(
              currentLayer$lines[[pname]][[facetIndex]][["x2"]],
              currentLayer$lines[[pname]][[facetIndex]][["y2"]])

            indexNum[[pname]] <- indexNum[[pname]] + 1
          }

          reqDraw = TRUE
        }
      }
    }
  }

  # Note: final overlays, once facets are drawn/coords cached
  if (reqDraw) {

    n.phaseLines <- unique(names(plotBots))

    for (pl in 1:length(n.phaseLines)) {

      n.facets <- length(plotTops[[n.phaseLines[pl]]])

      for (plfacet in 2:n.facets) {

        pts.pre <- plotTops[[n.phaseLines[pl]]][[plfacet - 1]]
        pbs.pre <- plotBots[[n.phaseLines[pl]]][[plfacet - 1]]

        pts <- plotTops[[n.phaseLines[pl]]][[plfacet]]
        pbs <- plotBots[[n.phaseLines[pl]]][[plfacet]]

        tmp.point.top.pre.dev <- cnvrt.coords(pts.pre$dev, input = 'dev')
        tmp.point.bot.pre.dev <- cnvrt.coords(pbs.pre$dev, input = 'dev')

        tmp.point.top.dev <- cnvrt.coords(pts$dev, input = 'dev')
        tmp.point.bot.dev <- cnvrt.coords(pbs$dev, input = 'dev')

        segments(tmp.point.bot.pre.dev$usr$x, tmp.point.bot.pre.dev$usr$y,
                 tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y +
                                                 tmp.point.top.dev$usr$y) / 2,
                 col = 'black')

        segments(tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y +
                                                 tmp.point.top.dev$usr$y) / 2,
                 tmp.point.top.dev$usr$x, (tmp.point.bot.pre.dev$usr$y +
                                             tmp.point.top.dev$usr$y) / 2,
                 col = 'black')

        segments(tmp.point.top.dev$usr$x, (tmp.point.bot.pre.dev$usr$y +
                                             tmp.point.top.dev$usr$y) / 2,
                 tmp.point.top.dev$usr$x, tmp.point.top.dev$usr$y,
                 col = 'black')
      }
    }
  }

  mtext(coreFrame$labs[["title"]], side = 3, outer = TRUE, line = 0)
  mtext(coreFrame$labs[["ylab"]],  side = 2, outer = TRUE)
  mtext(coreFrame$labs[["xlab"]],  side = 1, outer = TRUE)
}
