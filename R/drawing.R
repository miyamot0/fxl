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

### DRAWING ###

#' draw_arrows
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_arrows <- function(coreFrame, currentLayer, n) {
  if (currentLayer$facet == as.character(n)) {

    for (aindex in 1:length(currentLayer$arrows)) {
      currentArrow = currentLayer$arrows[[aindex]]

      arrows(x0 = as.numeric(currentArrow[["x0"]]),
             x1 = as.numeric(currentArrow[["x1"]]),
             y0 = as.numeric(currentArrow[["y0"]]),
             y1 = as.numeric(currentArrow[["y1"]]),
             length = as.numeric(currentLayer[["length"]]),
             angle = as.numeric(currentLayer[["angle"]]),
             code = as.numeric(currentLayer[["code"]]),
             col = as.character(currentLayer[["color"]]),
             lty = as.numeric(currentLayer[["lty"]]),
             lwd = as.numeric(currentLayer[["lwd"]]))
    }
  }
}

#' draw_brackets
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_brackets <- function(coreFrame, currentLayer, n) {
  if (currentLayer$facet == as.character(n)) {

    for (bindex in 1:length(currentLayer$brackets)) {
      currentBracket = currentLayer$brackets[[bindex]]

      segments(
        x0 = as.numeric(currentBracket[["x0"]]),
        x1 = as.numeric(currentBracket[["x1"]]),
        y0 = as.numeric(currentBracket[["y0"]]),
        y1 = as.numeric(currentBracket[["y0"]]),
        col = as.character(currentLayer[["color"]]),
        lty = as.numeric(currentLayer[["lty"]]),
        lwd = as.numeric(currentLayer[["lwd"]])
      )

      arrows(x0 = as.numeric(currentBracket[["x0"]]),
             x1 = as.numeric(currentBracket[["x0"]]),
             y0 = as.numeric(currentBracket[["y0"]]),
             y1 = as.numeric(currentBracket[["y1"]]),
             length = as.numeric(currentLayer[["length"]]),
             angle = as.numeric(currentLayer[["angle"]]),
             code = as.numeric(currentLayer[["code"]]),
             col = as.character(currentLayer[["color"]]),
             lty = as.numeric(currentLayer[["lty"]]),
             lwd = as.numeric(currentLayer[["lwd"]]))

      arrows(x0 = as.numeric(currentBracket[["x1"]]),
             x1 = as.numeric(currentBracket[["x1"]]),
             y0 = as.numeric(currentBracket[["y0"]]),
             y1 = as.numeric(currentBracket[["y1"]]),
             length = as.numeric(currentLayer[["length"]]),
             angle = as.numeric(currentLayer[["angle"]]),
             code = as.numeric(currentLayer[["code"]]),
             col = as.character(currentLayer[["color"]]),
             lty = as.numeric(currentLayer[["lty"]]),
             lwd = as.numeric(currentLayer[["lwd"]]))
    }
  }
}


#' draw_lines
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_lines <- function(coreFrame, currentLayer, n) {
  if (is.na(n)) {
    currentData <- coreFrame$data
  } else {
    currentData <- coreFrame$data[which(
      coreFrame$data[, as.character(coreFrame$aes['facet'])] == n),]
  }

  for (p in unique(currentData[, as.character(coreFrame$aes['p'])])) {

    currentData.slice <- currentData[which(currentData[, as.character(coreFrame$aes['p'])] == p),]

    localAesthetics = list(
      "x" = as.character(coreFrame$aes['x']),
      "y" = as.character(coreFrame$aes['y'])
    )

    if (!is.na(currentLayer['aesthetics'])) {
      localAesthetics = list(
        "x" = as.character(currentLayer$aesthetics['x']),
        "y" = as.character(currentLayer$aesthetics['y'])
      )
    }

    lines(
      currentData.slice[, as.character(localAesthetics['x'])],
      currentData.slice[, as.character(localAesthetics['y'])],
      lty = currentLayer$lty,
      col = currentLayer$color,
      lwd = currentLayer$size
    )
  }
}

#' draw_label_phase
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_label_phase <- function(coreFrame, currentLayer, n) {
  if (currentLayer$facet == n) {
    for (lindex in 1:length(currentLayer$labels)) {
      label = names(currentLayer$labels)[lindex]

      currentLabel = currentLayer$labels[[lindex]]

      srt = ifelse("srt" %in% names(currentLabel),
                   currentLabel[["srt"]],
                   0)

      text(x = currentLabel[["x"]],
           y = currentLabel[["y"]],
           cex = currentLayer[["cex"]],
           adj = currentLayer[["adj"]],
           srt = srt,
           labels = label)
    }
  }
}

#' draw_points
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_points <- function(coreFrame, currentLayer, n) {

  if (is.na(n)) {
    currentData <- coreFrame$data
  } else {
    currentData <- coreFrame$data[which(
      coreFrame$data[, as.character(coreFrame$aes['facet'])] == n),]
  }

  localAesthetics = list(
    "x" = as.character(coreFrame$aes['x']),
    "y" = as.character(coreFrame$aes['y'])
  )

  if (!is.na(currentLayer['aesthetics'])) {
    localAesthetics = list(
      "x" = as.character(currentLayer$aesthetics['x']),
      "y" = as.character(currentLayer$aesthetics['y'])
    )
  }

  for (p in unique(currentData[, as.character(coreFrame$aes['p'])])) {

    currentData.slice <- currentData[which(currentData[, as.character(coreFrame$aes['p'])] == p),]

    localAesthetics = list(
      "x" = as.character(coreFrame$aes['x']),
      "y" = as.character(coreFrame$aes['y'])
    )

    if (!is.na(currentLayer['aesthetics'])) {
      localAesthetics = list(
        "x" = as.character(currentLayer$aesthetics['x']),
        "y" = as.character(currentLayer$aesthetics['y'])
      )
    }

    pch = 1

    if (is.list(currentLayer$pch)) {
      pch = currentLayer$pch[[p]]
    } else {
      pch = currentLayer$pch
    }

    fill = 'black'

    if (is.list(currentLayer$fill)) {
      fill = currentLayer$fill[[p]]
    } else {
      fill = currentLayer$fill
    }

    points(
      currentData.slice[, as.character(localAesthetics['x'])],
      currentData.slice[, as.character(localAesthetics['y'])],
      pch = pch,
      cex = currentLayer$cex,
      bg = fill
    )
  }
}

#' draw_label_facet
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_label_facet <- function(coreFrame, currentLayer, n) {
  currentLabel = currentLayer$labels[[as.character(n)]]

  if (!is.null(currentLabel)) {
    text(x = currentLabel[["x"]],
         y = currentLabel[["y"]],
         cex = currentLayer[["cex"]],
         adj = currentLayer[["adj"]],
         labels = names(currentLayer$labels)[n])
  }
}

#' draw_scr_plines
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_scr_plines <- function(coreFrame, currentLayer, n) {
  if (as.character(n) %in% names(currentLayer$lines)) {
    for (name in names(currentLayer$lines[[n]])) {

      tempY1 <- ifelse(currentLayer$lines[[n]][[name]][['y1']] == 0,
                       - ((as.numeric(coreFrame$dims[["max.local.y"]]) -
                            as.numeric(coreFrame$dims[["min.local.y"]])) * 0.04),
                       currentLayer$lines[[n]][[name]][['y1']])

      tempY2 <- ifelse(currentLayer$lines[[n]][[name]][['y2']] == 0,
                       - ((as.numeric(coreFrame$dims[["max.local.y"]]) -
                            as.numeric(coreFrame$dims[["min.local.y"]])) * 0.04),
                       currentLayer$lines[[n]][[name]][['y2']])

      lines(
        c(currentLayer$lines[[n]][[name]][['x1']],
          currentLayer$lines[[n]][[name]][['x2']]),
        c(tempY1, tempY2),
        lty = currentLayer[["lty"]],
        col = currentLayer[["col"]],
        lwd = currentLayer[["lwd"]]
      )
    }
  }
}

#' draw_legend
#'
#' drawing function
#'
#' @param coreFrame fxl object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_legend <- function(coreFrame) {
  legend(
    coreFrame$legendpars[["position"]],
    legend = as.character(coreFrame$legendpars[["legend"]]),
    text.col = as.character(coreFrame$legendpars[["text.col"]]),
    lty = as.numeric(coreFrame$legendpars[["lty"]]),
    box.lty = as.numeric(coreFrame$legendpars[["box.lty"]]),
    pch = as.numeric(coreFrame$legendpars[["pch"]]),
    bty = as.character(coreFrame$legendpars[["bty"]]),
    pt.cex = as.numeric(coreFrame$legendpars[["pt.cex"]]),
    cex = as.numeric(coreFrame$legendpars[["cex"]]),
    col = as.character(coreFrame$legendpars[["col"]]),
  #pt.bg    = as.character(coreFrame$legendpars[["col"]]),
    horiz = as.logical(coreFrame$legendpars[["horiz"]])
  )
}
