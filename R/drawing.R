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
draw_arrows <- function(coreFrame, currentLayer, facetName) {
  if (currentLayer$facet == as.character(facetName)) {
    for (aindex in 1:length(currentLayer$arrows)) {
      currentArrow  = currentLayer$arrows[[aindex]]

      arrows(x0     = as.numeric(   currentArrow[[ "x0"     ]]),
             x1     = as.numeric(   currentArrow[[ "x1"     ]]),
             y0     = as.numeric(   currentArrow[[ "y0"     ]]),
             y1     = as.numeric(   currentArrow[[ "y1"     ]]),

             length = as.numeric(   currentLayer[[ "length" ]]),
             angle  = as.numeric(   currentLayer[[ "angle"  ]]),
             code   = as.numeric(   currentLayer[[ "code"   ]]),
             col    = as.character( currentLayer[[ "color"  ]]),
             lty    = as.numeric(   currentLayer[[ "lty"    ]]),
             lwd    = as.numeric(   currentLayer[[ "lwd"    ]]))
    }
  }
}

#' draw_brackets
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_brackets <- function(coreFrame, currentLayer, facetName) {
  if (currentLayer$facet == as.character(facetName)) {

    for (bindex in 1:length(currentLayer$brackets)) {
      currentBracket = currentLayer$brackets[[bindex]]

      l.lty = as.numeric(currentLayer[["lty"]])

      if ("lty" %in% names(currentBracket))
        l.lty = currentBracket$lty

      segments(
        x0          = as.numeric(   currentBracket[[ "x0"     ]]),
        x1          = as.numeric(   currentBracket[[ "x1"     ]]),
        y0          = as.numeric(   currentBracket[[ "y0"     ]]),
        y1          = as.numeric(   currentBracket[[ "y0"     ]]),

        col         = as.character( currentLayer[[   "color"  ]]),
        lty         = l.lty,
        lwd         = as.numeric(   currentLayer[[   "lwd"    ]]))

      arrows(x0     = as.numeric(   currentBracket[[ "x0"     ]]),
             x1     = as.numeric(   currentBracket[[ "x0"     ]]),
             y0     = as.numeric(   currentBracket[[ "y0"     ]]),
             y1     = as.numeric(   currentBracket[[ "y1"     ]]),

             length = as.numeric(   currentLayer[[   "length" ]]),
             angle  = as.numeric(   currentLayer[[   "angle"  ]]),
             code   = as.numeric(   currentLayer[[   "code"   ]]),
             col    = as.character( currentLayer[[   "color"  ]]),
             lty    = l.lty,
             lwd    = as.numeric(   currentLayer[[   "lwd"    ]]))

      arrows(x0     = as.numeric(   currentBracket[[ "x1"     ]]),
             x1     = as.numeric(   currentBracket[[ "x1"     ]]),
             y0     = as.numeric(   currentBracket[[ "y0"     ]]),
             y1     = as.numeric(   currentBracket[[ "y1"     ]]),
             length = as.numeric(   currentLayer[[   "length" ]]),
             angle  = as.numeric(   currentLayer[[   "angle"  ]]),
             code   = as.numeric(   currentLayer[[   "code"   ]]),
             col    = as.character( currentLayer[[   "color"  ]]),
             lty    = l.lty,
             lwd    = as.numeric(   currentLayer[[   "lwd"    ]]))
    }
  }
}

#' draw_bar_support
#'
#' Draw bars, but on a secondary axis
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @return
#' @export
draw_bar_support <- function(coreFrame,  currentLayer, facetName) {

  if (is.na(facetName))  currentData   = coreFrame$data
  else                   currentData   = coreFrame$data[which(
    coreFrame$data[, as.character(coreFrame$aes['facet'])] == facetName),]

  # In case no phases are included?
  if (!('p' %in% names(coreFrame$aes))) {
    coreFrame$aes['p'] = 'p'
    currentData[, 'p'] = '0'
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

  label.y = as.character(localAesthetics['y'])

  if (currentLayer$label != "")  label.y = as.character(currentLayer$label)

  opar <- par()

  # Add on to "new" plot
  par(new = TRUE,
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  plot(NULL,
       axes = FALSE,
       xlim = c(coreFrame$dims[["min.local.x"]],
                coreFrame$dims[["max.local.x"]]),
       ylim = c(0,100),
       xlab = "",
       ylab = "",
       frame.plot = FALSE,
       las = 1,
       xaxt = 'n',
       yaxt = 'n')

  box(bty = "U")

  p.off   = currentLayer$width / 2

  for (p in unique(currentData[, as.character(coreFrame$aes['p'])])) {

    currentData.slice <- currentData[which(currentData[, as.character(coreFrame$aes['p'])] == p),]

    for (row in 1:nrow(currentData.slice)) {

      rect(currentData.slice[row, as.character(localAesthetics['x'])] - p.off,
           0,
           currentData.slice[row, as.character(localAesthetics['x'])] + p.off,
           currentData.slice[row, as.character(localAesthetics['y'])],
           col = currentLayer$color)
    }
  }

  axis(side = 4,
       las = 1,
       at = pretty(range(c(0,currentData.slice[, as.character(localAesthetics['y'])]))))

  mtext(label.y,
        side = 4,
        outer = TRUE)

  par(opar)
}

#' draw_guide_line
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @return
#' @export
draw_guide_line <- function(coreFrame,  currentLayer, facetName) {

  if (is.na(facetName) | currentLayer$facet == facetName) {

    for (gindex in 1:length(currentLayer$coords)) {
      currentCoords = currentLayer$coords[[gindex]]

      l.col = as.character( currentLayer[["col"]])
      l.lty = as.numeric(   currentLayer[["lty"]])
      l.lwd = as.numeric(   currentLayer[["lwd"]])

      if ("col" %in% names(currentCoords))
        l.col = currentCoords$col

      if ("lty" %in% names(currentCoords))
        l.lty = currentCoords$lty

      if ("lwd" %in% names(currentCoords))
        l.lwd = currentCoords$lwd

      tempY2 = ifelse("y1" %in% names(currentCoords),
                      currentCoords$y1,
                      currentCoords$y0)

      segments(
        x0    = currentCoords$x0,
        x1    = currentCoords$x1,
        y0    = currentCoords$y0,
        y1    = tempY2,
        lty   = l.lty,
        lwd   = l.lwd,
        col   = l.col
      )
    }
  }
}

#' draw_lines
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_lines <- function(coreFrame, currentLayer, facetName) {
  if (is.na(facetName)) currentData = coreFrame$data
  else currentData = coreFrame$data[
    which(coreFrame$data[, as.character(coreFrame$aes['facet'])] == facetName),]

  # In case no phases are included?
  if (!('p' %in% names(coreFrame$aes))) {
    coreFrame$aes['p'] = 'p'
    currentData[, 'p'] = '0'
  }

  for (p in unique(currentData[, as.character(coreFrame$aes['p'])])) {
    currentData.slice <- currentData[
      which(currentData[, as.character(coreFrame$aes['p'])] == p),]

    localAesthetics = list(
      "x"   = as.character(coreFrame$aes['x']),
      "y"   = as.character(coreFrame$aes['y'])
    )

    if (!is.na(currentLayer['aesthetics'])) {
      localAesthetics = list(
        "x" = as.character(currentLayer$aesthetics['x']),
        "y" = as.character(currentLayer$aesthetics['y'])
      )
    }

    plotFrame = data.frame(
      X = currentData.slice[, as.character(localAesthetics['x'])],
      Y = currentData.slice[, as.character(localAesthetics['y'])]
    )

    lines(
      plotFrame$X,
      plotFrame$Y,
      lty   = currentLayer$lty,
      col   = currentLayer$color,
      lwd   = currentLayer$size
    )
  }
}

#' draw_cumsum_lines
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @return
#' @export
draw_cumsum_lines <- function(coreFrame, currentLayer, facetName) {

  if (is.na(facetName)) currentData = coreFrame$data
  else currentData = coreFrame$data[
    which(coreFrame$data[, as.character(coreFrame$aes['facet'])] == facetName),]

  # In case no phases are included?
  if (!('p' %in% names(coreFrame$aes))) {
    coreFrame$aes['p'] = 'p'
    currentData[, 'p'] = '0'
  }

  for (p in unique(currentData[, as.character(coreFrame$aes['p'])])) {
    currentData.slice <- currentData[
      which(currentData[, as.character(coreFrame$aes['p'])] == p),]

    localAesthetics = list(
      "x"   = as.character(coreFrame$aes['x']),
      "y"   = as.character(coreFrame$aes['y'])
    )

    if (!is.na(currentLayer['aesthetics'])) {
      localAesthetics = list(
        "x" = as.character(currentLayer$aesthetics['x']),
        "y" = as.character(currentLayer$aesthetics['y'])
      )
    }

    cumsumy = currentData.slice[, as.character(localAesthetics['y'])]
    cumsumy[is.na(cumsumy)] = 0
    cumsumy = cumsum(cumsumy)

    lines(
      currentData.slice[, as.character(localAesthetics['x'])],
      cumsumy,
      lty   = currentLayer$lty,
      col   = currentLayer$color,
      lwd   = currentLayer$size
    )
  }
}

#' draw_cumsum_points
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @return
#' @export
draw_cumsum_points <- function(coreFrame, currentLayer, facetName) {

  if (is.na(facetName))  currentData   = coreFrame$data
  else           currentData   = coreFrame$data[which(
    coreFrame$data[, as.character(coreFrame$aes['facet'])] == facetName),]

  # In case no phases are included?
  if (!('p' %in% names(coreFrame$aes))) {
    coreFrame$aes['p'] = 'p'
    currentData[, 'p'] = '0'
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
      "x"   = as.character(coreFrame$aes['x']),
      "y"   = as.character(coreFrame$aes['y'])
    )

    if (!is.na(currentLayer['aesthetics'])) {
      localAesthetics = list(
        "x" = as.character(currentLayer$aesthetics['x']),
        "y" = as.character(currentLayer$aesthetics['y'])
      )
    }

    pch = 1

    if (is.list(currentLayer$pch))  pch = currentLayer$pch[[p]]
    else                            pch = currentLayer$pch

    fill = 'black'

    if (is.list(currentLayer$fill)) fill = currentLayer$fill[[p]]
    else                            fill = currentLayer$fill

    cumsumy = currentData.slice[, as.character(localAesthetics['y'])]
    cumsumy[is.na(cumsumy)] = 0
    cumsumy = cumsum(cumsumy)

    points(
      currentData.slice[, as.character(localAesthetics['x'])],
      cumsumy,
      pch = pch,
      cex = currentLayer$cex,
      bg  = fill
    )
  }
}

#' draw_label_phase
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_label_phase <- function(coreFrame, currentLayer, facetName) {
  if (currentLayer$facet == facetName) {

    for (lindex in 1:length(currentLayer$labels)) {

      label        = names(currentLayer$labels)[lindex]

      currentLabel = currentLayer$labels[[lindex]]

      print(currentLabel)

      tempX = ifelse("x" %in% names(currentLabel),
                     currentLabel[[ "x" ]],
                     currentLayer$x)

      # tempX = ifelse(!is.null(currentLabel[[ "x"   ]]),
      #                currentLabel[[ "x"   ]],
      #                currentLayer$x)

      tempY = ifelse("y" %in% names(currentLabel),
                     currentLabel[[ "y" ]],
                     currentLayer$y)

      # tempY = ifelse(!is.null(currentLabel[[ "y"   ]]),
      #                currentLabel[[ "y"   ]],
      #                currentLayer$y)

      srt          = ifelse("srt" %in% names(currentLabel),
                            currentLabel[["srt"]],
                            0)

      text(x      = tempX,
           y      = tempY,
           cex    = currentLayer[[ "cex" ]],
           adj    = currentLayer[[ "adj" ]],
           srt    = srt,
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
#' @param facetName name of facet
#' @param zeroAxis filter out all but zeros
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_points <- function(coreFrame, currentLayer, facetName, zeroAxis = FALSE) {

  if (is.na(facetName))  currentData   = coreFrame$data
  else           currentData   = coreFrame$data[which(
    coreFrame$data[, as.character(coreFrame$aes['facet'])] == facetName),]

  # In case no phases are included?
  if (!('p' %in% names(coreFrame$aes))) {
    coreFrame$aes['p'] = 'p'
    currentData[, 'p'] = '0'
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
      "x"   = as.character(coreFrame$aes['x']),
      "y"   = as.character(coreFrame$aes['y'])
    )

    if (!is.na(currentLayer['aesthetics'])) {
      localAesthetics = list(
        "x" = as.character(currentLayer$aesthetics['x']),
        "y" = as.character(currentLayer$aesthetics['y'])
      )
    }

    pch = 1

    if (is.list(currentLayer$pch))  pch = currentLayer$pch[[p]]
    else                            pch = currentLayer$pch

    fill = 'black'

    if (is.list(currentLayer$fill)) fill = currentLayer$fill[[p]]
    else                            fill = currentLayer$fill

    col = 'black'

    if (is.list(currentLayer$color)) col = currentLayer$color[[p]]
    else                             col = currentLayer$color

    cex = 1

    if (is.list(currentLayer$cex)) cex = currentLayer$cex[[p]]
    else                           cex = currentLayer$cex

    plotFrame = data.frame(
      X = currentData.slice[, as.character(localAesthetics['x'])],
      Y = currentData.slice[, as.character(localAesthetics['y'])]
    )

    if (zeroAxis) plotFrame = plotFrame[plotFrame$Y == 0,]

    points(
      plotFrame$X,
      plotFrame$Y,
      pch = pch,
      cex = cex,
      bg  = fill,
      col = col
    )
  }
}

#' draw_label_facet
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_label_facet <- function(coreFrame, currentLayer, facetName) {
  currentLabel = currentLayer$labels[[as.character(facetName)]]
  label = facetName

  if ("label" %in% names(currentLabel)) label = currentLabel[["label"]]

  if (label == "" | label == facetName) {

    tempX = ifelse(!is.null(currentLabel[[ "x"   ]]),
                   currentLabel[[ "x"   ]],
                   currentLayer$x)

    tempY = ifelse(!is.null(currentLabel[[ "y"   ]]),
                   currentLabel[[ "y"   ]],
                   currentLayer$y)

    text(x      = tempX,
         y      = tempY,
         cex    = currentLayer[[ "cex" ]],
         adj    = currentLayer[[ "adj" ]],
         labels = label)
  }
}

#' draw_scr_plines
#'
#' drawing function
#'
#' @param coreFrame fxl object
#' @param currentLayer layer to be drawn
#' @param facetName name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_scr_plines <- function(coreFrame, currentLayer, facetName) {
  if (as.character(facetName) %in% names(currentLayer$lines)) {
    for (key in names(currentLayer$lines[[facetName]])) {

      l.lty = currentLayer[["lty"]]
      l.x1  = currentLayer$lines[[facetName]][[key]][['x1']]
      l.x2  = ifelse(is.null(currentLayer$lines[[facetName]][[key]][['x2']]),
                     l.x1,
                     currentLayer$lines[[facetName]][[key]][['x2']])

      l.y1  = ifelse(is.null(currentLayer$lines[[facetName]][[key]][['y1']]),
                     0,
                     currentLayer$lines[[facetName]][[key]][['y1']])
      l.y2  = ifelse(is.null(currentLayer$lines[[facetName]][[key]][['y2']]),
                     0,
                     currentLayer$lines[[facetName]][[key]][['y2']])

      tempY1 = ifelse(l.y1 == 0,
                      -((as.numeric(coreFrame$dims[["max.local.y"]]) -
                          as.numeric(coreFrame$dims[["min.local.y"]])) * 0.04),
                      currentLayer$lines[[facetName]][[key]][['y1']])

      tempY2 = ifelse(l.y2 == 0,
                      -((as.numeric(coreFrame$dims[["max.local.y"]]) -
                          as.numeric(coreFrame$dims[["min.local.y"]])) * 0.04),
                      l.y2)

      if ("lty" %in% names(currentLayer$lines[[facetName]][[key]]))
        l.lty = currentLayer$lines[[facetName]][[key]]$lty

      lines(
        c(l.x1, l.x2),
        c(tempY1, tempY2),
        lty = l.lty
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

  if (is.list(coreFrame$legendpars[["position"]])) {

    legend(
      x = coreFrame$legendpars[["position"]]$x,
      y = coreFrame$legendpars[["position"]]$y,

      legend    = as.character( coreFrame$legendpars[[ "legend"   ]]),
      adj       = as.numeric(   coreFrame$legendpars[[ "adj"      ]]),
      text.col  = as.character( coreFrame$legendpars[[ "text.col" ]]),
      lty       = as.numeric(   coreFrame$legendpars[[ "lty"      ]]),
      box.lty   = as.numeric(   coreFrame$legendpars[[ "box.lty"  ]]),
      pch       = as.numeric(   coreFrame$legendpars[[ "pch"      ]]),
      border    = as.numeric(   coreFrame$legendpars[[ "border"   ]]),
      bty       = as.character( coreFrame$legendpars[[ "bty"      ]]),
      pt.cex    = as.numeric(   coreFrame$legendpars[[ "pt.cex"   ]]),
      cex       = as.numeric(   coreFrame$legendpars[[ "cex"      ]]),
      bg        = as.character( coreFrame$legendpars[[ "bg"       ]]),
      col       = as.character( coreFrame$legendpars[[ "col"      ]]),
      pt.bg     = as.character( coreFrame$legendpars[[ "pt.bg"    ]]),
      horiz     = as.logical(   coreFrame$legendpars[[ "horiz"    ]])
    )

    return(NA)
  }

  legend(
    coreFrame$legendpars[["position"]],

    legend    = as.character( coreFrame$legendpars[[ "legend"   ]]),
    adj       = as.numeric(   coreFrame$legendpars[[ "adj"      ]]),
    text.col  = as.character( coreFrame$legendpars[[ "text.col" ]]),
    lty       = as.numeric(   coreFrame$legendpars[[ "lty"      ]]),
    box.lty   = as.numeric(   coreFrame$legendpars[[ "box.lty"  ]]),
    pch       = as.numeric(   coreFrame$legendpars[[ "pch"      ]]),
    bty       = as.character( coreFrame$legendpars[[ "bty"      ]]),
    pt.cex    = as.numeric(   coreFrame$legendpars[[ "pt.cex"   ]]),
    cex       = as.numeric(   coreFrame$legendpars[[ "cex"      ]]),
    bg        = as.character( coreFrame$legendpars[[ "bg"       ]]),
    col       = as.character( coreFrame$legendpars[[ "col"      ]]),
    pt.bg     = as.character( coreFrame$legendpars[[ "pt.col"   ]]),
    horiz     = as.logical(   coreFrame$legendpars[[ "horiz"    ]])
  )
}
