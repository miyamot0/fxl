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
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param n name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_arrows <- function(core_frame, current_layer, facet_name) {
  if (current_layer$facet == as.character(facet_name)) {
    for (aindex in seq_along(length(current_layer$arrows))) {
      current_arrow <- current_layer$arrows[[aindex]]

      arrows(x0     = as.numeric(current_arrow[["x0"]]),
             x1     = as.numeric(current_arrow[["x1"]]),
             y0     = as.numeric(current_arrow[["y0"]]),
             y1     = as.numeric(current_arrow[["y1"]]),

             length = as.numeric(current_layer[["length"]]),
             angle  = as.numeric(current_layer[["angle"]]),
             code   = as.numeric(current_layer[["code"]]),
             col    = as.character(current_layer[["color"]]),
             lty    = as.numeric(current_layer[["lty"]]),
             lwd    = as.numeric(current_layer[["lwd"]]))
    }
  }
}

#' draw_brackets
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_brackets <- function(core_frame, current_layer, facet_name) {
  if (current_layer$facet == as.character(facet_name)) {

    for (bindex in seq_along(length(current_layer$brackets))) {
      current_bracket <- current_layer$brackets[[bindex]]

      l_lty <- as.numeric(current_layer[["lty"]])

      if ("lty" %in% names(current_bracket))
        l_lty <- current_bracket$lty

      segments(
        x0          = as.numeric(current_bracket[["x0"]]),
        x1          = as.numeric(current_bracket[["x1"]]),
        y0          = as.numeric(current_bracket[["y0"]]),
        y1          = as.numeric(current_bracket[["y0"]]),

        col         = as.character(current_layer[["color"]]),
        lty         = l_lty,
        lwd         = as.numeric(current_layer[["lwd"]]))

      arrows(x0     = as.numeric(current_bracket[["x0"]]),
             x1     = as.numeric(current_bracket[["x0"]]),
             y0     = as.numeric(current_bracket[["y0"]]),
             y1     = as.numeric(current_bracket[["y1"]]),

             length = as.numeric(current_layer[["length"]]),
             angle  = as.numeric(current_layer[["angle"]]),
             code   = as.numeric(current_layer[["code"]]),
             col    = as.character(current_layer[["color"]]),
             lty    = l_lty,
             lwd    = as.numeric(current_layer[["lwd"]]))

      arrows(x0     = as.numeric(current_bracket[["x1"]]),
             x1     = as.numeric(current_bracket[["x1"]]),
             y0     = as.numeric(current_bracket[["y0"]]),
             y1     = as.numeric(current_bracket[["y1"]]),
             length = as.numeric(current_layer[["length"]]),
             angle  = as.numeric(current_layer[["angle"]]),
             code   = as.numeric(current_layer[["code"]]),
             col    = as.character(current_layer[["color"]]),
             lty    = l_lty,
             lwd    = as.numeric(current_layer[["lwd"]]))
    }
  }
}

#' draw_bar_support
#'
#' Draw bars, but on a secondary axis
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @return
#' @export
draw_bar_support <- function(core_frame,  current_layer, facet_name) {

  if (is.na(facet_name))  current_data   <- core_frame$data
  else                   current_data   <- core_frame$data[which(
    core_frame$data[, as.character(core_frame$aes["facet"])] == facet_name), ]

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] <- "p"
    current_data[, "p"] <- "0"
  }

  local_aesthetics <- list(
    "x" = as.character(core_frame$aes["x"]),
    "y" = as.character(core_frame$aes["y"])
  )

  if (!is.na(current_layer["aesthetics"])) {
    local_aesthetics <- list(
      "x" = as.character(current_layer$aesthetics["x"]),
      "y" = as.character(current_layer$aesthetics["y"])
    )
  }

  label.y <- as.character(local_aesthetics["y"])

  if (current_layer$label != "")  label.y <- as.character(current_layer$label)

  opar <- par()

  # Add on to "new" plot
  par(new = TRUE,
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  plot(NULL,
       axes = FALSE,
       xlim = c(core_frame$dims[["min.local.x"]],
                core_frame$dims[["max.local.x"]]),
       ylim = c(0, 100),
       xlab = "",
       ylab = "",
       frame.plot = FALSE,
       las = 1,
       xaxt = "n",
       yaxt = "n")

  box(bty = "U")

  p_off <- current_layer$width / 2

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {

    current_data_slice <- current_data[which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    for (row in seq_len(nrow(current_data_slice))) {

      rect(current_data_slice[row, as.character(local_aesthetics["x"])] - p_off,
           0,
           current_data_slice[row, as.character(local_aesthetics["x"])] + p_off,
           current_data_slice[row, as.character(local_aesthetics["y"])],
           col = current_layer$color)
    }
  }

  axis(side = 4,
       las = 1,
       at = pretty(range(c(0, current_data_slice[, as.character(local_aesthetics["y"])]))))

  mtext(label.y,
        side = 4,
        outer = TRUE)

  par(opar)
}

#' draw_guide_line
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @return
#' @export
draw_guide_line <- function(core_frame,  current_layer, facet_name) {

  if (is.na(facet_name) | current_layer$facet == facet_name) {

    for (gindex in 1:length(current_layer$coords)) {
      current_coords <- current_layer$coords[[gindex]]

      l_col <- as.character(current_layer[["col"]])
      l_lty <- as.numeric(current_layer[["lty"]])
      l_lwd <- as.numeric(current_layer[["lwd"]])

      if ("col" %in% names(current_coords))
        l_col <- current_coords$col

      if ("lty" %in% names(current_coords))
        l_lty <- current_coords$lty

      if ("lwd" %in% names(current_coords))
        l_lwd <- current_coords$lwd

      temp_y2 <- ifelse("y1" %in% names(current_coords),
        current_coords$y1,
        current_coords$y0)

      segments(
        x0  = current_coords$x0,
        x1  = current_coords$x1,
        y0  = current_coords$y0,
        y1  = temp_y2,
        lty = l_lty,
        lwd = l_lwd,
        col = l_col
      )
    }
  }
}

#' draw_lines
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_lines <- function(core_frame, current_layer, facet_name) {
  if (is.na(facet_name)) current_data = core_frame$data
  else current_data = core_frame$data[
    which(core_frame$data[, as.character(core_frame$aes["facet"])] == facet_name), ]

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] = "p"
    current_data[, "p"] = "0"
  }

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {
    current_data_slice <- current_data[
      which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    local_aesthetics = list(
      "x"   = as.character(core_frame$aes["x"]),
      "y"   = as.character(core_frame$aes["y"])
    )

    if (!is.na(current_layer["aesthetics"])) {
      local_aesthetics = list(
        "x" = as.character(current_layer$aesthetics["x"]),
        "y" = as.character(current_layer$aesthetics["y"])
      )
    }

    plot_frame = data.frame(
      X = current_data_slice[, as.character(local_aesthetics["x"])],
      Y = current_data_slice[, as.character(local_aesthetics["y"])]
    )

    lines(
      plot_frame$X,
      plot_frame$Y,
      lty   = current_layer$lty,
      col   = current_layer$color,
      lwd   = current_layer$size
    )
  }
}

#' draw_cumsum_lines
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @return
#' @export
draw_cumsum_lines <- function(core_frame, current_layer, facet_name) {

  if (is.na(facet_name)) current_data = core_frame$data
  else current_data = core_frame$data[
    which(core_frame$data[, as.character(core_frame$aes["facet"])] == facet_name), ]

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] = "p"
    current_data[, "p"] = "0"
  }

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {
    current_data_slice <- current_data[
      which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    local_aesthetics = list(
      "x"   = as.character(core_frame$aes["x"]),
      "y"   = as.character(core_frame$aes["y"])
    )

    if (!is.na(current_layer["aesthetics"])) {
      local_aesthetics <- list(
        "x" = as.character(current_layer$aesthetics["x"]),
        "y" = as.character(current_layer$aesthetics["y"])
      )
    }

    cumsumy = current_data_slice[, as.character(local_aesthetics["y"])]
    cumsumy[is.na(cumsumy)] = 0
    cumsumy = cumsum(cumsumy)

    lines(
      current_data_slice[, as.character(local_aesthetics["x"])],
      cumsumy,
      lty   = current_layer$lty,
      col   = current_layer$color,
      lwd   = current_layer$size
    )
  }
}

#' draw_cumsum_points
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @return
#' @export
draw_cumsum_points <- function(core_frame, current_layer, facet_name) {

  if (is.na(facet_name))  current_data   = core_frame$data
  else           current_data   = core_frame$data[which(
    core_frame$data[, as.character(core_frame$aes["facet"])] == facet_name), ]

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] = "p"
    current_data[, "p"] = "0"
  }

  local_aesthetics = list(
    "x" = as.character(core_frame$aes["x"]),
    "y" = as.character(core_frame$aes["y"])
  )

  if (!is.na(current_layer["aesthetics"])) {
    local_aesthetics = list(
      "x" = as.character(current_layer$aesthetics["x"]),
      "y" = as.character(current_layer$aesthetics["y"])
    )
  }

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {

    current_data_slice <- current_data[which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    local_aesthetics = list(
      "x"   = as.character(core_frame$aes["x"]),
      "y"   = as.character(core_frame$aes["y"])
    )

    if (!is.na(current_layer["aesthetics"])) {
      local_aesthetics = list(
        "x" = as.character(current_layer$aesthetics["x"]),
        "y" = as.character(current_layer$aesthetics["y"])
      )
    }

    pch = 1

    if (is.list(current_layer$pch))  pch = current_layer$pch[[p]]
    else                            pch = current_layer$pch

    fill = "black"

    if (is.list(current_layer$fill)) fill = current_layer$fill[[p]]
    else                            fill = current_layer$fill

    cumsumy = current_data_slice[, as.character(local_aesthetics["y"])]
    cumsumy[is.na(cumsumy)] = 0
    cumsumy = cumsum(cumsumy)

    points(
      current_data_slice[, as.character(local_aesthetics["x"])],
      cumsumy,
      pch = pch,
      cex = current_layer$cex,
      bg  = fill
    )
  }
}

#' draw_label_phase
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_label_phase <- function(core_frame, current_layer, facet_name) {
  if (current_layer$facet == facet_name) {

    for (lindex in 1:length(current_layer$labels)) {

      label        = names(current_layer$labels)[lindex]

      current_label = current_layer$labels[[lindex]]

      label = ifelse(is.null(label),
                     current_label,
                     label)

      temp_x = ifelse("x" %in% names(current_label),
                     current_label[["x"]],
                     current_layer$x)

      temp_y = ifelse("y" %in% names(current_label),
                     current_label[["y"]],
                     current_layer$y)

      font_c = ifelse("font" %in% names(current_label),
                     current_label[["font"]],
                     1)

      srt          = ifelse("srt" %in% names(current_label),
                            current_label[["srt"]],
                            0)

      text(x      = temp_x,
           y      = temp_y,
           cex    = current_layer[["cex"]],
           adj    = current_layer[["adj"]],
           font   = font_c,
           srt    = srt,
           labels = label)
    }
  }
}

#' draw_points
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#' @param zeroAxis filter out all but zeros
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_points <- function(core_frame, current_layer, facet_name, zeroAxis = FALSE) {

  if (is.na(facet_name))  current_data   = core_frame$data
  else           current_data   = core_frame$data[which(
    core_frame$data[, as.character(core_frame$aes["facet"])] == facet_name), ]

  # In case no phases are included?
  if (!("p" %in% names(core_frame$aes))) {
    core_frame$aes["p"] = "p"
    current_data[, "p"] = "0"
  }

  local_aesthetics = list(
    "x" = as.character(core_frame$aes["x"]),
    "y" = as.character(core_frame$aes["y"])
  )

  if (!is.na(current_layer["aesthetics"])) {
    local_aesthetics = list(
      "x" = as.character(current_layer$aesthetics["x"]),
      "y" = as.character(current_layer$aesthetics["y"])
    )
  }

  for (p in unique(current_data[, as.character(core_frame$aes["p"])])) {

    current_data_slice <- current_data[which(current_data[, as.character(core_frame$aes["p"])] == p), ]

    local_aesthetics = list(
      "x"   = as.character(core_frame$aes["x"]),
      "y"   = as.character(core_frame$aes["y"])
    )

    if (!is.na(current_layer["aesthetics"])) {
      local_aesthetics = list(
        "x" = as.character(current_layer$aesthetics["x"]),
        "y" = as.character(current_layer$aesthetics["y"])
      )
    }

    pch = 1

    if (is.list(current_layer$pch))  pch = current_layer$pch[[p]]
    else                            pch = current_layer$pch

    fill = "black"

    if (is.list(current_layer$fill)) fill = current_layer$fill[[p]]
    else                            fill = current_layer$fill

    col = "black"

    if (is.list(current_layer$color)) col = current_layer$color[[p]]
    else                             col = current_layer$color

    cex = 1

    if (is.list(current_layer$cex)) cex = current_layer$cex[[p]]
    else                           cex = current_layer$cex

    plot_frame = data.frame(
      X = current_data_slice[, as.character(local_aesthetics["x"])],
      Y = current_data_slice[, as.character(local_aesthetics["y"])]
    )

    if (zeroAxis) plot_frame = plot_frame[plot_frame$Y == 0, ]

    points(
      plot_frame$X,
      plot_frame$Y,
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
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_label_facet <- function(core_frame, current_layer, facet_name) {
  current_label = current_layer$labels[[as.character(facet_name)]]

  label = facet_name
  custom_label = FALSE

  if ("label" %in% names(current_label)) {
    label       = current_label[["label"]]
    custom_label = TRUE
  }

  if (label == "" | label == facet_name | custom_label == TRUE) {

    temp_x = ifelse(!is.null(current_label[["x"]]),
                   current_label[["x"]],
                   current_layer$x)

    temp_y = ifelse(!is.null(current_label[["y"]]),
                   current_label[["y"]],
                   current_layer$y)

    font_c = ifelse("font" %in% names(current_label),
                   current_label[["font"]],
                   1)

    text(x      = temp_x,
         y      = temp_y,
         cex    = current_layer[["cex"]],
         adj    = current_layer[["adj"]],
         font   = font_c,
         labels = label)
  }
}

#' draw_scr_plines
#'
#' drawing function
#'
#' @param core_frame fxl object
#' @param current_layer layer to be drawn
#' @param facet_name name of facet
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export
draw_scr_plines <- function(core_frame, current_layer, facet_name) {
  if (as.character(facet_name) %in% names(current_layer$lines)) {
    for (key in names(current_layer$lines[[facet_name]])) {

      l_lty = current_layer[["lty"]]
      l_x1  = current_layer$lines[[facet_name]][[key]][['x1']]
      l_x2  = ifelse(is.null(current_layer$lines[[facet_name]][[key]][['x2']]),
                     l_x1,
                     current_layer$lines[[facet_name]][[key]][['x2']])

      l_y1  = ifelse(is.null(current_layer$lines[[facet_name]][[key]][['y1']]),
                     0,
                     current_layer$lines[[facet_name]][[key]][['y1']])
      l_y2  = ifelse(is.null(current_layer$lines[[facet_name]][[key]][['y2']]),
                     0,
                     current_layer$lines[[facet_name]][[key]][['y2']])

      temp_y1 = ifelse(l_y1 == 0,
                      -((as.numeric(core_frame$dims[["max.local.y"]]) -
                          as.numeric(core_frame$dims[["min.local.y"]])) * 0.04),
                      current_layer$lines[[facet_name]][[key]][['y1']])

      temp_y2 = ifelse(l_y2 == 0,
                      -((as.numeric(core_frame$dims[["max.local.y"]]) -
                          as.numeric(core_frame$dims[["min.local.y"]])) * 0.04),
                      l_y2)

      if ("lty" %in% names(current_layer$lines[[facet_name]][[key]]))
        l_lty = current_layer$lines[[facet_name]][[key]]$lty

      lines(
        c(l_x1, l_x2),
        c(temp_y1, temp_y2),
        lty = l_lty
      )
    }
  }
}

#' draw_legend
#'
#' drawing function
#'
#' @param core_frame fxl object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
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
