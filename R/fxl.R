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

### EXPORTS ###

#' scrplot
#'
#' Core object for establishing fxl object and layers
#'
#' @importFrom rlang enexpr
#' @param data submitted data (not opinionated on naming)
#' @param aesthetics references for data in frame
#' @param mai margins in inches
#' @param omi outer margins in inches
#' @param xaxs x axis formatting, relative to hanging space
#' @param yaxs y axis formatting, relative to hanging space
#' @param ncol TODO
#' @param family font family
#' @param bty TODO
#' @param layout TODO
#' @param layout_h TODO
#' @param layout_v TODO
#' @param semi_color_major_y TODO
#' @param semi_color_midpoint_y TODO
#' @param semi_color_minor_y TODO
#' @param semi_color_major_x TODO
#' @param semilog determine if this is a semilog type of plot
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @returns class of 'fxl' that contains necessary plotting elements
#' @export
scr_plot <- function(data,
                     aesthetics = NULL,
                     mai = c(
                       0.375,
                       0.375,
                       0.25,
                       0.25
                     ),
                     omi = c(
                       0.25,
                       0.25,
                       0.25,
                       0.25
                     ),
                     xaxs = "i",
                     yaxs = "i",
                     ncol = 1,
                     family = "sans",
                     bty = "l",
                     layout = NA,
                     layout_h = NA,
                     layout_v = NA,
                     semi_color_major_y = "blue",
                     semi_color_midpoint_y = "blue",
                     semi_color_minor_y = "lightgray",
                     semi_color_major_x = "lightgray",
                     semilog = FALSE) {
  # Type checks
  isValidDataFrame(
    object = data,
    name = "data"
  )
  isValidAestheticMapping(
    aesthetics,
    name = "aesthetics"
  )
  isValidNumericVector(
    object = mai,
    length = 4,
    name = "mai"
  )
  isValidNumericVector(
    object = omi,
    length = 4,
    name = "omi"
  )
  isValidAXSCharacter(
    xaxs,
    "xaxs"
  )
  isValidAXSCharacter(
    yaxs,
    "yaxs"
  )
  isValidNumericVector(
    object = ncol,
    name = "ncol"
  )
  isValidCharacterVector(
    object = family,
    name = "family"
  )
  isValidLogicalVector(
    object = semilog,
    length = 1,
    name = "semilog"
  )

  core_frame <- list()
  core_frame[["layers"]] <- list() # Layers for drawing
  core_frame[["aes"]] <- enexpr(aesthetics) # Mappings
  core_frame[["data"]] <- data # Stored data
  core_frame[["dims"]] <- list( # Global dimensions
    global.max.x = max(
      data[[as.character(core_frame$aes["x"])]],
      na.rm = TRUE
    ),
    global.min.x = min(
      data[[as.character(core_frame$aes["x"])]],
      na.rm = TRUE
    ),
    global.max.y = max(
      data[[as.character(core_frame$aes["y"])]],
      na.rm = TRUE
    ),
    global.min.y = 0,
    mai = mai,
    omi = omi,
    xaxs = xaxs,
    yaxs = yaxs,
    ncol = ncol,
    xdelta = 1,
    ydelta = 1
  )

  core_frame[["labs"]] <- list( # Presumed labels, blank title by default
    xlab         = as.character(core_frame$aes["x"]),
    ylab         = as.character(core_frame$aes["y"]),
    outer        = TRUE,
    outer.x.line = 0,
    outer.y.line = 0,
    title        = ""
  )

  core_frame[["family"]] <- family

  core_frame$labs[["ylab_color"]] <- "black"
  core_frame$labs[["ylab_cex"]] <- 1
  core_frame$labs[["ylab_adj"]] <- 0.5
  core_frame$labs[["ylab_face"]] <- 1

  core_frame$labs[["xlab_color"]] <- "black"
  core_frame$labs[["xlab_cex"]] <- 1
  core_frame$labs[["xlab_adj"]] <- 0.5
  core_frame$labs[["xlab_face"]] <- 1

  core_frame$labs[["title_color"]] <- "black"
  core_frame$labs[["title_cex"]] <- 1
  core_frame$labs[["title_adj"]] <- 0.5
  core_frame$labs[["title_face"]] <- 1

  core_frame[["layout"]] <- layout
  core_frame[["layout_h"]] <- layout_h
  core_frame[["layout_v"]] <- layout_v

  class(core_frame) <- c("fxl")

  if (semilog) {
    class(core_frame) <- c("fxlsemilog")

    assert_input_type(semi_color_major_y, "character", "semi_color_major_y")
    core_frame[["semi_color_major_y"]] <- semi_color_major_y

    assert_input_type(semi_color_midpoint_y, "character", "semi_color_midpoint_y")
    core_frame[["semi_color_midpoint_y"]] <- semi_color_midpoint_y

    assert_input_type(semi_color_minor_y, "character", "semi_color_minor_y")
    core_frame[["semi_color_minor_y"]] <- semi_color_minor_y

    assert_input_type(semi_color_major_x, "character", "semi_color_major_x")
    core_frame[["semi_color_major_x"]] <- semi_color_major_x

    assert_input_type(bty, "character", "bty")
    core_frame[["bty"]] <- bty
  }

  core_frame
}
