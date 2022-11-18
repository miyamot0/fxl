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
#' @param semilog determine if this is a semilog type of plot
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export
scr_plot <- function(data,
                     aesthetics = NULL,
                     mai = c(0.375,
                             0.375,
                             0.25,
                             0.25),
                     omi = c(0.25,
                             0.25,
                             0.25,
                             0.25),
                     xaxs = "i",
                     yaxs = "i",
                     ncol = 1,
                     family = "sans",
                     semilog = FALSE) {

  # Type checks
  isValidDataFrame(
    object = data,
    name = "data"
  )
  isValidAestheticMapping(
    aesthetics,
    name = 'aesthetics'
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
  core_frame[["dims"]] <- list(# Global dimensions
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
    global.min.y = min(
      data[[as.character(core_frame$aes["y"])]],
      na.rm = TRUE
    ),
    mai    = mai,
    omi    = omi,
    xaxs   = xaxs,
    yaxs   = yaxs,
    ncol   = ncol,
    xdelta = 1,
    ydelta = 1
  )

  core_frame[["labs"]] <- list(# Presumed labels, blank title by default
    xlab         = as.character(core_frame$aes["x"]),
    ylab         = as.character(core_frame$aes["y"]),
    outer        = TRUE,
    outer.x.line = 0,
    outer.y.line = 0,
    title        = ""
  )

  core_frame[["family"]] <- family

  class(core_frame) <- c("fxl")  # Apply a class name (to override print)

  if (semilog) class(core_frame) <- c("fxlsemilog")

  core_frame
}
