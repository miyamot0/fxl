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

#' cnvrt_coords
#'
#' Pulled from the TeachingDemos package (GPLv2+ Licensed)
#'
#' Slightly hacked/trimmed
#'
#' @param x abscissa
#' @param y ordnate
#' @param input device
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>, Greg Snow <538280@@gmail.com>
#'
#' @return
#' @export
cnvrt_coords <- function(x,
                         y = NULL,
                         input = c("usr",
                                   "plt",
                                   "fig",
                                   "dev",
                                   "tdev")) {
  #warning("this function is now depricated, use grconvertX instead")
  input <- match.arg(input)
  xy <- xy.coords(x, y, recycle = TRUE)

  cusr <- par("usr")
  cplt <- par("plt")
  cfig <- par("fig")
  cdin <- par("din")
  comi <- par("omi")
  cdev <- c(comi[2] / cdin[1], (cdin[1] - comi[4]) / cdin[1],
            comi[1] / cdin[2], (cdin[2] - comi[3]) / cdin[2])

  if (input == "usr") {
    usr <- xy

    plt <- list()
    plt$x <- (xy$x - cusr[1]) / (cusr[2] - cusr[1])
    plt$y <- (xy$y - cusr[3]) / (cusr[4] - cusr[3])

    fig <- list()
    fig$x <- plt$x * (cplt[2] - cplt[1]) + cplt[1]
    fig$y <- plt$y * (cplt[4] - cplt[3]) + cplt[3]

    dev <- list()
    dev$x <- fig$x * (cfig[2] - cfig[1]) + cfig[1]
    dev$y <- fig$y * (cfig[4] - cfig[3]) + cfig[3]

    tdev <- list()
    tdev$x <- dev$x * (cdev[2] - cdev[1]) + cdev[1]
    tdev$y <- dev$y * (cdev[4] - cdev[3]) + cdev[3]

    return(list(usr = usr, plt = plt, fig = fig, dev = dev, tdev = tdev))
  }

  if (input == "plt") {

    plt <- xy

    usr <- list()
    usr$x <- plt$x * (cusr[2] - cusr[1]) + cusr[1]
    usr$y <- plt$y * (cusr[4] - cusr[3]) + cusr[3]

    fig <- list()
    fig$x <- plt$x * (cplt[2] - cplt[1]) + cplt[1]
    fig$y <- plt$y * (cplt[4] - cplt[3]) + cplt[3]

    dev <- list()
    dev$x <- fig$x * (cfig[2] - cfig[1]) + cfig[1]
    dev$y <- fig$y * (cfig[4] - cfig[3]) + cfig[3]

    tdev <- list()
    tdev$x <- dev$x * (cdev[2] - cdev[1]) + cdev[1]
    tdev$y <- dev$y * (cdev[4] - cdev[3]) + cdev[3]

    return(list(usr = usr, plt = plt, fig = fig, dev = dev, tdev = tdev))
  }

  if (input == "fig") {

    fig <- xy

    plt <- list()
    plt$x <- (fig$x - cplt[1]) / (cplt[2] - cplt[1])
    plt$y <- (fig$y - cplt[3]) / (cplt[4] - cplt[3])

    usr <- list()
    usr$x <- plt$x * (cusr[2] - cusr[1]) + cusr[1]
    usr$y <- plt$y * (cusr[4] - cusr[3]) + cusr[3]

    dev <- list()
    dev$x <- fig$x * (cfig[2] - cfig[1]) + cfig[1]
    dev$y <- fig$y * (cfig[4] - cfig[3]) + cfig[3]

    tdev <- list()
    tdev$x <- dev$x * (cdev[2] - cdev[1]) + cdev[1]
    tdev$y <- dev$y * (cdev[4] - cdev[3]) + cdev[3]

    return(list(usr = usr, plt = plt, fig = fig, dev = dev, tdev = tdev))
  }

  if (input == "dev") {
    dev <- xy

    fig <- list()
    fig$x <- (dev$x - cfig[1]) / (cfig[2] - cfig[1])
    fig$y <- (dev$y - cfig[3]) / (cfig[4] - cfig[3])

    plt <- list()
    plt$x <- (fig$x - cplt[1]) / (cplt[2] - cplt[1])
    plt$y <- (fig$y - cplt[3]) / (cplt[4] - cplt[3])

    usr <- list()
    usr$x <- plt$x * (cusr[2] - cusr[1]) + cusr[1]
    usr$y <- plt$y * (cusr[4] - cusr[3]) + cusr[3]

    tdev <- list()
    tdev$x <- dev$x * (cdev[2] - cdev[1]) + cdev[1]
    tdev$y <- dev$y * (cdev[4] - cdev[3]) + cdev[3]

    return(list(usr = usr, plt = plt, fig = fig, dev = dev, tdev = tdev))
  }

  if (input == "tdev") {
    tdev <- xy

    dev <- list()
    dev$x <- (tdev$x - cdev[1]) / (cdev[2] - cdev[1])
    dev$y <- (tdev$y - cdev[3]) / (cdev[4] - cdev[3])

    fig <- list()
    fig$x <- (dev$x - cfig[1]) / (cfig[2] - cfig[1])
    fig$y <- (dev$y - cfig[3]) / (cfig[4] - cfig[3])

    plt <- list()
    plt$x <- (fig$x - cplt[1]) / (cplt[2] - cplt[1])
    plt$y <- (fig$y - cplt[3]) / (cplt[4] - cplt[3])

    usr <- list()
    usr$x <- plt$x * (cusr[2] - cusr[1]) + cusr[1]
    usr$y <- plt$y * (cusr[4] - cusr[3]) + cusr[3]

    tdev <- list()
    tdev$x <- dev$x * (cdev[2] - cdev[1]) + cdev[1]
    tdev$y <- dev$y * (cdev[4] - cdev[3]) + cdev[3]

    return(list(usr = usr,
                plt = plt,
                fig = fig,
                dev = dev,
                tdev = tdev))
  }
}
