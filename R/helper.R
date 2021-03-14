
#' getFAMarkerType
#'
#' Behavior analysts traditionally hold to conventions regarding specific
#' marker types for FA conditions. This function attempts to match the
#' included conditions to a pre-specified marker.
#'
#' @param condition character(1) condition name
#'
#' @return
#' @export
#'
getFAMarkerType = function(condition) {

  condition.upper <- toupper(condition)

  marker = switch(
    condition.upper,
    "TOY PLAY"  = 16,
    "ATTENTION" = 22,
    "DEMAND"    = 24,
    "TANGIBLE"  = 8
  )

  return (marker)
}

#' getFAMarkerColor
#'
#' Behavior analysts traditionally hold to conventions regarding specific
#' marker colors for FA conditions. This function attempts to match the
#' included conditions to a pre-specified colors.
#'
#' @param condition character(1) condition name
#'
#' @return
#' @export
#'
getFAMarkerColor = function(condition) {

  condition.upper <- toupper(condition)

  color = switch(
    condition.upper,
    "TOY PLAY"  = 'black',
    "ATTENTION" = 'white',
    "DEMAND"    = 'white',
    "TANGIBLE"  = 'black'
  )

  return (color)
}



#' cnvrt.coords (Modified)
#'
#' A hacked-up, pared-down, reconstitution of helper methods to draw
#' using both user and figure-level coordinates
#'
#' Author: Greg Snow <538280 at gmail.com>
#' Licensed: Artistic 2.0
#' Published: 2020-04-07
#'
#' @param x numeric(1) abscissa index
#' @param y numeric(1) ordinate index
#' @param input character(1) device
#'
#' @return list: coordinate equivalents across multiple devices
#' @export
#'
cnvrt.coords <- function(x,
                         y     = NULL,
                         input = c('usr','plt','dev')) {
  input <- match.arg(input)

  xy <- xy.coords(x,y, recycle=TRUE)

  cusr <- par('usr')
  cplt <- par('plt')
  cfig <- par('fig')

  if(input=='usr'){
    usr <- xy

    plt <- list()
    plt$x <- (xy$x-cusr[1])/(cusr[2]-cusr[1])
    plt$y <- (xy$y-cusr[3])/(cusr[4]-cusr[3])

    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev) )
  }

  if(input=='plt') {

    plt <- xy

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    fig <- list()
    fig$x <- plt$x*(cplt[2]-cplt[1])+cplt[1]
    fig$y <- plt$y*(cplt[4]-cplt[3])+cplt[3]

    dev <- list()
    dev$x <- fig$x*(cfig[2]-cfig[1])+cfig[1]
    dev$y <- fig$y*(cfig[4]-cfig[3])+cfig[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev) )
  }

  if(input=='dev'){
    dev <- xy

    fig <- list()
    fig$x <- (dev$x-cfig[1])/(cfig[2]-cfig[1])
    fig$y <- (dev$y-cfig[3])/(cfig[4]-cfig[3])

    plt <- list()
    plt$x <- (fig$x-cplt[1])/(cplt[2]-cplt[1])
    plt$y <- (fig$y-cplt[3])/(cplt[4]-cplt[3])

    usr <- list()
    usr$x <- plt$x*(cusr[2]-cusr[1])+cusr[1]
    usr$y <- plt$y*(cusr[4]-cusr[3])+cusr[3]

    return( list( usr=usr, plt=plt, fig=fig, dev=dev) )
  }

}
