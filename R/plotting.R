

#' plotMultiElementFA
#'
#' @param data data frame of assessment information
#' @param grouping column name, for grouping, referring to FA condition
#' @param session column name, for session numbering (should be distinct)
#' @param response column name, for response to chart
#' @param title plot title
#' @param ylab plot axis title
#' @param xlab plot axis title
#' @param ylims override ylims
#' @param legend.horizonal override legend aes
#' @param legend.position override legend aes
#'
#' @return
#' @export
#'
#' @examples
#'
#' plotMultiElementFA(Gilroyetal2019,
#' grouping = 'Condition',
#' session  = 'Session',
#' response = 'CTB',
#' title    = 'Functional Analysis',
#' xlab     = 'Session',
#' ylab     = 'Frequency (Responses, Reinforcers Delivered)',
#' ylims    = c(0, 4),
#' legend.position = 'topright')
#'
plotMultiElementFA <- function(data = NULL,    grouping = NULL,
                               session = NULL, response = NULL,
                               title,  ylab, xlab, ylims,
                               legend.horizonal = NULL,
                               legend.position = NULL)
{

  ### Sanity checks ###

  if (is.null(data))                   stop("No data was supplied")
  if (!(grouping %in% colnames(data))) stop("The grouping column was not found")
  if (!(session  %in% colnames(data))) stop("The session column was not found")
  if (!(response %in% colnames(data))) stop("The response column was not found")

  #####################

  conds = unique(data[,grouping])

  minY <- min(data[,response])
  maxY <- max(data[,response])

  minX <- min(data[,session])
  maxX <- max(data[,session])

  X    <- minX:maxX

  # mai
  marginInches = c(0.375,
                   0.375,
                   0.25,
                   0.25)

  # omi
  outMarInches = c(0.25,
                   0.25,
                   0.25,
                   0.25)

  par(mfrow  = c(1,1),
      family = "serif",
      omi    = outMarInches,
      mai    = marginInches,
      xaxs   = "r",
      yaxs   = "r")

  isFirst <- TRUE

  for (condition in conds) {
    currentData = data[which(data[,grouping] == condition), ]

    x <- currentData[,session]
    y <- currentData[,response]

    if (isFirst) {

      if (missing(ylims))
        yminvec <- c(minY, maxY)
      else
        yminvec <- ylims

      plot(x, y,
           ylim       = yminvec,
           xlim       = c(minX, maxX),
           type       = 'l',
           xaxt       = 'n',
           cex        = 2,
           las        = 1,
           frame.plot = FALSE)

      axis(1,
           labels = TRUE,
           at = X)

      box(bty = "l")
    } else {
      lines(x, y)
    }

    points(x, y,
           pch = getFAMarkerType(condition),
           bg  = getFAMarkerColor(condition),
           cex = 2)

    isFirst <- FALSE
  }

  if (!missing(title)) mtext(title, side=3, outer=TRUE, line=0)
  if (!missing(ylab))  mtext(ylab,  side=2, outer=TRUE)
  if (!missing(xlab))  mtext(xlab,  side=1, outer=TRUE)

  styleFrame <- data.frame(
    Condition  = conds,
    Marker     = NA,
    Fill       = NA
  )

  for (r in 1:nrow(styleFrame)) {
    styleFrame[r, "Marker"] = getFAMarkerType( styleFrame[r, "Condition"])
    styleFrame[r, "Fill"]   = getFAMarkerColor(styleFrame[r, "Condition"])
  }

  leg.pos = 'right'
  leg.h   = F
  leg.c   = conds
  leg.bg  = styleFrame$Fill
  leg.pch = styleFrame$Marker
  leg.lty = rep(1, length(conds))
  leg.ctx = 1.0

  if (!missing(legend.horizonal)) leg.pos = legend.horizonal
  if (!missing(legend.position))  leg.pos = legend.position

  legend(leg.pos,
         legend   = leg.c,
         pt.bg    = leg.bg,
         lty      = leg.lty,
         box.lty  = 0,
         pch      = leg.pch,
         cex      = leg.ctx,
         horiz    = leg.h)
}
