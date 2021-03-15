

#' plotMultiElementFA
#'
#' @param data data.frame of assessment information
#' @param grouping character(1) column name, for grouping, referring to FA condition
#' @param session character(1) column name, for session numbering (should be distinct)
#' @param response character(1) column name, for response to chart
#' @param title character(1) plot title
#' @param ylab character(1) plot axis title
#' @param xlab character(1) plot axis title
#' @param ylims numeric(n) override ylims
#' @param legend.horizonal logical(1) override legend aes
#' @param legend.position character(1) override legend aes
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

#' plotMultipleBaseline
#'
#' @param data dataframe of assessment information
#' @param grouping character(1) column name, for grouping, referring to FA condition
#' @param session character(1) column name, for session numbering (should be distinct)
#' @param response character(1) column name, for response to chart
#' @param response2 character(1) column name, for response to chart
#' @param pnum character(1) column name, for numbered phases
#' @param poff character(1) column name, for offsetting overlapped phase connections
#' @param clabs data.frame condition labels
#' @param plabs data.frame phase labels
#' @param llabs data.frame legend stuff
#' @param title character(1) plot title
#' @param ylab character(1) plot axis title
#' @param xlab character(1) plot axis title
#' @param ymins numeric(n) override ylims
#' @param ymaxs numeric(n) override ylims
#' @param xmax numeric(1) override xlims
#'
#' @return
#' @export
#'
#' @examples
#'
#' conditionLabel <- data.frame(
#' Panel = rep("Andrew", 4),
#' X     = c(3.5,
#'           9,
#'           19,
#'           27),
#' Y     = rep(100, 4),
#' Cex   = rep(1.25, 4),
#' Text  = c("Baseline",
#'           "Training",
#'           "Post-Training",
#'           "Generalization")
#' )
#'
#' panelLabel <- data.frame(
#'   Panel = c("Andrew",
#'             "Brian",
#'             "Charles"),
#'   X     = rep(27, 3),
#'   Y     = c(0,
#'             0,
#'             0),
#'   Cex   = rep(1.5, 3),
#'   Text  = c("Andrew",
#'             "Brian",
#'             "Charles")
#' )
#'
#' # Roundabout way to 'prompt' drawing of lines in correct locations
#' dataFrameAdds <- data.frame(
#'   Participant	= c("Andrew",
#'                   "Brian",
#'                   "Charles"),
#'   Session	    = c(23,
#'                    23,
#'                    23),
#'   Condition	  = c("Generalization",
#'                   "Maintenance",
#'                   "Maintenance"),
#'   Responding	= c(NA,
#'                  NA,
#'                  NA),
#'   PhaseNum	  = c(3,3,4),
#'   LineOff     = c(0,0,0)
#' )
#'
#' Gilroyetal2015 <- rbind(Gilroyetal2015,
#'                         dataFrameAdds)
#'
#' plotMultipleBaseline(data     = Gilroyetal2015,
#'                      grouping = 'Participant',
#'                      session  = 'Session',
#'                      response = 'Responding',
#'                      pnum     = 'PhaseNum',
#'                      poff     = 'LineOff',
#'                      ymins    = list("Andrew"    = 0,
#'                                      "Brian" = 0,
#'                                      "Charles" = 0),
#'                      ymaxs    = list("Andrew"    = 100,
#'                                      "Brian" = 100,
#'                                      "Charles" = 100),
#'                      title    = 'Rates of Acquisition across Participants',
#'                      xlab     = 'Session',
#'                      xmax     = 27,
#'                      ylab     = 'Percent Accuracy',
#'                      clabs    = conditionLabel,
#'                      plabs    = panelLabel)
#'
plotMultipleBaseline <- function(data = NULL, grouping = NULL, session = NULL,
                                 response = NULL, response2 = NULL,
                                 pnum = NULL, poff = NULL,
                                 clabs= NULL, plabs = NULL, llabs = NULL,
                                 title,  ylab, xlab, ymins, ymaxs, xmax) {

  if (is.null(data))                   stop("No data was supplied")
  if (is.null(clabs))                  stop("No condition labels supplied")
  if (is.null(plabs))                  stop("No panel labels supplied")
  if (!(grouping %in% colnames(data))) stop("The grouping column was not found")
  if (!(session  %in% colnames(data))) stop("The session column was not found")
  if (!(response %in% colnames(data))) stop("The response column was not found")

  facetNms  <- unique(data[,grouping])
  nRows     <- length(facetNms)
  nPhases   <- max(data[,pnum]) - 1
  xMin      <- min(data[,session])

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

  if (missing(xmax))
    nSessions <- max(data[,session])
  else
    nSessions <- xmax

  yMin      <- min(data[,response])

  if (!missing(response2)) {
    if (!(response %in% colnames(data))) stop("The second response was not found")

    yMin2   <- min(data[,response2])
    yMin    <- ifelse(yMin2 < yMin,
                      yMin2,
                      yMin)
  }

  if (!missing(ymins)) {
    if(length(ymins) != length(facetNms))
      stop("Ymins of incorrect length")
  }

  if (!missing(ymaxs)) {
    if(length(ymaxs) != length(facetNms))
      stop("Ymaxs of incorrect length")
    else
      maxYs <- ymaxs
  }

  plotDims    <- c(nRows,
                   1)
  X           <- seq(xMin,
                     nSessions)
  panelData   <- list()
  phaseShifts <- list()
  maxYs       <- list()
  plotTops    <- list()
  plotBots    <- list()

  for(p in facetNms) {
    subs           <- data[which(data[,grouping] == p), ]
    panelData[[p]] <- subs
    maxYs[[p]]     <- ifelse(missing(response2),
                             max(subs[,response], na.rm = TRUE),
                             max(c(as.numeric(subs[,response]),
                                   as.numeric(subs[,response2])), na.rm = TRUE))
  }

  par(mfrow  = plotDims,
      family = "serif",
      omi    = outMarInches,
      mai    = marginInches,
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  for(p in facetNms) {
    data    <- panelData[[p]]
    conds   <- clabs[clabs$Panel == p,]
    pans    <- plabs[plabs$Panel == p,]
    legs    <- llabs[llabs$panel == p,]

    pl.ind  <- list()
    plt.top <- list()
    plt.bot <- list()

    x       <- data[,session]
    y       <- data[,response]
    ps      <- data[,pnum]

    shouldShowSessions <- p == facetNms[length(facetNms)]

    if (!missing(ymins))
      yMin <- ymins[[p]]

    if (!missing(ymaxs))
      yMax <- ymaxs[[p]]
    else
      yMax <- maxYs[[p]]

    if (shouldShowSessions) {
      deltaY <- (yMax - yMin) * 0.04
    }

    plot(x,y,
         pch        = 19,
         xlab       = "",
         xlim       = c(xMin, nSessions),
         ylim       = c(yMin, yMax),
         ylab       = "",
         xaxt       = 'n',
         type       = 'p',
         cex        = 2,
         las        = 1,
         frame.plot = FALSE)

    axis(1,
         labels     = shouldShowSessions,
         at         = X)

    box(bty         = "l")

    ### Conditions tags

    if (nrow(conds) > 0)
      for (c in 1:nrow(conds)) {
        text(conds[c, "X"],
             conds[c, "Y"],
             conds[c, "Text"],
             cex = conds[c, "Cex"],
             adj = c(1, 0))
      }

    if (nrow(pans) > 0)
      for (panNum in 1:nrow(pans)) {
        text(pans[panNum, "X"],
             pans[panNum, "Y"],
             pans[panNum, "Text"],
             cex = pans[panNum, "Cex"],
             adj = c(1, 0))
      }

    isFirst <- TRUE
    last.X  <- NA
    offsetYMax <- 0

    for (ls in unique(ps)) {
      pdata <- data[which(data[,pnum] == ls), ]
      lines(pdata[,session], pdata[,response])

      # Plot alternative response, if necessary
      if (!missing(response2)) {
        points(pdata[,session],
               pdata[,response2],
               pch = 2,
               cex = 2.5)
        lines(pdata[,session],
              pdata[,response2],
              lty = 2)
      }

      ### the first begins at 0, so skip it
      if (!isFirst) {
        pl.ind[ls-1] = (last.X + min(pdata[[session]])) / 2
        plt.top[[ls-1]] = cnvrt.coords(pl.ind[ls-1], yMax - offsetYMax)

        yOff <- ifelse(p == facetNms[length(facetNms)],
                       -deltaY,
                       yMin)

        plt.bot[[ls-1]] = cnvrt.coords(pl.ind[ls-1], yOff)
      }

      isFirst <- FALSE
      last.X <- max(pdata[[session]])
      offsetYMax <- pdata[1,poff]
    }

    phaseShifts[[p]] = pl.ind
    phaseShifts$num = length(unique(ps))

    plotTops[[p]] = plt.top
    plotBots[[p]] = plt.bot
  }

  if (!missing(title)) mtext(title, side=3, outer=TRUE, line=0)
  if (!missing(ylab))  mtext(ylab,  side=2, outer=TRUE)
  if (!missing(xlab))  mtext(xlab,  side=1, outer=TRUE)

  ### Loop across panels
  for (panel in facetNms) {
    pts <- plotTops[[panel]]
    pbs <- plotBots[[panel]]

    ### Loop within panels
    for (ls in 1:length(pts)) {
      tmp.point.top.dev <- cnvrt.coords(pts[[ls]]$dev,
                                        input='dev')
      tmp.point.bot.dev <- cnvrt.coords(pbs[[ls]]$dev,
                                        input='dev')

      segments(tmp.point.top.dev$usr$x,
               tmp.point.top.dev$usr$y,
               tmp.point.bot.dev$usr$x,
               tmp.point.bot.dev$usr$y,
               col='black')
    }
  }

  panelVector <- unique(facetNms)

  for (p in 1:nPhases) {
    for (panelId in 2:length(panelVector)) {

      pts.pre <- plotTops[[as.character(panelVector[panelId - 1])]]
      pbs.pre <- plotBots[[as.character(panelVector[panelId - 1])]]

      pts     <- plotTops[[as.character(panelVector[panelId])]]
      pbs     <- plotBots[[as.character(panelVector[panelId])]]

      tmp.point.top.pre.dev <- cnvrt.coords( pts.pre[[p]]$dev, input='dev' )
      tmp.point.bot.pre.dev <- cnvrt.coords( pbs.pre[[p]]$dev, input='dev' )

      tmp.point.top.dev     <- cnvrt.coords( pts[[p]]$dev, input='dev' )
      tmp.point.bot.dev     <- cnvrt.coords( pbs[[p]]$dev, input='dev' )

      segments(tmp.point.bot.pre.dev$usr$x, tmp.point.bot.pre.dev$usr$y,
               tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               col='black')

      segments(tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               tmp.point.top.dev$usr$x,     (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               col='black')

      segments(tmp.point.top.dev$usr$x,     (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               tmp.point.top.dev$usr$x,     tmp.point.top.dev$usr$y,
               col='black')
    }
  }
}


#' plotConcurrentReversals
#'
#' This is another multiple panel figure, however, conditions changes are not
#' always staggered out in terms of varying baselines. For example, this
#' implementation is more akin to a multiple probe design.
#'
#' @param data dataframe of assessment information
#' @param grouping character(1) column name, for grouping, referring to FA condition
#' @param session character(1) column name, for session numbering (should be distinct)
#' @param response character(1) column name, for response to chart
#' @param response2 character(1) column name, for response to chart
#' @param pnum character(1) column name, for numbered phases
#' @param poff character(1) column name, for offsetting overlapped phase connections
#' @param clabs data.frame condition labels
#' @param plabs data.frame phase labels
#' @param llabs data.frame legend stuff
#' @param title character(1) plot title
#' @param ylab character(1) plot axis title
#' @param xlab character(1) plot axis title
#' @param ymins numeric(n) override ylims
#' @param ymaxs numeric(n) override ylims
#'
#' @return
#' @export
#'
#' @examples
#'
#' conditionLabel <- data.frame(
#' Panel = rep("John", 6),
#' X     = c(2.5,
#'           6,
#'           9,
#'           12,
#'           15,
#'           18),
#' Y     = rep(19, 6),
#' Cex   = rep(1.25, 6),
#' Text  = c("Baseline",
#'           "FR-Lowest",
#'           "Baseline",
#'           "FR-Inelastic",
#'           "FR-Elastic",
#'           "FR-Inelastic")
#' )
#'
#' panelLabel <- data.frame(
#'   Panel = c("John",
#'             "Anthony",
#'             "Charles"),
#'   X     = rep(26, 3),
#'   Y     = c(5,
#'             12,
#'             21),
#'   Cex   = rep(1.5, 3),
#'   Text  = c("John",
#'             "Anthony",
#'             "Charles")
#' )
#'
#' legendParams <- data.frame(
#'   panel = "John",
#'   position = "topright",
#'   legend = c("Responses Observed", "Reinforcers Produced"),
#'   col = c('black', 'black'),
#'   lty = c(1, 2),
#'   pch = c(19,2),
#'   bty = "n",
#'   pt.cex = 2.25,
#'   cex = 1.25,
#'   text.col = "black",
#'   horiz = F,
#'   box.lty = 0
#' )
#'
#' plotConcurrentReversals(data     = Gilroyetal2021,
#'                         grouping = 'Participant',
#'                         session  = 'Session',
#'                         response = 'Responding',
#'                         response2= 'Reinforcers',
#'                         pnum     = 'PhaseNum',
#'                         poff     = 'LineOff',
#'                         title    = 'Individual Evaluations of Reinforcer Efficacy and Elasticity across Reinforcers',
#'                         xlab     = 'Session',
#'                         ylab     = 'Frequency (Responses, Reinforcers Delivered)',
#'                         ymins    = list("John"    = 0,
#'                                         "Anthony" = 0,
#'                                         "Charles" = 0),
#'                         ymaxs    = list("John"    = 20,
#'                                         "Anthony" = 10,
#'                                         "Charles" = 20),
#'                         clabs    = conditionLabel,
#'                         plabs    = panelLabel,
#'                         llabs    = legendParams)
#'
plotConcurrentReversals <- function(data = NULL, grouping = NULL, session = NULL,
                                    response = NULL, response2 = NULL,
                                    pnum = NULL, poff = NULL,
                                    clabs= NULL, plabs = NULL, llabs = NULL,
                                    title,  ylab, xlab, ymins, ymaxs) {


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

  if (is.null(data))                   stop("No data was supplied")
  if (is.null(clabs))                  stop("No condition labels supplied")
  if (is.null(plabs))                  stop("No panel labels supplied")
  if (!(grouping %in% colnames(data))) stop("The grouping column was not found")
  if (!(session  %in% colnames(data))) stop("The session column was not found")
  if (!(response %in% colnames(data))) stop("The response column was not found")

  facetNms  <- unique(data[,grouping])
  nRows     <- length(facetNms)
  nPhases   <- max(data[,pnum]) - 1
  xMin      <- min(data[,session])
  nSessions <- max(data[,session])
  yMin      <- min(data[,response])

  if (!missing(response2)) {
    if (!(response %in% colnames(data))) stop("The second response was not found")

    yMin2   <- min(data[,response2])
    yMin    <- ifelse(yMin2 < yMin,
                      yMin2,
                      yMin)
  }

  if (!missing(ymins)) {
    if(length(ymins) != length(facetNms))
      stop("Ymins of incorrect length")
  }

  if (!missing(ymaxs)) {
    if(length(ymaxs) != length(facetNms))
      stop("Ymaxs of incorrect length")
    else
      maxYs <- ymaxs
  }

  plotDims    <- c(nRows,
                   1)
  X           <- seq(xMin,
                     nSessions)
  panelData   <- list()
  phaseShifts <- list()
  maxYs       <- list()
  plotTops    <- list()
  plotBots    <- list()

  for(p in facetNms) {
    subs           <- data[which(data[,grouping] == p), ]
    panelData[[p]] <- subs
    maxYs[[p]]     <- ifelse(missing(response2),
                             max(subs[,response], na.rm = TRUE),
                             max(c(as.numeric(subs[,response]),
                                   as.numeric(subs[,response2])), na.rm = TRUE))
  }

  par(mfrow  = plotDims,
      family = "serif",
      omi    = outMarInches,
      mai    = marginInches,
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  for(p in facetNms) {
    data    <- panelData[[p]]
    conds   <- clabs[clabs$Panel == p,]
    pans    <- plabs[plabs$Panel == p,]
    legs    <- llabs[llabs$panel == p,]

    pl.ind  <- list()
    plt.top <- list()
    plt.bot <- list()

    x       <- data[,session]
    y       <- data[,response]
    ps      <- data[,pnum]

    shouldShowSessions <- p == facetNms[length(facetNms)]

    if (!missing(ymins))
      yMin <- ymins[[p]]

    if (!missing(ymaxs))
      yMax <- ymaxs[[p]]
    else
      yMax <- maxYs[[p]]

    if (shouldShowSessions) {
      deltaY <- (yMax - yMin) * 0.04
    }

    plot(x,y,
         pch        = llabs[1, "pch"],
         xlab       = "",
         xlim       = c(xMin, nSessions),
         ylim       = c(yMin, yMax),
         ylab       = "",
         xaxt       = 'n',
         type       = 'p',
         cex        = llabs[1, "pt.cex"],
         las        = 1,
         frame.plot = FALSE)

    axis(1,
         labels     = shouldShowSessions,
         at         = X)

    box(bty         = "l")

    ### Conditions tags

    if (nrow(conds) > 0)
      for (c in 1:nrow(conds)) {
        text(conds[c, "X"],
             conds[c, "Y"],
             conds[c, "Text"],
             cex = conds[c, "Cex"],
             adj = c(1, 0))
      }

    if (nrow(pans) > 0)
      for (panNum in 1:nrow(pans)) {
        text(pans[panNum, "X"],
             pans[panNum, "Y"],
             pans[panNum, "Text"],
             cex = pans[panNum, "Cex"],
             adj = c(1, 0))
      }

    if (nrow(legs) > 0)
      legend(as.character(legs[1, "position"]),
             legend   = as.character(legs[, "legend"]),
             col      = as.character(legs[, "col"]),
             text.col = as.character(legs[1, "text.col"]),
             lty      = as.numeric(legs[, "lty"]),
             box.lty  = as.numeric(legs[1, "box.lty"]),
             pch      = as.numeric(legs[, "pch"]),
             bty      = as.numeric(legs[1, "bty"]),
             pt.cex   = as.numeric(legs[1, "pt.cex"]),
             cex      = as.numeric(legs[1, "cex"]),
             horiz    = as.logical(legs[1, "horiz"]))

    isFirst <- TRUE
    last.X  <- NA
    offsetYMax <- 0

    for (ls in unique(ps)) {

      pdata <- data[which(data[,pnum] == ls), ]

      lines(pdata[,session], pdata[,response])
      points(pdata[,session],
             pdata[,response],
             pch = llabs[1, "pch"],
             cex = llabs[1, "pt.cex"])

      # Plot alternative response, if necessary
      if (!missing(response2)) {
        points(pdata[,session],
               pdata[,response2],
               pch = llabs[2, "pch"],
               cex = llabs[2, "pt.cex"])
        lines(pdata[,session],
              pdata[,response2],
              lty = 2)
      }

      ### the first begins at 0, so skip it
      if (!isFirst) {
        pl.ind[ls-1] = (last.X + min(pdata[[session]])) / 2
        plt.top[[ls-1]] = cnvrt.coords(pl.ind[ls-1], yMax - offsetYMax)

        yOff <- ifelse(p == facetNms[length(facetNms)],
                       -deltaY,
                       yMin)

        plt.bot[[ls-1]] = cnvrt.coords(pl.ind[ls-1], yOff)
      }

      isFirst <- FALSE
      last.X <- max(pdata[[session]])
      offsetYMax <- pdata[1,poff]
    }

    phaseShifts[[p]] = pl.ind
    phaseShifts$num = length(unique(ps))

    plotTops[[p]] = plt.top
    plotBots[[p]] = plt.bot
  }

  if (!missing(title)) mtext(title, side=3, outer=TRUE, line=0)
  if (!missing(ylab))  mtext(ylab,  side=2, outer=TRUE)
  if (!missing(xlab))  mtext(xlab,  side=1, outer=TRUE)

  ### Loop across panels
  for (panel in facetNms) {
    pts <- plotTops[[panel]]
    pbs <- plotBots[[panel]]

    ### Loop within panels
    for (ls in 1:length(pts)) {
      tmp.point.top.dev <- cnvrt.coords(pts[[ls]]$dev,
                                        input='dev')
      tmp.point.bot.dev <- cnvrt.coords(pbs[[ls]]$dev,
                                        input='dev')

      segments(tmp.point.top.dev$usr$x,
               tmp.point.top.dev$usr$y,
               tmp.point.bot.dev$usr$x,
               tmp.point.bot.dev$usr$y,
               col='black')
    }
  }

  panelVector <- unique(facetNms)

  for (p in 1:nPhases) {
    for (panelId in 2:length(panelVector)) {

      pts.pre <- plotTops[[as.character(panelVector[panelId - 1])]]
      pbs.pre <- plotBots[[as.character(panelVector[panelId - 1])]]

      pts     <- plotTops[[as.character(panelVector[panelId])]]
      pbs     <- plotBots[[as.character(panelVector[panelId])]]

      tmp.point.top.pre.dev <- cnvrt.coords( pts.pre[[p]]$dev, input='dev' )
      tmp.point.bot.pre.dev <- cnvrt.coords( pbs.pre[[p]]$dev, input='dev' )

      tmp.point.top.dev     <- cnvrt.coords( pts[[p]]$dev, input='dev' )
      tmp.point.bot.dev     <- cnvrt.coords( pbs[[p]]$dev, input='dev' )

      segments(tmp.point.bot.pre.dev$usr$x, tmp.point.bot.pre.dev$usr$y,
               tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               col='black')

      segments(tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               tmp.point.top.dev$usr$x,     (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               col='black')

      segments(tmp.point.top.dev$usr$x,     (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               tmp.point.top.dev$usr$x,     tmp.point.top.dev$usr$y,
               col='black')
    }
  }

}


#' plotMultiElementFA
#'

#' plotAnnotatedReversal
#'
#' The goal of the plotAnnotatedReversal function is to construct a feature-
#' heavy figure that communicates many ecological changes without a table.
#'
#' @param data data.frame of assessment information
#' @param grouping character(1) column name, for grouping, referring to FA condition
#' @param session character(1) column name, for session numbering (should be distinct)
#' @param response character(1) column name, for response to chart
#' @param response2 character(1) column name, for secondary response to chart
#' @param response3 character(1) column name, for tertiary response to chart
#' @param condCol character(1) column name, for conditions
#' @param pnum character(1) column name, for numbered phases
#' @param poff character(1) column name, for offsetting overlapped phase connections
#' @param clabs data.frame condition labels
#' @param plabs data.frame phase labels
#' @param alines data.frame annotations for lines
#' @param abracks data.frame annotations for brackets
#' @param title character(1) plot title
#' @param ylab character(1) plot axis title
#' @param xlab character(1) plot axis title
#' @param ymins numeric(n) override ylims
#' @param ymaxs numeric(n) override ylims
#' @param deltaX numeric(n) n-by increases from 0 in sessions
#'
#' @return
#' @export
#'
#' @examples
#'
#'
#' # Specify location, text, and aesthetics of condition labels
#' conditionLabel <- data.frame(
#'   Panel = rep("John", 6),         # Only on top row (for andrew)
#'   X     = c(2.5,                  # Positioned between phase change lines
#'             6,
#'             9,
#'             12,
#'             15,
#'             18),
#'   Y     = rep(19, 6),             # Positioned near top of (John's) panel
#'   Cex   = rep(1.25, 6),           # Slightly oversized for clarity
#'   Text  = c("Baseline",           # Content of phase labels
#'             "FR-Lowest",
#'             "Baseline",
#'             "FR-Inelastic",
#'             "FR-Elastic",
#'             "FR-Inelastic")
#' )
#'
#' # Specify location, text, and aesthetics of panel labels
#' panelLabel <- data.frame(
#'   Panel = c("John",               # Specify the panels to draw upon
#'             "Anthony",
#'             "Charles"),
#'   X     = rep(26, 3),             # Positioned near right of panel edge
#'   Y     = c(5,                    # Positioned at various heights, for each case
#'             12,
#'             21),
#'   Cex   = rep(1.5, 3),            # Slightly oversized for clarity
#'   Text  = c("John",               # Specific text to write
#'             "Anthony",
#'             "Charles")
#' )
#'
#' # Specify if/how we want to show a legend
#' legendParams <- data.frame(
#'   panel =      "John",                # legend drawn on panel for John
#'   position =   "topright",            # drawn in top right corner
#'   legend = c("Responses Observed",    # Items to draw for legend
#'              "Reinforcers Produced"),
#'   col = c('black',                    # colors to draw lines
#'           'black'),
#'   lty = c(1, 2),                      # draw solid lines
#'   pch = c(19,2),                      # markers to draw
#'   bty = "n",                          # don't draw surrounding box
#'   pt.cex = 2.25,                      # oversize markers
#'   cex = 1.25,                         # slightly oversize text
#'   text.col = "black",                 # draw text in black
#'   horiz = F,                          # organize items vertically
#'   box.lty = 0                         # don't draw surrounding box
#' )
#'
#' plotConcurrentReversals(data     = Gilroyetal2021,
#'                         grouping = 'Participant',
#'                         session  = 'Session',
#'                         response = 'Responding',
#'                         response2= 'Reinforcers',
#'                         pnum     = 'PhaseNum',
#'                         poff     = 'LineOff',
#'                         title    = 'Individual Evaluations of Reinforcer Efficacy and Elasticity across Reinforcers',
#'                         xlab     = 'Session',
#'                         ylab     = 'Frequency (Responses, Reinforcers Delivered)',
#'                         ymins    = list("John"    = 0,
#'                                         "Anthony" = 0,
#'                                         "Charles" = 0),
#'                         ymaxs    = list("John"    = 20,
#'                                         "Anthony" = 10,
#'                                         "Charles" = 20),
#'                         clabs    = conditionLabel,
#'                         plabs    = panelLabel,
#'                         llabs    = legendParams)
plotAnnotatedReversal <- function(data = NULL, grouping = NULL, session = NULL,
                                  response = NULL, response2 = NULL, response3 = NULL,
                                  condCol = NULL,  pnum = NULL, poff = NULL,
                                  clabs= NULL, plabs = NULL, alines, abracks,
                                  title, ylab, xlab,
                                  ymins, ymaxs, deltaX) {

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

  if (is.null(data))                   stop("No data was supplied")
  if (is.null(clabs))                  stop("No condition labels supplied")
  if (is.null(plabs))                  stop("No panel labels supplied")
  if (!(grouping %in% colnames(data))) stop("The grouping column was not found")
  if (!(session  %in% colnames(data))) stop("The session column was not found")
  if (!(response %in% colnames(data))) stop("The response column was not found")
  if (!(condCol  %in% colnames(data))) stop("The conditions column was not found")

  facetNms  <- unique(data[,grouping])
  nRows     <- length(facetNms)
  nPhases   <- max(data[,pnum]) - 1
  xMin      <- 0  #min(data[,session])
  nSessions <- max(data[,session])
  yMin      <- min(data[,response])

  if (!missing(response2)) {
    if (!(response %in% colnames(data))) stop("The second response was not found")

    yMin2   <- min(data[,response2])
    yMin    <- ifelse(yMin2 < yMin,
                      yMin2,
                      yMin)
  }

  if (!missing(ymins)) {
    if(length(ymins) != length(facetNms))
      stop("Ymins of incorrect length")
  }

  if (!missing(ymaxs)) {
    if(length(ymaxs) != length(facetNms))
      stop("Ymaxs of incorrect length")
    else
      maxYs <- ymaxs
  }

  delta.X <- ifelse(missing(deltaX), 1, deltaX)

  plotDims    <- c(nRows,
                   1)
  X           <- seq(xMin,
                     nSessions,
                     by = deltaX)

  panelData   <- list()
  phaseShifts <- list()
  maxYs       <- list()
  plotTops    <- list()
  plotBots    <- list()

  for(p in facetNms) {
    subs           <- data[which(data[,grouping] == p), ]
    panelData[[p]] <- subs
    maxYs[[p]]     <- ifelse(missing(response2),
                             max(subs[,response], na.rm = TRUE),
                             max(c(as.numeric(subs[,response]),
                                   as.numeric(subs[,response2])), na.rm = TRUE))
  }

  par(mfrow  = plotDims,
      family = "serif",
      omi    = outMarInches,
      mai    = marginInches,
      xaxs   = "r",
      yaxs   = "r",
      xpd    = NA)

  for(p in facetNms) {
    data    <- panelData[[p]]
    conds   <- clabs[clabs$Panel == p,]
    pans    <- plabs[plabs$Panel == p,]
    aline   <- alines[alines$Panel == p,]
    abrack  <- abracks[abracks$Panel == p,]

    pl.ind  <- list()
    plt.top <- list()
    plt.bot <- list()

    x       <- data[,session]
    y       <- data[,response]
    ps      <- data[,pnum]

    shouldShowSessions <- p == facetNms[length(facetNms)]

    if (!missing(ymins))
      yMin <- ymins[[p]]

    if (!missing(ymaxs))
      yMax <- ymaxs[[p]]
    else
      yMax <- maxYs[[p]]

    deltaY <- (yMax - yMin) * 0.04

    # plot baseline

    baselineCondition = unique(data[, condCol])[1]
    data.sub <- data[which(data[,condCol] == baselineCondition), ]

    x       <- data.sub[,session]
    y       <- data.sub[,response]

    plot(x,y,
         pch        = 21,
         col        = "black",
         bg         = "white",
         xlab       = "",
         xlim       = c(xMin, nSessions),
         ylim       = c(yMin, yMax),
         ylab       = "",
         xaxt       = 'n',
         yaxt       = 'n',
         cex        = 1.25,
         las        = 1,
         frame.plot = FALSE)

    axis(1,
         labels     = shouldShowSessions,
         at         = X)

    axis(2,
         labels     = yMin:yMax,
         las        = 2,
         at         = yMin:yMax)

    box(bty         = "l")

    ### Conditions tags

    if (nrow(conds) > 0)
      for (c in 1:nrow(conds)) {
        text(conds[c, "X"],
             conds[c, "Y"],
             conds[c, "Text"],
             cex = conds[c, "Cex"],
             srt = conds[c, "Srt"],
             adj = c(1, 0))
      }

    if (nrow(pans) > 0)
      for (panNum in 1:nrow(pans)) {
        text(pans[panNum, "X"],
             pans[panNum, "Y"],
             pans[panNum, "Text"],
             cex = pans[panNum, "Cex"],
             adj = c(1, 0))
      }

    if (nrow(aline) > 0) {
      for (alineNum in 1:nrow(aline)) {

        topCoords = cnvrt.coords(aline[alineNum, "X"], yMax + deltaY)
        botCoords = cnvrt.coords(aline[alineNum, "X"], -deltaY)

        tmp.point.top.dev <- cnvrt.coords(topCoords$dev, input='dev')
        tmp.point.bot.dev <- cnvrt.coords(botCoords$dev, input='dev')

        segments(tmp.point.top.dev$usr$x,
                 tmp.point.top.dev$usr$y,
                 tmp.point.bot.dev$usr$x,
                 tmp.point.bot.dev$usr$y,
                 col='black',
                 lty = aline[alineNum, "Lty"])
      }
    }

    if (nrow(abrack) > 0) {
      for (abrackNum in 1:nrow(abrack)) {

        topCoordsL = cnvrt.coords(abrack[abrackNum, "X1"],
                                  abrack[abrackNum, "Y1"])

        topCoordsLD = cnvrt.coords(abrack[abrackNum, "X1"],
                                   abrack[abrackNum, "Y2"])

        topCoordsR = cnvrt.coords(abrack[abrackNum, "X2"],
                                  abrack[abrackNum, "Y1"])

        topCoordsRD = cnvrt.coords(abrack[abrackNum, "X2"],
                                   abrack[abrackNum, "Y2"])

        segments(topCoordsL$usr$x,
                 topCoordsL$usr$y,
                 topCoordsR$usr$x,
                 topCoordsR$usr$y,
                 col='black',
                 lty = abrack[abrackNum, "Lty"])

        arrows(topCoordsR$usr$x,
               topCoordsR$usr$y,
               x1 = topCoordsRD$usr$x,
               y1 = topCoordsRD$usr$y,
               lty = abrack[abrackNum, "Lty"],
               length = 0.125)

        arrows(topCoordsL$usr$x,
               topCoordsL$usr$y,
               x1 = topCoordsLD$usr$x,
               y1 = topCoordsLD$usr$y,
               lty = abrack[abrackNum, "Lty"],
               length = 0.125)
      }
    }

    isFirst <- TRUE
    last.X  <- NA
    offsetYMax <- 0

    for (ls in unique(ps)) {
      pdata <- data[which(data[,pnum] == ls), ]
      lines(pdata[,session], pdata[,response])
      points(pdata[,session],
             pdata[,response],
             pch = 21,
             col = "black",
             bg  = 'white',
             cex = 1.25)

      # Plot alternative response, if necessary
      if (!missing(response2)) {
        lines(pdata[,session],
              pdata[,response2],
              lty = 1)
        points(pdata[,session],
               pdata[,response2],
               pch = 18,
               bg = 'black',
               cex = 1.25)
      }

      # Plot alternative response, if necessary
      if (!missing(response3)) {
        lines(pdata[,session],
              pdata[,response3],
              lty = 1)
        points(pdata[,session],
               pdata[,response3],
               pch = 17,
               cex = 1.25)
      }

      ### the first begins at 0, so skip it
      if (!isFirst) {
        pl.ind[ls-1] = (last.X + min(pdata[[session]])) / 2
        plt.top[[ls-1]] = cnvrt.coords(pl.ind[ls-1], yMax - offsetYMax + deltaY)

        plt.bot[[ls-1]] = cnvrt.coords(pl.ind[ls-1], -deltaY)
      }

      isFirst <- FALSE
      last.X <- max(pdata[[session]])
      offsetYMax <- pdata[1,poff]
    }

    phaseShifts[[p]] = pl.ind
    phaseShifts$num = length(unique(ps))

    plotTops[[p]] = plt.top
    plotBots[[p]] = plt.bot
  }

  if (!missing(title)) mtext(title, side=3, outer=TRUE, line=0)
  if (!missing(ylab))  mtext(ylab,  side=2, outer=TRUE)
  if (!missing(xlab))  mtext(xlab,  side=1, outer=TRUE)

  ### Loop across panels
  for (panel in facetNms) {
    pts <- plotTops[[panel]]
    pbs <- plotBots[[panel]]

    ### Loop within panels
    for (ls in 1:length(pts)) {
      tmp.point.top.dev <- cnvrt.coords(pts[[ls]]$dev,
                                        input='dev')
      tmp.point.bot.dev <- cnvrt.coords(pbs[[ls]]$dev,
                                        input='dev')

      segments(tmp.point.top.dev$usr$x,
               tmp.point.top.dev$usr$y,
               tmp.point.bot.dev$usr$x,
               tmp.point.bot.dev$usr$y,
               col='black')
    }
  }

  panelVector <- unique(facetNms)

  for (p in 1:nPhases) {
    for (panelId in 2:length(panelVector)) {

      pts.pre <- plotTops[[as.character(panelVector[panelId - 1])]]
      pbs.pre <- plotBots[[as.character(panelVector[panelId - 1])]]

      pts     <- plotTops[[as.character(panelVector[panelId])]]
      pbs     <- plotBots[[as.character(panelVector[panelId])]]

      if (length(pts) < p | length(pbs) < p) {
        next;
      }

      tmp.point.top.pre.dev <- cnvrt.coords( pts.pre[[p]]$dev, input='dev' )
      tmp.point.bot.pre.dev <- cnvrt.coords( pbs.pre[[p]]$dev, input='dev' )

      tmp.point.top.dev     <- cnvrt.coords( pts[[p]]$dev, input='dev' )
      tmp.point.bot.dev     <- cnvrt.coords( pbs[[p]]$dev, input='dev' )

      segments(tmp.point.bot.pre.dev$usr$x, tmp.point.bot.pre.dev$usr$y,
               tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               col='black')

      segments(tmp.point.bot.pre.dev$usr$x, (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               tmp.point.top.dev$usr$x,     (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               col='black')

      segments(tmp.point.top.dev$usr$x,     (tmp.point.bot.pre.dev$usr$y + tmp.point.top.dev$usr$y) / 2,
               tmp.point.top.dev$usr$x,     tmp.point.top.dev$usr$y,
               col='black')
    }
  }
}
