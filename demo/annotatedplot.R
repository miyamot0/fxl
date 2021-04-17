# Shawn Gilroy, 2021 GPLv2+
#
# demo/concurrentplot.R
#
# This file illustrates how to construct a complicated figure, without a legend, and instead
# describing features using annotations. This is particularly useful for emulating existing
# conventions (e.g., labeled series with arrows, brackets for schedule thinning/demand fading)
#

library(dplyr) # included for infix logic
library(fxl)   # core plotting methods

currentData = Gilroyetal2019Tx %>%
  mutate(Condition = paste0(Condition, PhaseNum)) %>%
  rename(Function = Participant,
         AFCR = FCR,
         EFCR = FCR2) %>%
  select(-c(PhaseNum, LineOff))

scr_plot(currentData, aesthetics = list(x = Session,
                                       y = CTB,
                                       p = Condition,
                                       facet = Function),
        mai = c(0.5, 0.5, 0.1, 0.5),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_yoverride(c(0, 3)) %>%
  scr_xoverride(c(0, 100), xdelta = 10) %>%
  scr_lines(size = 1) %>%
  scr_lines(mapping = list(x = Session,
                           y = AFCR),
            size = 1,
            lty = 2) %>%
  scr_lines(mapping = list(x = Session,
                           y = EFCR),
            size = 1,
            lty = 3) %>%
  scr_points(fill = 'white', pch = 21) %>%
  scr_points(mapping = list(x = Session,
                            y = AFCR),
             cex = 1,
             pch = 20,
             fill = 'black') %>%
  scr_points(mapping = list(x = Session,
                            y = EFCR),
             cex = 0.75,
             pch = 24,
             fill = 'black') %>%
  scr_plines_mbd(lines = list(
    "A" = list(
      "Attention" = list(x1 = 13.5, y1 = 3.15,
                         x2 = 13.5, y2 = 0),
      "Demand" = list(x1 = 20, y1 = 3,
                         x2 = 20, y2 = 0)
    )
  )) %>%
  scr_plines(# solid ones
    lty = 1,
    lines = list(
    "Attention" = list(
      "A" = list(
        x1 = 25.5, y1 = 3,
        x2 = 25.5, y2 = 0
      ),
      "B" = list(
        x1 = 26.5, y1 = 3,
        x2 = 26.5, y2 = 0
      )
    ),
    "Demand" = list(
      "A" = list(
        x1 = 34.5, y1 = 3,
        x2 = 34.5, y2 = 0
      ),
      "B" = list(
        x1 = 37.5, y1 = 3,
        x2 = 37.5, y2 = 0
      ),
      "C" = list(
        x1 = 41.5, y1 = 3,
        x2 = 41.5, y2 = 0
      )
    )
  )) %>%
  scr_plines(# dashed ones
    lty = 3,
    lines = list(
      "Attention" = list(
        "A" = list(
          x1 = 60.5, y1 = 3,
          x2 = 60.5, y2 = 0
        ),
        "B" = list(
          x1 = 76.5, y1 = 3,
          x2 = 76.5, y2 = 0
        )
      ),
      "Demand" = list(
        "A" = list(
          x1 = 50.5, y1 = 3,
          x2 = 50.5, y2 = 0
        ),
        "B" = list(
          x1 = 72.5, y1 = 3,
          x2 = 72.5, y2 = 0
        )
      )
    )) %>%
    scr_label_facet(cex = 1.25,
                    adj = 1,
                    labels = list(
                      "Attention" = list(x = 100,
                                         y = 3.25),
                      "Demand" = list(x = 100,
                                         y = 3.25)
                    )) %>%
    scr_label_phase(facet = "Attention",
                    cex = 0.6,
                    adj = 0.5,
                    labels = list(
                      "Baseline" = list(x = 14,
                                            y = 3.5),
                      "FCR-A + EXT" = list(x = 19,
                                            y = 3),
                      "FCR-A + EXT" = list(x = 32,
                                            y = 3),
                      "Parent Implementation" =
                                       list(x = 68.5,
                                            y = 3),
                      "Generalization" = list(x = 82,
                                            y = 3),
                      "Problem Behavior" = list(
                        x = 7,
                        y = 1.8
                      ),
                      "FCR-A" = list(
                        x = 20,
                        y = 2.5
                      ),
                      "Add FCR\nOptions" = list(
                        x = 31,
                        y = 2.5
                      )
                    )) %>%
  scr_label_phase(facet = "Attention",
                  cex = 0.6,
                  adj = 0.5,
                  labels = list(
                    "5s" = list(x = 39,
                                            y = 2.4),
                    "Schedule Thinning" = list(x = 54,
                                            y = 2.4),
                    "300s" = list(x = 75,
                                            y = 2.4))) %>%
    scr_label_phase(facet = "Demand",
                    cex = 0.6,
                    adj = 0.5,
                    labels = list(
                      "FCR-E + EXT" = list(x = 30,
                                            y = 3.45),
                      "FCR-A P = 0.1" = list(x = 36,
                                            y = 2,
                                            srt = 90),
                      "FCR-A/E + EXT" = list(x = 47,
                                              y = 3.35),
                      "Parent Implementation" =
                        list(x = 58.5,
                             y = 3),
                      "Generalization" = list(x = 78,
                                              y = 3),
                      "FCR-E" = list(
                        x = 24,
                        y = 2.5),
                      "FCR-A\nP = 0.1\n200% SR" = list(
                        x = 46,
                        y = 2)
                    )) %>%
    scr_label_phase(facet = "Demand",
                    cex = 0.6,
                    adj = 0.5,
                    labels = list(
                      "1" = list(x = 30,
                                              y = 1.375),
                      "Demand Fading" = list(x = 56,
                                              y = 1.375),
                      "6" = list(x = 71.5,
                                              y = 1.375))) %>%
    scr_arrows(facet = "Attention",
               length = 0.1,
               arrows = list(
                 "A" = list(
                   x0 = 7,
                   x1 = 7,
                   y0 = 1.5,
                   y1 = 1
                 ),
                 "B" = list(
                   x0 = 20,
                   x1 = 20,
                   y0 = 2.25,
                   y1 = 2
                 ),
                 "C" = list(
                   x0 = 31,
                   x1 = 31,
                   y0 = 2.25,
                   y1 = 2
                 )
               )) %>%
  scr_arrows(facet = "Demand",
             length = 0.1,
             arrows = list(
               "A" = list(
                 x0 = 24,
                 x1 = 24,
                 y0 = 2.25,
                 y1 = 1.5
               ),
               "B" = list(
                 x0 = 36,
                 x1 = 36,
                 y0 = 1.3,
                 y1 = 0.75
               ),
               "C" = list(
                 x0 = 46,
                 x1 = 46,
                 y0 = 1.5,
                 y1 = 0.75
               )
             )) %>%
  scr_brackets(facet = "Attention",
               length = 0.1,
               brackets = list(
                 "A" = list(
                   x0 = 8,
                   x1 = 26,
                   y0 = 3.3,
                   y1 = 3
                 )
               )) %>%
  scr_brackets(facet = "Attention",
               length = 0.1,
               lty = 3,
               brackets = list(
                 "A" = list(
                   x0 = 38,
                   x1 = 76,
                   y0 = 2.25,
                   y1 = 1.5
                 )
               )) %>%
  scr_brackets(facet = "Demand",
               length = 0.1,
               brackets = list(
                 "A" = list(
                   x0 = 23,
                   x1 = 40,
                   y0 = 3.3,
                   y1 = 3
                 ),
                 "B" = list(
                   x0 = 36,
                   x1 = 47,
                   y0 = 3.2,
                   y1 = 2.9
                 )
               )) %>%
  scr_brackets(facet = "Demand",
               length = 0.1,
               lty = 3,
               brackets = list(
                 "A" = list(
                   x0 = 29,
                   x1 = 72,
                   y0 = 1.25,
                   y1 = 0.5
                 )
               ))

#%>%
#   scr_save(name = "annotatedfigure2.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
