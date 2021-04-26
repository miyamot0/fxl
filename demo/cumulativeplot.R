# Shawn Gilroy, 2021 GPLv2+
#
# demo/cumulativeplot.R
#
# This file illustrates how to construct a cumulative figure, without a legend, and instead
# describing features using annotations. This is particularly useful for emulating existing
# conventions that reflect cumulative choice across time (e.g., counts of initial link selections)
#

library(dplyr) # included for infix logic
library(fxl)   # core plotting methods

scr_plot(data = LozyEtAl2020, aesthetics = list(x = Session,
                                                 y = KM,
                                                 p = Phase,
                                                 facet = Participant),
         ncol = 2,
         mai = c(0.3,  0.3,  0.0, 0.1),
         omi = c(0.25, 0.25, 0.1, 0)) %>%
    scr_xoverride(c(1, 30),
                  xdraws = c("Eva", "Cali", "Ari"),
                  xticks = list(
                    "Eli"  = c(1, 5, 10, 15),
                    "Ari"  = c(1, 10, 20, 30),
                    "Al"   = c(1, 5, 10, 15),
                    "Ry"   = c(1, 5, 10, 15),
                    "Eva"  = c(1, 5, 10, 15),
                    "Cali" = c(1, 5, 10, 15)
                  )) %>%
    scr_yoverride(ydraws = c("Eli", "Al", "Eva"),
                  list(
                    "Eli"  = list(y0 = 0, y1 = 15, yticks = c(0, 5, 10, 15)),
                    "Ari"  = list(y0 = 0, y1 = 15, yticks = c(0, 5, 10, 15)),
                    "Al"   = list(y0 = 0, y1 = 8,  yticks = c(0, 2, 4,  6,  8)),
                    "Ry"   = list(y0 = 0, y1 = 8,  yticks = c(0, 2, 4,  6,  8)),
                    "Eva"  = list(y0 = 0, y1 = 8,  yticks = c(0, 2, 4,  6,  8)),
                    "Cali" = list(y0 = 0, y1 = 8,  yticks = c(0, 2, 4,  6,  8))
                  )
    ) %>%
    scr_cumsum_lines() %>%
    scr_cumsum_points(pch = 24, fill = 'white', cex = 1.75)  %>%
    scr_cumsum_lines(mapping = list(x = Session,
                                    y = TD)) %>%
    scr_cumsum_points(pch = 22, fill = 'white', cex = 1.75,
                      mapping = list(x = Session,
                                     y = TD)) %>%
    scr_label_facet(cex = 1.5,         # plot labels across facets (not within a single facet)
                    adj = 1,
                    labels = list(     # list of labels to draw (will use assigned key for label)
                      "Eli"   = list(x = 15,
                                     y = 2),
                      "Ari"   = list(x = 30,
                                     y = 2),
                      "Al"    = list(x = 15,
                                     y = 1),
                      "Cali"  = list(x = 15,
                                     y = 1),
                      "Ry"    = list(x = 15,
                                     y = 1),
                      "Eva"   = list(x = 15,
                                     y = 1))) %>%
    scr_plines(                                                      # add in simple phase lines
      lty = 3,
      lines = list(
        "Ari" = list(
          "A" = list(
            x1 = 11.5, y1 = 15,
            x2 = 11.5,  y2 = 0
          )
        ),
        "Al" = list(
          "A" = list(
            x1 = 4.5, y1 = 8,
            x2 = 4.5, y2 = 0
          )
        ),
        "Cali" = list(
          "A" = list(
            x1 = 8.5, y1 = 8,
            x2 = 8.5, y2 = 0
          )
        ),
        "Ry" = list(
          "A" = list(
            x1 = 9.5, y1 = 8,
            x2 = 9.5, y2 = 0
          )
        ),
        "Eva" = list(
          "A" = list(
            x1 = 4.5, y1 = 8,
            x2 = 4.5, y2 = 0
          ),
          "B" = list(
            x1 = 8.5, y1 = 8,
            x2 = 8.5, y2 = 0
          )
        )
      )
    ) %>%
    scr_label_phase(facet = "Eli",
                    cex = 1.25,
                    adj = 0.5,
                    labels = list(
                      "Choice 1" = list(x = 6,
                                        y = 15)
                      )) %>%
    scr_label_phase(facet = "Ari",
                    cex = 1.25,
                    adj = 0.5,
                    labels = list(
                      "Choice 1" = list(x = 5,
                                        y = 15),
                      "Choice 2" = list(x = 20,
                                        y = 15)
                    )) %>%
    scr_label_phase(facet = "Al",
                    cex = 1.25,
                    adj = 0.5,
                    labels = list(
                      "Choice 1" = list(x = 2.25,
                                        y = 8),
                      "Choice 2" = list(x = 11,
                                        y = 8)
                    )) %>%
    scr_label_phase(facet = "Ry",
                    cex = 1.25,
                    adj = 0.5,
                    labels = list(
                      "Choice 1" = list(x = 5,
                                        y = 8),
                      "Choice 2" = list(x = 13,
                                        y = 8)
                    )) %>%
    scr_label_phase(facet = "Eva",
                    cex = 1.25,
                    adj = 0.5,
                    labels = list(
                      "Choice 1" = list(x = 2.25,
                                        y = 8),
                      "Choice 2" = list(x = 6.5,
                                        y = 8),
                      "Choice 3" = list(x = 11,
                                        y = 8)
                    )) %>%
    scr_label_phase(facet = "Cali",
                    cex = 1.25,
                    adj = 0.5,
                    labels = list(
                      "Choice 1" = list(x = 4,
                                        y = 8),
                      "Choice 2" = list(x = 11,
                                        y = 8)
                    )) %>%
    scr_xlabel("Choice Training Session") %>%                 # Override x-axis label (bottom only shown by default)
    scr_ylabel("Cumulative Number of Selections") %>%
    scr_legend(position = "topright",                         # Specify legend location
               panel    = "Eli",
               legend   = c("KM",                               # labels to include (ordered)
                            "TD"),
               col      = c('white',                            # color of markers (ordered)
                            'white'),
               m.col    = c('black',                            # color of markers (ordered)
                            'black'),
               lty      = c(1, 1),                            # line types (ordered)
               pch      = c(24, 22),                          # marker types (ordered)
               bty      = "y",                                # remove border
               pt.cex   = 2.25,                               # point size scale
               cex      = 1.25,                               # text size scale
               text.col = "black",                            # text color
               horiz    = F,                                  # list items vertically
               box.lty  = 1)

#%>%                             # change box size (0 = removed)
  # scr_save(units = "in",
  #          name = "cumulativeplot.svg",
  #          format = "svg",
  #          width = 8,
  #          height = 6)  %>%
  # scr_save(units = "in",
  #          name = "cumulativeplot.png",
  #          format = "png",
  #          width = 8,
  #          height = 6,
  #          res = 300)


