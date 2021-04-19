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

selectionData = Tigeretal2006

scr_plot(data = selectionData, aesthetics = list(x = Session,
                                                 y = Splus,
                                                 facet = Participant),
         mai = c(0.3, 0.25, 0.1, 0.1),
         omi = c(0.25, 0.25, 0.25, 0)) %>%
  scr_xoverride(c(15, 55), xdelta = 5) %>%
  scr_yoverride(c(0, 15), ydelta = 3) %>%
  scr_cumsum_lines() %>%
  scr_cumsum_lines(mapping = list(x = Session,
                                  y = SplusSminus)) %>%
  scr_cumsum_lines(mapping = list(x = Session,
                                  y = Mix)) %>%
  scr_cumsum_points(pch = 22, fill = 'white', cex = 1.75) %>%
  scr_cumsum_points(pch = 22, fill = 'black', cex = 1.75,
                    mapping = list(x = Session,
                                   y = SplusSminus)) %>%
  scr_cumsum_points(pch = 24, fill = 'gray', cex = 1.75,
                    mapping = list(x = Session,
                                   y = Mix)) %>%
  scr_label_facet(cex = 1.5,         # plot labels across facets (not within a single facet)
                  adj = 1,
                  labels = list(     # list of labels to draw (will use assigned key for label)
                    "Ana"  = list(x = 55,
                                  y = 14),
                    "Josh" = list(x = 55,
                                  y = 14),
                    "John" = list(x = 55,
                                  y = 14))) %>%
  scr_arrows(facet = "Ana",
             length = 0.1,
             arrows = list(
               "A" = list(
                 x0 = 40,
                 x1 = 40,
                 y0 = 10,
                 y1 = 7
               ),
               "B" = list(
                 x0 = 51,
                 x1 = 47,
                 y0 = 8,
                 y1 = 6
               ),
               "C" = list(
                 x0 = 50.5,
                 x1 = 46.5,
                 y0 = 2.25,
                 y1 = 0
               )
             )) %>%
  scr_label_phase(facet = "Ana",
                  cex = 1.25,
                  adj = 0.5,
                  labels = list(
                    "S+/S-" = list(x = 40,
                                   y = 11),
                    "S+"    = list(x = 52,
                                   y = 8.5),
                    "Mix"   = list(x = 51.5,
                                   y = 2.5))) %>%
  scr_xlabel("Session") %>%               # Override x-axis label (bottom only shown by default)
  scr_ylabel("Cumulative Selections") %>% # Override y-axis label (centered, leftmost label)
  scr_title("Participant Choice of Schedules")
