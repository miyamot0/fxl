# Shawn Gilroy, 2021 GPLv2+
#
# demo/concurrentplot.R
#
# This file illustrates how to construct a collection of reversals that are
# illustrated across participants using shared phase change lines
#

library(dplyr) # included for infix logic
library(fxl)   # core plotting methods

# Data frame from prior study, relevant columns include:
#
# Participant: participant label (i.e., each facet)
# Session:     session number of data
# Condition:   condition where data emerged from
# Responding:  measurement of behavior
# Reinforcers: another type of phenomena being measured

data = Gilroyetal2021

# Initial setup, sets core aesthetics (what data to show where)
scr_plot(data, aesthetics = list(x = Session,
                                 y = Responding,
                                 p = Condition,
                                 facet = Participant),
        mai = c(0.375, 0.375, 0.1, 0.1),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_xoverride(c(1, 25)) %>%                           # manually override x-axis
  scr_yoverride(list(                                   # manually override y-axis and tick interval (tick every 5 units, individal overrides)
    "John"    = list(y0 = 0,
                     y1 = 20),
    "Anthony" = list(y0 = 0,
                     y1 = 10),
    "Charles" = list(y0 = 0,
                     y1 = 20)),
    ydelta = 5) %>%
  scr_points(cex = 2) %>%                               # plot points, using x/y from aesthetics
  scr_points(cex = 2,                                   # plot more points, but override aesthetics and style (to show second series)
             pch = 2,
             mapping = list(x = Session,
                            y = Reinforcers)) %>%
  scr_lines(size = 1) %>%                               # plot lines, using x/y from aesthetics
  scr_lines(size = 1,                                   # plot more lines, but override aesthetics and style (to show second series)
            lty = 2,
            mapping = list(x = Session,
                           y = Reinforcers)) %>%
  scr_label_phase(facet = "John",                       # plot a set of labels on specific facet (typically the top)
                  cex    = 1.25,
                  adj    = 0.5,
                  labels = list(                        # list of labels to draw (will use assigned key for label)
                    "Baseline"     = list(x = 2,
                                          y = 20),
                    "FR-Lowest"    = list(x = 5,
                                          y = 20),
                    "Baseline"     = list(x = 8,
                                          y = 20),
                    "FR-Inelastic" = list(x = 11,
                                          y = 20),
                    "FR-Elastic"   = list(x = 14,
                                          y = 20),
                    "FR-Inelastic" = list(x = 18,
                                          y = 20)
                  )) %>%
  scr_label_facet(cex = 1.5,                            # plot labels across facets (not within a single facet)
                  adj = 1,
                  labels = list(                        # list of labels to draw (will use assigned key for label)
                    "John"    = list(x = 25,
                                     y = 5),
                    "Anthony" = list(x = 25,
                                     y = 10),
                    "Charles" = list(x = 25,
                                     y = 21)
                  )) %>%
  scr_plines_mbd(lines = list(                          # plot linked phase lines (note: drawn from top through bottom)
    "A" = list(                                         # list of phase lines to draw (where to draw lines across each participant)
      "John"    = list(x1 = 3.5, y1 = 20,
                       x2 = 3.5, y2 = 0),
      "Anthony" = list(x1 = 3.5, y1 = 10,
                       x2 = 3.5, y2 = 0),
      "Charles" = list(x1 = 3.5, y1 = 20,
                       x2 = 3.5, y2 = 0)
    ),
    "B" = list(
      "John"    = list(x1 = 6.5, y1 = 20,
                       x2 = 6.5, y2 = 0),
      "Anthony" = list(x1 = 6.5, y1 = 10,
                       x2 = 6.5, y2 = 0),
      "Charles" = list(x1 = 8.5, y1 = 20,
                       x2 = 8.5, y2 = 0)
    ),
    "C" = list(
      "John"    = list(x1 = 9.5, y1 = 20,
                       x2 = 9.5, y2 = 0),
      "Anthony" = list(x1 = 9.5, y1 = 10,
                       x2 = 9.5, y2 = 0),
      "Charles" = list(x1 = 11.5, y1 = 20,
                       x2 = 11.5, y2 = 0)
    ),
    "D" = list(
      "John"    = list(x1 = 12.5, y1 = 20,
                       x2 = 12.5, y2 = 0),
      "Anthony" = list(x1 = 16.5, y1 = 10,
                       x2 = 16.5, y2 = 0),
      "Charles" = list(x1 = 16.5, y1 = 20,
                       x2 = 16.5, y2 = 0)
    ),
    "E" = list(
      "John"    = list(x1 = 15.5, y1 = 20,
                       x2 = 15.5, y2 = 2), # Note, bumped up slightly to prevent overlap
      "Anthony" = list(x1 = 22.5, y1 = 10,
                       x2 = 22.5, y2 = 0),
      "Charles" = list(x1 = 19.5, y1 = 20,
                       x2 = 19.5, y2 = 0)
    )
  )) %>%
  scr_xlabel("Session") %>%
  scr_ylabel("Frequency (Responses, Reinforcers Delivered)") %>%
  scr_title("Individual Evaluations of Reinforcer Efficacy and Elasticity across Reinforcers") %>%
  scr_legend(panel    = "John",
             position = "topright",
             legend   = c("Responses Observed", "Reinforcers Produced"),
             col      = c('black', 'black'),
             lty      = c(1, 2),
             pch      = c(19, 2),
             bty      = "n",
             pt.cex   = 2.25,
             cex      = 1.25,
             text.col = "black",
             horiz    = F,
             box.lty  = 0)

# Optional Save
#
# %>%
#   scr_save(name = "concurrentfigure.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
