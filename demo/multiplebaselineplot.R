library(dplyr)
library(fxl)

data = Gilroyetal2015

scr_plot(data, aesthetics = list(x     = Session,
                                y     = Responding,
                                p     = Condition,
                                facet = Participant),
        mai = c(0.375, 0.375, 0.1, 0.1),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_xoverride(c(1, 27)) %>%
  scr_yoverride(c(0, 109), ydelta = 10) %>%
  scr_points(cex = 2) %>%
  scr_lines(size = 1) %>%
  scr_label_phase(facet = "Andrew",
                 cex   = 1.25,
                 adj   = 0.5,
                 labels = list(
                   "Baseline"       = list(x = 2.5,
                                           y = 107),
                   "Treatment"      = list(x = 9,
                                           y = 107),
                   "Maintenance"    = list(x = 19,
                                           y = 107),
                   "Generalization" = list(x = 26,
                                           y = 107))) %>%
  scr_label_facet(cex   = 1.5,
                  adj   = 1,
                  labels = list(
                    "Andrew"  = list(x = 27,
                                     y = 10),
                    "Brian"   = list(x = 27,
                                     y = 10),
                    "Charles" = list(x = 27,
                                     y = 10)
                  )) %>%
  scr_plines_mbd(lines = list(
    "A" = list(
      "Andrew"  = list(x1 = 4.5,  y1 = 100,
                       x2 = 4.5,  y2 = 0),
      "Brian"   = list(x1 = 11.5, y1 = 100,
                       x2 = 11.5, y2 = 0),
      "Charles" = list(x1 = 18.5, y1 = 100,
                       x2 = 18.5, y2 = 0)
    ),
    "B" = list(
      "Andrew"  = list(x1 = 13.5,  y1 = 100,
                       x2 = 13.5,  y2 = 0),
      "Brian"   = list(x1 = 20.5, y1 = 100,
                       x2 = 20.5, y2 = 0),
      "Charles" = list(x1 = 23.5, y1 = 100,
                       x2 = 23.5, y2 = 0)
    ),
    "C" = list(
      "Andrew"  = list(x1 = 23.5,  y1 = 100,
                       x2 = 23.5,  y2 = 0),
      "Brian"   = list(x1 = 23.5, y1 = 100,
                       x2 = 23.5, y2 = 0),
      "Charles" = list(x1 = 23.5, y1 = 100,
                       x2 = 23.5, y2 = 0)
    )
  )) %>%
  scr_xlabel("Session") %>%
  scr_ylabel("Percent Accuracy") %>%
  scr_title("Rates of Acquisition across Participants")

# %>%
#   scr_save(name = "multiplebaselinefigure.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
