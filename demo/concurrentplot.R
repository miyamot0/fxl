# Shawn Gilroy, 2021 GPLv2+

library(dplyr)
library(fxl)

data = Gilroyetal2021

scr_plot(data, aesthetics = list(x = Session,
                                y = Responding,
                                p = Condition,
                                facet = Participant),
        mai = c(0.375, 0.375, 0.1, 0.1),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_xoverride(c(1, 25)) %>%
  scr_yoverride(list(
    "John" = list(y0 = 0,
                     y1 = 20),
    "Anthony" = list(y0 = 0,
                     y1 = 10),
    "Charles" = list(y0 = 0,
                     y1 = 20)),
    ydelta = 5) %>%
  scr_points(cex = 2) %>%
  scr_points(cex = 2,
             pch = 2,
             mapping = list(x = Session,
                            y = Reinforcers)) %>%
  scr_lines(size = 1) %>%
  scr_lines(size = 1,
            lty = 2,
            mapping = list(x = Session,
                           y = Reinforcers)) %>%
  scr_label_phase(facet = "John",
                  cex = 1.25,
                  adj = 0.5,
                  labels = list(
                    "Baseline" = list(x = 2,
                                          y = 20),
                    "FR-Lowest" = list(x = 5,
                                          y = 20),
                    "Baseline" = list(x = 8,
                                          y = 20),
                    "FR-Inelastic" = list(x = 11,
                                          y = 20),
                    "FR-Elastic" = list(x = 14,
                                          y = 20),
                    "FR-Inelastic" = list(x = 18,
                                          y = 20)
                  )) %>%
  scr_label_facet(cex = 1.5,
                  adj = 1,
                  labels = list(
                    "John" = list(x = 25,
                                     y = 5),
                    "Anthony" = list(x = 25,
                                     y = 10),
                    "Charles" = list(x = 25,
                                     y = 21)
                  )) %>%
  scr_plines_mbd(lines = list(
    "A" = list(
      "John" = list(x1 = 3.5, y1 = 20,
                       x2 = 3.5, y2 = 0),
      "Anthony" = list(x1 = 3.5, y1 = 10,
                       x2 = 3.5, y2 = 0),
      "Charles" = list(x1 = 3.5, y1 = 20,
                       x2 = 3.5, y2 = 0)
    ),
    "B" = list(
      "John" = list(x1 = 6.5, y1 = 20,
                       x2 = 6.5, y2 = 0),
      "Anthony" = list(x1 = 6.5, y1 = 10,
                       x2 = 6.5, y2 = 0),
      "Charles" = list(x1 = 8.5, y1 = 20,
                       x2 = 8.5, y2 = 0)
    ),
    "C" = list(
      "John" = list(x1 = 9.5, y1 = 20,
                       x2 = 9.5, y2 = 0),
      "Anthony" = list(x1 = 9.5, y1 = 10,
                       x2 = 9.5, y2 = 0),
      "Charles" = list(x1 = 11.5, y1 = 20,
                       x2 = 11.5, y2 = 0)
    ),
    "D" = list(
      "John" = list(x1 = 12.5, y1 = 20,
                       x2 = 12.5, y2 = 0),
      "Anthony" = list(x1 = 16.5, y1 = 10,
                       x2 = 16.5, y2 = 0),
      "Charles" = list(x1 = 16.5, y1 = 20,
                       x2 = 16.5, y2 = 0)
    ),
    "E" = list(
      "John" = list(x1 = 15.5, y1 = 20,
                       x2 = 15.5, y2 = 2),
      "Anthony" = list(x1 = 22.5, y1 = 10,
                       x2 = 22.5, y2 = 0),
      "Charles" = list(x1 = 19.5, y1 = 20,
                       x2 = 19.5, y2 = 0)
    )
  )) %>%
  scr_xlabel("Session") %>%
  scr_ylabel("Frequency (Responses, Reinforcers Delivered)") %>%
  scr_title("Individual Evaluations of Reinforcer Efficacy and Elasticity across Reinforcers") %>%
  scr_legend(panel = "John",
            position = "topright",
            legend = c("Responses Observed", "Reinforcers Produced"),
            col = c('black', 'black'),
            lty = c(1, 2),
            pch = c(19, 2),
            bty = "n",
            pt.cex = 2.25,
            cex = 1.25,
            text.col = "black",
            horiz = F,
            box.lty = 0)

# %>%
#   scr_save(name = "concurrentfigure.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
