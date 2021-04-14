# Shawn Gilroy, 2021 GPLv2+

library(dplyr)
library(fxl)

data = Gilroyetal2019

scr_plot(data, aesthetics = list(x = Session,
                                        y = CTB,
                                        p = Condition),
        mai = c(0.5, 0.5, 0.1, 0.5),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_lines(size = 1) %>%
  scr_points(cex = 2,
             pch = list(
               "Toy Play" = 16,
               "Attention" = 22,
               "Demand" = 24,
               "Tangible" = 8
             ),
             fill = list(
               "Toy Play" = 'black',
               "Attention" = 'white',
               "Demand" = 'white',
               "Tangible" = 'black'
             )) %>%
  scr_xlabel("Session") %>%
  scr_ylabel("Combined Target Behavior (Per Minute)") %>%
  scr_yoverride(c(0, 2)) %>%
  scr_title("Analog Functional Analysis") %>%
  scr_legend(position = "topright",
            legend = c("Toy Play",
                       "Attention",
                       "Demand",
                       "Tangible"),
            col = c('black',
                       'white',
                       'white',
                       'black'),
            lty = c(1, 1, 1, 1),
            pch = c(16, 22, 24, 8),
            bty = "n",
            pt.cex = 2.25,
            cex = 1.25,
            text.col = "black",
            horiz = F,
            box.lty = 0)

# %>%
#   scr_save(name = "fafigure.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
