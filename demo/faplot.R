# Shawn Gilroy, 2021 GPLv2+
#
# demo/faplot.R
#
# This file illustrates how to construct a basic,
# no frills visual of a functional analysis
#

library(dplyr) # included for infix logic
library(fxl) # core plotting methods

data <- Gilroyetal2019

scr_plot(
  data,
  aesthetics = list(
    x = Session,
    y = CTB,
    p = Condition
  ),
  mai = c(0.5, 0.5, 0.1, 0.5),
  omi = c(0.25, 0.25, 0.25, 0.25)
) %>%
scr_yoverride(c(0, 2)) %>% # manually override y-axis
scr_lines(size = 1) %>% # plot lines, using x/y from aesthetics
scr_points(
  cex = 2, # plot points, using x/y from aesthetics
  pch = list(# override point marker types (match FA conventions)
    "Toy Play" = 16,
    "Attention" = 22,
    "Demand" = 24,
    "Tangible" = 8
  ),
  fill = list(# override point marker colors (match FA conventions)
    "Toy Play" = "black",
    "Attention" = "white",
    "Demand" = "white",
    "Tangible" = "black"
  )
) %>%
scr_xlabel("Session") %>%
scr_ylabel("Combined Target Behavior (Per Minute)") %>%
scr_title("Analog Functional Analysis") %>%
scr_legend(
  position = "topright", # Specify legend location
  legend = c(
    "Toy Play",
    "Attention",
    "Demand",
    "Tangible"
  ),
  col = c(
    "black",
    "black",
    "black",
    "black"
  ),
  pt_bg = c(
    "black",
    "black",
    "black",
    "black"
  ),
  lty = c(
    1,
    1,
    1,
    1
  ),
  pch = c(
    16,
    22,
    24,
    8
  ),
  bty = "n",
  pt_cex = 2.25,
  cex = 1.25,
  text_col = "black",
  horiz = FALSE,
  box_lty = 0)

# %>%
#   scr_save(name = "fafigure.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
