# Shawn Gilroy, 2021 GPLv2+
#
# demo/faplotintegrity.R
#
# This file illustrates how to construct a basic,
# no frills visual of a functional analysis but with
# integrity data mapped alongside the rates of target behavior.
#

library(fxl) # core plotting methods
library(magrittr)

data <- Gilroyetal2019

set.seed(65535)
data$Integrity <- sample(
  80:100,
  nrow(data),
  replace = TRUE
)

scr_plot(
  data,
  aesthetics = var_map(
    x = Session,
    y = CTB,
    p = Condition
  ),
  mai = c(
    0.5,
    0.5,
    0.1,
    0.5
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
) %>%
scr_yoverride(
  c(-.175, 5),
  yticks = c(0, 1, 2, 3, 4, 5),
  ytickslabs = c("0", "1", "2", "3", "4", "5")
) %>%
scr_xoverride(
  c(0.5, 15.5),
  xticks = 1:15,
  xtickslabs = as.character(1:15)
) %>%
scr_bar_support(
  mapping = list(
    x = Session,
    y = Integrity
  ),
  color = rgb(.8, .8, .8, alpha = 1),
  label = "Procedural Fidelity"
) %>%
scr_lines(
  size = 1
) %>%
scr_points(
  cex = 2,
  pch = list(
    "Toy Play" = 16,
    "Attention" = 22,
    "Demand" = 24,
    "Tangible" = 8
  ),
  fill = list(
    "Toy Play" = "black",
    "Attention" = "white",
    "Demand" = "white",
    "Tangible" = "black"
  )
) %>%
scr_xlabel("Session") %>%
scr_ylabel("Combined Target Behavior (Per Minute)") %>%
scr_title("Analog Functional Analysis w/ Integrity Information") %>%
scr_legend(
  position = "right", # Specify legend location
  legend = c(
    "Toy Play", # labels to include (ordered)
    "Attention",
    "Demand",
    "Tangible"
  ),
  col = c(
    "black", # color of markers (ordered)
    "black",
    "black",
    "black"
  ),
  bg = "white",
  pt_bg = c(
    "black", # color of markers (ordered)
    "black",
    "black",
    "black"
  ),
  lty = c(1, 1, 1, 1), # line types (ordered)
  pch = c(16, 22, 24, 8), # marker types (ordered)
  bty = "o", # remove border
  pt_cex = 2.25, # point size scale
  cex = 1.25, # text size scale
  text_col = "black", # text color
  horiz = FALSE, # list items vertically
  box_lty = 1)

# %>%
#   scr_save(name = "fafigureintegrity.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9) %>%
#   scr_save(name = "fafigureintegrity.png",
#            format = "png",
#            units = "in",
#            res = 300,
#            height = 6,
#            width = 9)
