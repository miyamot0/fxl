# Shawn Gilroy, 2021 GPLv2+
#
# demo/cigarettepolicy.R
#
# This file illustrates how to construct a figure,
# that used timesseries data in FXL along with predictions
# drawn from a model. This is useful for communicating SCRD
# findings in conjunction with statistical modeling.
#

library(fxl) # core plotting methods
library(magrittr)
library(extrafont) # using custom font

extrafont::choose_font("Open Sans")

newFrame <- GelinoEtAl2022

xticks <- c(
   "4/13/18", "4/20/18", "4/27/18",
   "10/26/18", "11/2/18", "4/19/19",
   "4/26/19", "5/3/19", "11/1/19",
   "11/8/19", "11/15/19"
)

yticks <- c(
  "0", "50", "100", "150",
  "200", "250", "300", "350",
  "400", "450", "500"
)

scr_plot(newFrame,
  aesthetics = var_map(
    x = Time,
    y = Count1,
    p = Condition,
    facet = Facet
  ),
  mai = c(0.85, 0.6, 0, 0.25),
  family = "Open Sans",
  omi = c(0.25, 0.25, 0.5, 0.5)
) %>%
scr_xoverride(
  c(0.4, 11),
  xticks = seq_len(11),
  xrotation = 45,
  xtickslabs = xticks,
  xlabeloffset = 30
) %>%
scr_yoverride(
  c(-10, 500),
  yticks = c(
    0, 50, 100, 150,
    200, 250, 300, 350,
    400, 450, 500
  ),
  ydelta = 50,
  ytickslabs = yticks
) %>%
scr_lines() %>%
scr_points(
  cex = 2,
  pch = 21,
  fill = "gray"
) %>%
scr_points(
  cex = 2,
  pch = 22,
  mapping = list(
    x = Time,
    y = Count2,
    p = Condition
  )
) %>%
scr_lines(
  mapping = list(
    x = Time,
    y = Count2,
    p = Condition
  )
) %>%
scr_points(
  cex = 2,
  pch = 23,
  mapping = list(
    x = Time,
    y = Count3,
    p = Condition
  )
) %>%
scr_lines(
  mapping = list(
    x = Time,
    y = Count3,
    p = Condition
  )
) %>%
scr_lines(
  mapping = list(
    x = Time,
    y = Count4,
    p = Condition
  )
) %>%
scr_points(
  cex = 2,
  pch = 24,
  fill = "gray",
  mapping = list(
    x = Time,
    y = Count4,
    p = Condition
  )
) %>%
scr_lines(
  size = 1,
  mapping = list(
    x = Time,
    y = yhat,
    p = Condition
  ),
  lty = 3,
  color = "black") %>%
  scr_label_phase(
  facet = "1",
  cex = 1.25,
  adj = 0.5,
  y = 525,
  labels = list(
    "Pre-Policy" = list(x = 2),
    "Policy Enacted" = list(x = 7)
  )) %>%
scr_plines(
  lty = 1,
  lines = list(
    "1" = list(
      "A" = list(
        x1 = 3.5,
        y1 = 500,
        y2 = 1
      )
    )
  )
) %>%
scr_xlabel("Collection Period") %>%
scr_ylabel("                          Frequency of Butts Collected") %>%
scr_legend(
  panel = "1",
  position = "topright",
  legend = c(
    "Location A",
    "Location B",
    "Location C",
    "Location D"
  ),
  col = c(
    "black",
    "black",
    "black",
    "black"
  ),
  pt_bg = c(
    "gray",
    "black",
    "black",
    "gray"
  ),
  lty = c(1, 1, 1, 1),
  pch = c(21, 22, 23, 24),
  bg = c(
    "black",
    "black",
    "black",
    "black"
  ),
  bty = "n",
  pt_cex = 1.5,
  cex = 1,
  text_col = "black",
  horiz = FALSE,
  box_lty = 0
)
# %>%
# scr_save(
#   name = "../man/figures/cigarettepolicy.svg",
#   format = "svg",
#   height = 6,
#   width = 9
# )
