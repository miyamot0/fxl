# Shawn Gilroy, 2021 GPLv2+
#
# demo/faplot.R
#
# This file illustrates how to construct a basic, no frills visual of a functional analysis
#

library(dplyr) # included for infix logic
library(fxl)   # core plotting methods

# Data frame from prior study, relevant columns include:
#
# Session:     session number of data
# Condition:   condition where data emerged from
# CTB:         measurement of behavior

data = Gilroyetal2019

# Initial setup, sets core aesthetics (what data to show where)
scr_plot(data, aesthetics = list(x = Session,
                                 y = CTB,
                                 p = Condition),
        mai = c(0.5, 0.5, 0.1, 0.5),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_yoverride(c(0, 2)) %>%                                # manually override y-axis and tick interval (tick every 10 units)
  scr_lines(size = 1) %>%                                   # plot lines, using x/y from aesthetics
  scr_points(cex = 2,                                       # plot points, using x/y from aesthetics
             pch = list(                                    # override point marker types (match FA conventions)
               "Toy Play" = 16,
               "Attention" = 22,
               "Demand" = 24,
               "Tangible" = 8
             ),
             fill = list(                                   # override point marker colors (match FA conventions)
               "Toy Play" = 'black',
               "Attention" = 'white',
               "Demand" = 'white',
               "Tangible" = 'black'
             )) %>%
  scr_xlabel("Session") %>%                                 # Override x-axis label (bottom only shown by default)
  scr_ylabel("Combined Target Behavior (Per Minute)") %>%   # Override y-axis label (centered, leftmost label)
  scr_title("Analog Functional Analysis") %>%
  scr_legend(position = "topright",                         # Specify legend
             legend = c("Toy Play",                         # labels to include
                        "Attention",
                        "Demand",
                        "Tangible"),
             col    = c('black',                            # color of markers
                        'white',
                        'white',
                        'black'),
             lty      = c(1, 1, 1, 1),                      # line types
             pch      = c(16, 22, 24, 8),                   # marker types
             bty      = "n",                                # remove border
             pt.cex   = 2.25,                               # point size scale
             cex      = 1.25,                               # text size scale
             text.col = "black",                            # text color
             horiz    = F,                                  # list items vertically
             box.lty  = 0)                                  # change box size (0 = removed)

# Optional Save
#
# %>%
#   scr_save(name = "fafigure.svg",
#            format = "svg",
#            units = "in",
#            height = 6,
#            width = 9)
