# Shawn Gilroy, 2021 GPLv2+
#
# demo/faplotintegrity.R
#
# This file illustrates how to construct a basic, no frills visual of a functional analysis but with integrity data mapped alongside the rates of target behavior.
#

library(dplyr) # included for infix logic
library(fxl)   # core plotting methods

# Data frame from prior study, relevant columns include:
#
# Session:     session number of data
# Condition:   condition where data emerged from
# CTB:         measurement of behavior

data = Gilroyetal2019

set.seed(65535)
data$Integrity = sample(80:100,
                        nrow(data),
                        replace = TRUE)

# Integrity:    some randomly generated integrity data

# Initial setup, sets core aesthetics (what data to show where)
scr_plot(data, aesthetics = list(x = Session,
                                 y = CTB,
                                 p = Condition),
        mai = c(0.5, 0.5, 0.1, 0.5),
        omi = c(0.25, 0.25, 0.25, 0.25)) %>%
  scr_yoverride(c(0, 5)) %>%                                # manually override y-axis
  scr_bar_support(mapping = list(x = Session,
                                 y = Integrity),
                  color = rgb(.8,.8,.8,
                              alpha = 1),
                  label = "Procedural Fidelity") %>%
  scr_lines(size = 1) %>%                                   # plot lines, using x/y from aesthetics
  scr_points(cex = 2,                                       # plot points, using x/y from aesthetics
             pch = list(                                    # override point marker types (match FA conventions)
               "Toy Play"  = 16,
               "Attention" = 22,
               "Demand"    = 24,
               "Tangible"  = 8
             ),
             fill = list(                                   # override point marker colors (match FA conventions)
               "Toy Play"  = 'black',
               "Attention" = 'white',
               "Demand"    = 'white',
               "Tangible"  = 'black'
             )) %>%
  scr_xlabel("Session") %>%                                 # Override x-axis label (bottom only shown by default)
  scr_ylabel("Combined Target Behavior (Per Minute)") %>%   # Override y-axis label (centered, leftmost label)
  scr_title("Analog Functional Analysis w/ Integrity Information") %>%
  scr_legend(position = "right",                         # Specify legend location
             legend = c("Toy Play",                         # labels to include (ordered)
                        "Attention",
                        "Demand",
                        "Tangible"),
             col    = c('black',                            # color of markers (ordered)
                        'black',
                        'black',
                        'black'),
             bg       = "white",
             pt.bg    = c('black',                            # color of markers (ordered)
                          'black',
                          'black',
                          'black'),
             lty      = c(1, 1, 1, 1),                      # line types (ordered)
             pch      = c(16, 22, 24, 8),                   # marker types (ordered)
             bty      = "o",                                # remove border
             pt.cex   = 2.25,                               # point size scale
             cex      = 1.25,                               # text size scale
             text.col = "black",                            # text color
             horiz    = F,                                  # list items vertically
             box.lty  = 1)

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
