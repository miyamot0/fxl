
library(dplyr) # included for infix logic
library(fxl)   # core plotting methods

minX = 1
maxX = 30

set.seed(65535)

xs  <- 1:30
ys  <- 0 + 5*xs + rnorm(length(xs), 0, 2)

ys2 <- 0.2 + 0.1*xs + rnorm(length(xs), 0, 0.25)

ys2[2] <- 0

semiLogData = data.frame(
  Session   = xs,
  SkillA    = ys,
  SkillB    = ys2
)

scr_plot(semiLogData, aesthetics = list(x     = Session,
                                        y     = SkillA),
         omi    = c(0.0, 0.35, 0.2, 0.25),
         mai    = c(0.0, 0.25, 0.1, 0.25),
         semilog = TRUE) %>%
  scr_yoverride(c(0.1, 1000)) %>%
  scr_title("Semi-log Chart: Hypothetical Case") %>%
  scr_xlabel("Session") %>%
  scr_ylabel("Frequency") %>%
  scr_lines() %>%
  scr_points(pch = 21,
             fill = 'black',
             cex = 2) %>%
  scr_lines(mapping = list(x = Session,
                           y = SkillB)) %>%
  scr_points(pch = 21,
             fill = 'gray',
             cex = 2,
             mapping = list(x = Session,
                            y = SkillB)) %>%
  scr_label_phase(cex = 1,
                  adj = 0.5,
                  labels = list(
                    "Annotated Labels on Plot" = list(x = 5,
                                                      y = 150),
                    "Labels for Phase Changes" = list(x = 18,
                                                      y = 15),
                    "Logarithmic Plotting, With Zero Visualizations" = list(x = 6,
                                                                           y = 3,
                                                                           cex = 1)
                  )) %>%
  scr_arrows(length = 0.1,
             arrows = list(
               "A" = list(
                 x0 = 5,
                 x1 = 7,
                 y0 = 125,
                 y1 = 50
               ),
               "B" = list(
                 x0 = 18,
                 x1 = 14,
                 y0 = 21,
                 y1 = 55
               )
             )) %>%
  scr_brackets(length = 0.1,
               brackets = list(
                 "A" = list(
                   x0 = 1.5,
                   x1 = 10.5,
                   y0 = 2.5,
                   y1 = 1.5
                 )
               )) %>%
  scr_guide_line(color  = 'black',
                 lty    = 1,
                 coords = list(
                   "A"  = list(x0 = 9.5, x1 = 9.5,
                               y0 = 10,  y1 = 500),
                   "B"  = list(x0 = 1,   x1 = 9,
                               y0 = 8,   y1 = 50,
                               col = 'green',
                               lty = 2,
                               lwd = 3),
                   "C"  = list(x0 = 10,  x1 = 30,
                               y0 = 50,  y1 = 150,
                               col = 'orange',
                               lty = 2,
                               lwd = 3))) %>%

  scr_legend(position = "topright",               # Specify legend location
             legend = c("Skill A",                # labels to include (ordered)
                        "Skill B"),
             col    = c('black',                  # color of markers (ordered)
                        'black'),
             m.col  = c('black',                  # color of markers (ordered)
                        'gray'),
             bg       = 'white',
             lty      = c(1, 1),                  # line types (ordered)
             pch      = c(21, 21),                # marker types (ordered)
             bty      = "y",                      # remove border
             pt.cex   = 2.25,                     # point size scale
             cex      = 1.5,                      # text size scale
             text.col = "black",                  # text color
             horiz    = FALSE,                    # list items vertically
             box.lty  = 1)

  #%>%
  # scr_save(name = "semilogfigure.svg",
  #          format = "svg",
  #          units = "in",
  #          height = 6,
  #          width = 9) %>%
  # scr_save(name = "semilogfigure.png",
  #          format = "png",
  #          units = "in",
  #          height = 6,
  #          width = 9,
  #          res = 300)
