rm(list = ls())


library(dplyr) # included for infix logic
#library(fxl)   # core plotting methods

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
  scr_label_phase(cex = 1.5,
                  adj = 0.5,
                  labels = list(
                    "Annotated Labels on Plot" = list(x = 5,
                                                      y = 150)
                  )) %>%
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
             box.lty  = 1)                        # change box size (0 = removed)




