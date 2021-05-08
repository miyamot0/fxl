library(fxl)

KoffarnusEtAl2011 = KoffarnusEtAl2011 %>%
  mutate(facet = ifelse(ID < 40, "1",
                        ifelse(ID > 82, "3", "2"))) %>%
  arrange(-ID)

# TODO: adaptable MAI/OMI

scr_plot(KoffarnusEtAl2011,
         aesthetics = list(x = X, y = ID,
                           p = Code, facet = facet),
         mai     = c(0.0,   0.0,   0.1,  0.0),
         omi     = c(0.725, 0.725, 0.0, 0.0)) %>%
  scr_yoverride(list(                                   # manually override y-axis and tick interval (tick every 5 units, individal overrides)
    "1" = list(y0 = 0,
               y1 = 40,
               yticks = c(5, 10, 15, 20, 25, 30, 35)),
    "2" = list(y0 = 40,
               y1 = 85,
               yticks = c(40, 45, 50, 55, 60, 65, 70, 75, 80)),
    "3" = list(y0 = 80,
               y1 = 125,
               yticks = c(85, 90, 95, 100, 105, 110, 115, 120))),
    ydelta = 5) %>%
  scr_ylabel("Participant Number", line = 3) %>%
  scr_xoverride(var = c(1, 135), xdelta = 5) %>%
  scr_xlabel("Consecutive Work Day", line = 3) %>%
  scr_points(cex = list(                                    # override point marker types (match FA conventions)
               "0"  = 1,
               "1"  = 1,
               "2"  = 0.6
             ),                                       # plot points, using x/y from aesthetics
             pch = list(                                    # override point marker types (match FA conventions)
               "0"  = 22,
               "1"  = 22,
               "2"  = 22
             ),
             fill = list(                                   # override point marker colors (match FA conventions)
               "0" = 'black',
               "1" = 'white',
               "2" = rgb(.8,
                         .8,
                         .8,
                         alpha = 0.25)
             ),
             color = list(                                   # override point marker colors (match FA conventions)
               "0" = 'transparent',
               "1" = 'black',
               "2" = rgb(.8,
                         .8,
                         .8,
                         alpha = 0.85)
             )) %>%
  scr_save(name = "silvermanfigure.png",
           format = "png",
           units = "in",
           height = 9,
           res = 1000,
           width = 11)
