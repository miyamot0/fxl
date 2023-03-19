## ----structure_core, fig.width=7, fig.height=4--------------------------------
suppressPackageStartupMessages(library(fxl))

scr_plot(
  Gilroyetal2015, # Data frame (long format)
  aesthetics = var_map(
    x = Session, # Column name for x-axis values
    y = Responding, # Column name for y-axis values
    p = Condition, # Column name for phases associated with x-y coordinates
    facet = Participant # Column name distinguishing individual plots/facets
  )
)

## ----structure_layers, fig.width=8, fig.height=4------------------------------
scr_plot(
  Gilroyetal2015, 
  aesthetics = var_map(
    x = Session, 
    y = Responding, 
    p = Condition, 
    facet = Participant 
  )
) |>
scr_points(cex = 2) |> # plot points, using x/y from aesthetics
scr_lines(size = 1) # plot line, using x/y from aesthetics

## ----structure_labels, fig.width=8, fig.height=4------------------------------
scr_plot(
  Gilroyetal2015, 
  aesthetics = var_map(
    x = Session, 
    y = Responding, 
    p = Condition, 
    facet = Participant 
  )
) |>
scr_points(cex = 2) |> 
scr_lines(size = 1) |> 
scr_xoverride(c(0.25, 27.5),
  xticks = 1:27,
  xtickslabs = as.character(1:27)
) |> # manually override x-axis (make extra room for labels) and specify ticks
scr_yoverride(c(-5, 105), 
  yticks = c(0, 50, 100),
  ytickslabs = as.character(c(0, 50, 100)),
) |> # manually override y-axis and tick interval (tick for every 10 units)
scr_xlabel("Session") |> # Override x-axis label (bottom only shown by default)
scr_ylabel("Percent Accuracy") |> # Override y-axis label (centered, leftmost label)
scr_title("Rates of Acquisition across Participants") 


## ----structure_lines, fig.width=8, fig.height=4-------------------------------
scr_plot(
  Gilroyetal2015, 
  aesthetics = var_map(
    x = Session, 
    y = Responding, 
    p = Condition, 
    facet = Participant 
  )
) |>
scr_points(cex = 2) |> 
scr_lines(size = 1) |> 
scr_xoverride(c(0.25, 27.5),
  xticks = 1:27,
  xtickslabs = as.character(1:27)
) |> 
scr_yoverride(c(-5, 105), 
  yticks = c(0, 50, 100),
  ytickslabs = as.character(c(0, 50, 100)),
) |> 
scr_label_facet( # Draw a label for each facet (i.e., plot)
  cex = 1.5, 
  adj = 1,
  y = 25, # Specify a common height for all labels
  labels = list(
    "Andrew" = list(x = 27), "Brian" = list(x = 27), "Charles" = list(x = 27)
  ) # Labels to draw, for each respective facet, with specific args (i.e., x)
) |>
scr_label_phase(
  facet = "Andrew", # plot labels on specific facet
  cex = 1.25,
  adj = 0.5,
  y = 120,
  labels = list(# list of labels to draw (will use assigned key for label)
    "Baseline" = list(x = 2.5), "Treatment" = list(x = 9),
    "Maintenance" = list(x = 19), "Generalization" = list(x = 26)
  )
) |>
scr_plines_mbd(
  lines = list(# plot linked phase lines (note: drawn from top through bottom)
    "A" = list(
      "Andrew" = list(x1 = 4.5, y1 = 100),
      "Brian" = list(x1 = 11.5, y1 = 100),
      "Charles" = list(x1 = 18.5, y1 = 100, y2 = -5)
  ),
  "B" = list(
    "Andrew" = list(x1 = 13.5, y1 = 100),
    "Brian" = list(x1 = 20.5, y1 = 100),
    "Charles" = list(x1 = 23.5, y1 = 100,y2 = -5)
  ),
  "C" = list(
    "Andrew" = list(x1 = 23.5, y1 = 100),
    "Brian" = list(x1 = 23.5, y1 = 100),
    "Charles" = list(x1 = 23.5, y1 = 100, y2 = -5)
  )
)) |>
scr_xlabel("Session") |> 
scr_ylabel("Percent Accuracy") |> 
scr_title("Rates of Acquisition across Participants") 


## ----structure_save, eval=FALSE-----------------------------------------------
#  scr_plot(
#    Gilroyetal2015,
#    aesthetics = var_map(
#      x = Session,
#      y = Responding,
#      p = Condition,
#      facet = Participant
#    )
#  ) |>
#  scr_points(cex = 2) |>
#  scr_lines(size = 1) |>
#  scr_xoverride(c(0.25, 27.5),
#    xticks = 1:27,
#    xtickslabs = as.character(1:27)
#  ) |>
#  scr_yoverride(c(-5, 105),
#    yticks = c(0, 50, 100),
#    ytickslabs = as.character(c(0, 50, 100)),
#  ) |>
#  scr_label_facet(
#    cex = 1.5,
#    adj = 1,
#    y = 25,
#    labels = list(
#      "Andrew" = list(x = 27),
#      "Brian" = list(x = 27),
#      "Charles" = list(x = 27)
#    )
#  ) |>
#  scr_label_phase(
#    facet = "Andrew",
#    cex = 1.25,
#    adj = 0.5,
#    y = 120,
#    labels = list(
#      "Baseline" = list(x = 2.5), "Treatment" = list(x = 9),
#      "Maintenance" = list(x = 19), "Generalization" = list(x = 26)
#    )
#  ) |>
#  scr_plines_mbd(
#    lines = list(
#      "A" = list(
#        "Andrew" = list(x1 = 4.5, y1 = 100),
#        "Brian" = list(x1 = 11.5, y1 = 100),
#        "Charles" = list(x1 = 18.5, y1 = 100, y2 = -5)
#    ),
#    "B" = list(
#      "Andrew" = list(x1 = 13.5, y1 = 100),
#      "Brian" = list(x1 = 20.5, y1 = 100),
#      "Charles" = list(x1 = 23.5, y1 = 100,y2 = -5)
#    ),
#    "C" = list(
#      "Andrew" = list(x1 = 23.5, y1 = 100),
#      "Brian" = list(x1 = 23.5, y1 = 100),
#      "Charles" = list(x1 = 23.5, y1 = 100, y2 = -5)
#    )
#  )) |>
#  scr_xlabel("Session") |>
#  scr_ylabel("Percent Accuracy") |>
#  scr_title("Rates of Acquisition across Participants") |>
#  scr_save(name = "NewFileName.svg",
#             format = "svg",
#             units = "in",
#             height = 6,
#             width = 9) # Save options for figure
#  

## ----ex_gilroy_et_al_2021, fig.width=9, fig.height=6, echo=FALSE--------------

data <- Gilroyetal2021

scr_plot(
  data,
  aesthetics = var_map(
    x = Session,
    y = Responding,
    p = Condition,
    facet = Participant
  ),
  mai = c(0.375,
          0.375,
          0.175,
          0.1),
  omi = c(0.25,
          0.25,
          0.25,
          0.25)
) |>
scr_xoverride(
  c(0.5, 25),
  xticks = 1:25
) |>
scr_yoverride(
  list(
    "John" = list(
      y0 = -1,
      y1 = 20,
      yticks = c(0, 5, 10, 15, 20)
    ),
    "Anthony" = list(
      y0 = -0.5,
      y1 = 10,
      yticks = c(0, 5, 10)
    ),
    "Charles" = list(
      y0 = -1,
      y1 = 20,
      yticks = c(0, 5, 10, 15, 20)
    )
  ),
  ydelta = 5
) |>
scr_points(
  cex = 2
) |>
scr_points(
  cex = 2,
  pch = 2,
  mapping = list(
    x = Session,
    y = Reinforcers
  )
) |>
scr_lines() |>
scr_lines(
  lty = 2,
  mapping = list(
    x = Session,
    y = Reinforcers
  )
) |>
scr_label_phase(
  facet = "John",
  cex = 1.25,
  adj = 0.5,
  y = 20,
  labels = list(
    "Baseline" = list(
      x = 2
    ),
    "FR-Lowest" = list(
      x = 5
    ),
    "Baseline" = list(
      x = 8
    ),
    "FR-Inelastic" = list(
      x = 11
    ),
    "FR-Elastic" = list(
      x = 14
    ),
    "FR-Inelastic" = list(
      x = 18
    )
  )
) |>
scr_label_facet(
  cex = 1.5,
  adj = 1,
  x = 25,
  labels = list(
    "John" = list(
      y = 2.5
    ),
    "Anthony" = list(
      y = 12
    ),
    "Charles" = list(
      y = 25
    )
  )) |>
scr_plines_mbd(lines = list(
  "A" = list(
    "John" = list(
      x1 = 3.5,
      y1 = 20
    ),
    "Anthony" = list(
      x1 = 3.5,
      y1 = 10
    ),
    "Charles" = list(
      x1 = 3.5,
      y1 = 20,
      y2 = -1
    )
  ),
  "B" = list(
    "John" = list(
      x1 = 6.5,
      y1 = 20
    ),
    "Anthony" = list(
      x1 = 6.5,
      y1 = 10
    ),
    "Charles" = list(
      x1 = 8.5,
      y1 = 20,
      y2 = -1
    )
  ),
  "C" = list(
    "John" = list(
      x1 = 9.5,
      y1 = 20
    ),
    "Anthony" = list(
      x1 = 9.5,
      y1 = 10
    ),
    "Charles" = list(
      x1 = 11.5,
      y1 = 20,
      y2 = -1
    )
  ),
  "D" = list(
    "John" = list(
      x1 = 12.5,
      y1 = 20
    ),
    "Anthony" = list(
      x1 = 16.5,
      y1 = 10
    ),
    "Charles" = list(
      x1 = 16.5,
      y1 = 20,
      y2 = -1
    )
  ),
  "E" = list(
    "John" = list(
      x1 = 15.5,
      y1 = 20,
      y2 = 2
    ),
    "Anthony" = list(
      x1 = 22.5,
      y1 = 10
    ),
    "Charles" = list(
      x1 = 19.5,
      y1 = 20,
      y2 = -1
    )
  )
)) |>
scr_xlabel("Session") |>
scr_ylabel("         Frequency (Responses, Reinforcers Delivered)") |>
scr_title("Individual Evaluations of Reinforcer Efficacy and Elasticity across Reinforcers") |>
scr_legend(
  panel = "John",
  position = "right",
  legend = c("Responses Observed",
             "Reinforcers Produced"),
  col = c("black",
          "black"),
  pt_bg = c("black",
            "black"),
  lty = c(1,
          2),
  pch = c(19,
          2),
  bg = c("black",
         "black"),
  bty = "n",
  pt_cex = 2.25,
  cex = 1.25,
  text_col = "black",
  horiz = FALSE,
  box_lty = 0
)


## ----ex_gilroy_et_al_at, fig.width=9, fig.height=6, echo=FALSE----------------

data <- Gilroyetal2019

scr_plot(
  data,
  aesthetics = var_map(
    x = Session,
    y = CTB,
    p = Condition
  ),
  mai = c(0.5, 0.5, 0.1, 0.5),
  omi = c(0.25, 0.25, 0.25, 0.25)
) |>
scr_xoverride(c(-.5, 15)) |>
scr_yoverride(c(-.05, 2),
              yticks = c(0, 0.5, 1, 1.5, 2),
              ytickslabs = c("0",
                             "0.5",
                             "1",
                             "1.5",
                             "2")) |> # manually override y-axis
scr_lines(size = 1) |> # plot lines, using x/y from aesthetics
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
) |>
scr_xlabel("Session") |>
scr_ylabel("Combined Target Behavior (Per Minute)") |>
scr_title("Analog Functional Analysis") |>
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
    "white",
    "white",
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
  box_lty = 0) |>
  scr_save(name = "../man/figures/fafigure.svg",
           format = "svg",
           units = "in",
           height = 6,
           width = 9)


## ----ex_gilroy_et_al_mbd, fig.width=9, fig.height=6, echo=FALSE---------------
current_data <- Gilroyetal2019Tx
current_data$Condition <- paste0(current_data$Condition, current_data$PhaseNum)
current_data$Function <- current_data$Participant
current_data$AFCR <- current_data$FCR
current_data$EFCR <- current_data$FCR2

scr_plot(current_data,
  aesthetics = var_map(
    x = Session,
    y = CTB,
    p = Condition,
    facet = Function
  ),
  mai = c(0.375, 0.375, 0.25, .25),
  omi = c(0.25, 0.25, 0.25, 0.25)
) |>
scr_yoverride(
  list(
    "Attention" = list(
      y0 = -0.125,
      y1 = 3,
      yticks = c(0, 1, 2, 3)
    ),
    "Demand" = list(
      y0 = -0.125,
      y1 = 3,
      yticks = c(0, 1, 2, 3)
    )
)) |>
scr_xoverride(
  c(-1, 100),
  xdelta = 10,
  xticks = c(1,
             seq(10, 100,
                 by = 10))
) |>
scr_lines(
  size = 1
) |>
scr_lines(
  mapping = list(
    x = Session,
    y = AFCR
  ),
  size = 1,
  lty = 2
) |>
scr_lines(
  mapping = list(
    x = Session,
    y = EFCR
  ),
  size = 1,
  lty = 3
) |>
scr_points(
  fill = "white",
  pch = 21
) |>
scr_points(
  mapping = list(
    x = Session,
    y = AFCR
  ),
  cex = 1,
  pch = 20,
  fill = "black"
) |>
scr_points(
  mapping = list(
    x = Session,
    y = EFCR
  ),
  cex = 0.75,
  pch = 24,
  fill = "black"
) |>
scr_plines_mbd(
  lines = list(
    "A" = list(
      "Attention" = list(
        x1 = 13.5,
        y1 = 3.15,
        y2 = -0.125
      ),
      "Demand" = list(
        x1 = 20,
        y1 = 3,
        y2 = -0.125)
    )
  )
) |>
scr_plines(
  lty = 1,
  lines = list(
    "Attention" = list(
      "A" = list(
        x1 = 25.5,
        y1 = 3
      ),
      "B" = list(
        x1 = 26.5,
        y1 = 3
      ),
      "C" = list(
        x1 = 60.5,
        y1 = 3,
        lty = 3
      ),
      "D" = list(
        x1 = 76.5,
        y1 = 3,
        lty = 3
      )
    ),
    "Demand" = list(
      "A" = list(
        x1 = 34.5,
        y1 = 3
      ),
      "B" = list(
        x1 = 37.5,
        y1 = 3
      ),
      "C" = list(
        x1 = 41.5,
        y1 = 3
      ),
      "D" = list(
        x1 = 50.5,
        y1 = 3,
        lty = 3
      ),
      "E" = list(
        x1 = 72.5,
        y1 = 3,
        lty = 3
      )
    )
  )
) |>
scr_label_facet(
  cex = 1.25,
  adj = 1,
  y = 3.15,
  x = 100,
  labels = list(
    "Attention",
    "Demand"
  )
) |>
scr_label_phase(
  facet = "Attention",
  cex = 0.6,
  adj = 0.5,
  y = 3,
  labels = list(
    "Baseline" = list(
      x = 14,
      y = 3.5
    ),
    "FCR-A + EXT" = list(
      x = 19
    ),
    "FCR-A + EXT" = list(
      x = 32
    ),
    "Parent Implementation" = list(
      x = 68.5
    ),
    "Generalization" = list(
      x = 82
    ),
    "Problem Behavior" = list(
      x = 7,
      y = 1.8
    ),
    "FCR-A" = list(
      x = 20,
      y = 2.5
    ),
    "Add FCR\nOptions" = list(
      x = 31,
      y = 2.5
    )
  )
) |>
scr_label_phase(
  facet = "Attention",
  cex = 0.6,
  adj = 0.5,
  labels = list(
    "5s" = list(
      x = 39,
      y = 2.4
    ),
    "Schedule Thinning" = list(
      x = 54,
      y = 2.4
    ),
    "300s" = list(
      x = 75,
      y = 2.4
    )
  )
) |>
scr_label_phase(
  facet = "Demand",
  cex = 0.6,
  adj = 0.5,
  y = 3,
  labels = list(
    "FCR-E + EXT" = list(
      x = 30,
      y = 3.45
    ),
    "FCR-A P = 0.1" = list(
      x = 36,
      y = 2,
      srt = 90
    ),
    "FCR-A/E + EXT" = list(
      x = 47,
      y = 3.35
    ),
    "Parent Implementation" = list(
      x = 58.5
    ),
    "Generalization" = list(
      x = 78
    ),
    "FCR-E" = list(
      x = 24,
      y = 2.5
    ),
    "FCR-A\nP = 0.1\n200% SR" = list(
      x = 46,
      y = 2
    )
  )
) |>
scr_label_phase(
  facet = "Demand",
  cex = 0.6,
  adj = 0.5,
  y = 1.375,
  labels = list(
    "1" = list(
      x = 30
    ),
    "Demand Fading" = list(
      x = 56
    ),
    "6" = list(
      x = 71.5
    )
  )
) |>
scr_arrows(
  facet = "Attention",
  length = 0.1,
  arrows = list(
    "A" = list(
      x0 = 7,
      x1 = 7,
      y0 = 1.5,
      y1 = 1
    ),
    "B" = list(
      x0 = 20,
      x1 = 20,
      y0 = 2.25,
      y1 = 2
    ),
    "C" = list(
      x0 = 31,
      x1 = 31,
      y0 = 2.25,
      y1 = 2
    )
  )
) |>
scr_arrows(
  facet = "Demand",
  length = 0.1,
  arrows = list(
    "A" = list(
      x0 = 24,
      x1 = 24,
      y0 = 2.25,
      y1 = 1.5
    ),
    "B" = list(
      x0 = 36,
      x1 = 36,
      y0 = 1.3,
      y1 = 0.75
    ),
    "C" = list(
      x0 = 46,
      x1 = 46,
      y0 = 1.5,
      y1 = 0.75
    )
  )
) |>
scr_brackets(
  facet = "Attention",
  length = 0.1,
  brackets = list(
    "A" = list(
      x0 = 8,
      x1 = 26,
      y0 = 3.3,
      y1 = 3
    ),
    "B" = list(
      x0 = 38,
      x1 = 76,
      y0 = 2.25,
      y1 = 1.5,
      lty = 3
    )
  )
) |>
scr_brackets(
  facet = "Demand",
  length = 0.1,
  brackets = list(
    "A" = list(
      x0 = 23,
      x1 = 40,
      y0 = 3.3,
      y1 = 3
    ),
    "B" = list(
      x0 = 36,
      x1 = 47,
      y0 = 3.2,
      y1 = 2.9
    ),
    "C" = list(
      x0 = 29,
      x1 = 72,
      y0 = 1.25,
      y1 = 0.5,
      lty = 3
    )
  )
) |>
scr_guide_line(
  color = "red",
  lty = 3,
  facet = "Attention",
  coords = list(
    "A" = list(
      x0 = 14,
      x1 = 25.5,
      y0 = 0.1
    ),
    "B" = list(
      x0 = 26.5,
      x1 = 100,
      y0 = 0.1
    )
  )
) |>
scr_guide_line(
  color = "red",
  lty = 3,
  facet = "Demand",
  coords = list(
    "A" = list(
      x0 = 20,
      x1 = 100,
      y0 = 0.1
    )
  )
)


## ----ex_gilroy_et_al_2015, fig.width=9, fig.height=6, echo=FALSE--------------
data <- Gilroyetal2015

scr_plot(
  data,
  aesthetics = var_map(
    x = Session,
    y = Responding,
    p = Condition,
    facet = Participant
  ),
  mai = c(
    0.375,
    0.375,
    0.2,
    0.0
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.1
  )
) |>
scr_xoverride(
  c(0.25, 27.5),
  xticks = 1:27,
  xtickslabs = as.character(1:27)
) |> # manually override x-axis (make extra room for labels)
scr_yoverride(
  c(-5, 105), # manually override y-axis and tick interval (tick every 10 units)
  yticks = seq(0, 100, by = 10),
  ytickslabs = as.character(seq(0, 100, by = 10)),
) |>
scr_points(
  cex = 2
) |> # plot points, using x/y from aesthetics
scr_lines(
  size = 1
) |> # plot lines, using x/y from aesthetics
scr_label_phase(
  facet = "Andrew", # plot labels on specific facet
  cex = 1.25,
  adj = 0.5,
  y = 107,
  labels = list(# list of labels to draw (will use assigned key for label)
    "Baseline" = list(
      x = 2.5
    ),
    "Treatment" = list(
      x = 9
    ),
    "Maintenance" = list(
      x = 19
    ),
    "Generalization" = list(
      x = 26
    )
  )
) |>
scr_label_facet(
  cex = 1.5, # plot labels across facets (not within a single facet)
  adj = 1,
  y = 10,
  labels = list(# list of labels to draw (will use assigned key for label)
    "Andrew" = list(
      x = 27
    ),
    "Brian" = list(
      x = 27
    ),
    "Charles" = list(
      x = 27
    )
  )
) |>
scr_plines_mbd(
  lines = list(# plot linked phase lines (note: drawn from top through bottom)
    "A" = list(
      "Andrew" = list(
        x1 = 4.5,
        y1 = 100
      ),
      "Brian" = list(
        x1 = 11.5,
        y1 = 100
      ),
      "Charles" = list(
        x1 = 18.5,
        y1 = 100,
        y2 = -5
      )
  ),
  "B" = list(
    "Andrew" = list(
      x1 = 13.5,
      y1 = 100
    ),
    "Brian" = list(
      x1 = 20.5,
      y1 = 100
    ),
    "Charles" = list(
      x1 = 23.5,
      y1 = 100,
      y2 = -5
    )
  ),
  "C" = list(
    "Andrew" = list(
      x1 = 23.5,
      y1 = 100
    ),
    "Brian" = list(
      x1 = 23.5,
      y1 = 100
    ),
    "Charles" = list(
      x1 = 23.5,
      y1 = 100,
      y2 = -5
    )
  )
)) |>
scr_xlabel("Session") |> # Override x-axis label (bottom only shown by default)
scr_ylabel("      Percent Accuracy") |> # Override y-axis label (centered, leftmost label)
scr_title("Rates of Acquisition across Participants") |>
  scr_save(name = "../man/figures/multiplebaselinefigure.svg",
           format = "svg",
           units = "in",
           height = 6,
           width = 9)


