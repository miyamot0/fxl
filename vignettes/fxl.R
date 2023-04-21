## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(dpi = 75, fig.width = 8, fig.height = 4, dev = 'png')

## ----structure_core, warning=FALSE--------------------------------------------
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

## ----structure_layers---------------------------------------------------------
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

## ----structure_labels---------------------------------------------------------
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

## ----ex_gilroy_et_al_2021, echo=FALSE-----------------------------------------
data <- Gilroyetal2021

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
    0.175,
    0.1
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
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
    )
  ) |>
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
    legend = c(
      "Responses Observed",
      "Reinforcers Produced"
    ),
    col = c(
      "black",
      "black"
    ),
    pt_bg = c(
      "black",
      "black"
    ),
    lty = c(
      1,
      2
    ),
    pch = c(
      19,
      2
    ),
    bg = c(
      "black",
      "black"
    ),
    bty = "n",
    pt_cex = 2.25,
    cex = 1.25,
    text_col = "black",
    horiz = FALSE,
    box_lty = 0
  )

## ----ex_gilroy_et_al_at, echo=FALSE-------------------------------------------
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
    ytickslabs = c(
      "0",
      "0.5",
      "1",
      "1.5",
      "2"
    )
  ) |> # manually override y-axis
  scr_lines(size = 1) |> # plot lines, using x/y from aesthetics
  scr_points(
    cex = 2, # plot points, using x/y from aesthetics
    pch = list( # override point marker types (match FA conventions)
      "Toy Play" = 16,
      "Attention" = 22,
      "Demand" = 24,
      "Tangible" = 8
    ),
    fill = list( # override point marker colors (match FA conventions)
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
    box_lty = 0
  )

## ----ex_gilroy_et_al_mbd, echo=FALSE------------------------------------------
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
    )
  ) |>
  scr_xoverride(
    c(-1, 100),
    xdelta = 10,
    xticks = c(
      1,
      seq(10, 100,
        by = 10
      )
    )
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
          y2 = -0.125
        )
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
  scr_anno_arrows(
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
  scr_anno_arrows(
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
  scr_anno_brackets(
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
  scr_anno_brackets(
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
  scr_anno_guide_line(
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
  scr_anno_guide_line(
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

## ----ex_gilroy_et_al_2015, echo=FALSE-----------------------------------------
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
    labels = list( # list of labels to draw (will use assigned key for label)
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
    labels = list( # list of labels to draw (will use assigned key for label)
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
    lines = list( # plot linked phase lines (note: drawn from top through bottom)
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
    )
  ) |>
  scr_xlabel("Session") |> # Override x-axis label (bottom only shown by default)
  scr_ylabel("      Percent Accuracy") |> # Override y-axis label (centered, leftmost label)
  scr_title("Rates of Acquisition across Participants")

## ----academics_grouped, echo=FALSE--------------------------------------------
needFluency <- SimulatedAcademicFluency[SimulatedAcademicFluency$Times > 160 &
  SimulatedAcademicFluency$pred < 40 &
  SimulatedAcademicFluency$pred > 10, "index"]

needAccuracy <- SimulatedAcademicFluency[SimulatedAcademicFluency$Times > 160 &
  SimulatedAcademicFluency$pred < 10, "index"]

fluencyString <- paste("Consider Fluency Intervention:", paste(needFluency, collapse = ", "))
accuracyString <- paste("Consider Accuracy Intervention:", paste(needAccuracy, collapse = ", "))

labelList <- list()
labelList[[fluencyString]] <- list(
  x = 130,
  y = 11.5,
  color = "orange"
)
labelList2 <- list()
labelList2[[accuracyString]] <- list(
  x = 130,
  y = .115,
  color = "red"
)

scr_plot(
  SimulatedAcademicFluency,
  aesthetics = var_map(
    x = Times,
    y = pred,
    p = index
  ),
  omi = c(
    0.0,
    0.35,
    0.2,
    0.25
  ),
  mai = c(
    0.0,
    0.25,
    0.1,
    0.25
  ),
  semilog = TRUE
) |>
  scr_yoverride(c(0.1, 100)) |>
  scr_xoverride(c(1, 183),
    xticks = c(1, 30, 60, 90, 120, 150, 180),
    xtickslabs = c(
      "September",
      "October",
      "November",
      "December",
      "January",
      "February",
      "March"
    )
  ) |>
  scr_title("Addition Sums to 12: Monthly Class-wide Screening") |>
  scr_xlabel("Days Into Academic School Year") |>
  scr_ylabel("Digits Correct Per Minute (DCPM)") |>
  scr_anno_guide_line(
    color = "green",
    lty = 1,
    lwd = 2,
    coords = list(
      "A" = list(
        x0 = -3,
        x1 = 190,
        y0 = 41,
        y1 = 41
      ),
      "B" = list(
        x0 = -3,
        x1 = 190,
        y0 = 100,
        y1 = 100
      ),
      "C" = list(
        x0 = -3,
        x1 = -3,
        y0 = 41,
        y1 = 100
      ),
      "D" = list(
        x0 = 190,
        x1 = 190,
        y0 = 41,
        y1 = 100
      )
    )
  ) |>
  scr_anno_guide_line(
    color = "orange",
    lty = 1,
    lwd = 2,
    coords = list(
      "A" = list(
        x0 = -3,
        x1 = 190,
        y0 = 10,
        y1 = 10
      ),
      "B" = list(
        x0 = -3,
        x1 = 190,
        y0 = 39,
        y1 = 39
      ),
      "C" = list(
        x0 = -3,
        x1 = -3,
        y0 = 39,
        y1 = 10
      ),
      "D" = list(
        x0 = 190,
        x1 = 190,
        y0 = 39,
        y1 = 10
      )
    )
  ) |>
  scr_anno_guide_line(
    color = "red",
    lty = 1,
    lwd = 2,
    coords = list(
      "A" = list(
        x0 = -3,
        x1 = 190,
        y0 = .1,
        y1 = .1
      ),
      "B" = list(
        x0 = -3,
        x1 = 190,
        y0 = 9.5,
        y1 = 9.5
      ),
      "C" = list(
        x0 = -3,
        x1 = -3,
        y0 = 9.5,
        y1 = .1
      ),
      "D" = list(
        x0 = 190,
        x1 = 190,
        y0 = 9.5,
        y1 = .1
      )
    )
  ) |>
  scr_lines() |>
  scr_points(
    pch = 21,
    fill = "white",
    cex = 1.5
  ) |>
  scr_label_phase(
    cex = 1,
    adj = 0,
    labels = labelList
  ) |>
  scr_label_phase(
    cex = 1,
    adj = 0,
    labels = labelList2
  )

## ----academics_individual, echo=FALSE-----------------------------------------
hypotheticalReadingFluency <- data.frame(
  Time   = c(1, 30, 60, 90, 120, 150, 180, 210, 240),
  BM_ORF = c(73, NA, NA, NA, 105, NA, NA, NA, 114),
  ORF    = c(60, 64, 68, 72, 74, NA, NA, NA, NA),
  BM_WRF = c(40, NA, NA, NA, 50, NA, NA, NA, 55),
  WRF    = c(32, 33, 35, 36, 38, NA, NA, NA, NA),
  BM_MAZ = c(8, NA, NA, NA, 12, NA, NA, NA, 15.5),
  MAZ    = c(6, 6, 7, 8, 8, NA, NA, NA, NA)
)


scr_plot(
  hypotheticalReadingFluency,
  aesthetics = var_map(
    x = Time,
    y = ORF
  ),
  omi = c(
    0.0,
    0.35,
    0.2,
    0.25
  ),
  mai = c(
    0.0,
    0.25,
    0.1,
    0.25
  ),
  semilog = TRUE
) |>
  scr_yoverride(c(1, 1000)) |>
  scr_xoverride(c(1, 243),
    xticks = c(1, 30, 60, 90, 120, 150, 180, 210, 240),
    xtickslabs = c(
      "September",
      "October",
      "November",
      "December",
      "January",
      "February",
      "March",
      "April",
      "May"
    )
  ) |>
  scr_title("Response-to-Intervention: Core Reading Indicators") |>
  scr_xlabel("Days Into Academic School Year") |>
  scr_ylabel("Fluency") |>
  scr_anno_guide_line(
    lty = 1,
    coords = list(
      "ORF" = list(
        x0 = 120,
        x1 = 240,
        y0 = 74,
        y1 = 114,
        col = "green",
        lty = 2,
        lwd = 2.5
      ),
      "WRF" = list(
        x0 = 120,
        x1 = 240,
        y0 = 38,
        y1 = 55,
        col = "orange",
        lty = 2,
        lwd = 2.5
      ),
      "MAX" = list(
        x0 = 120,
        x1 = 240,
        y0 = 8,
        y1 = 15.5,
        col = "blue",
        lty = 2,
        lwd = 2.5
      )
    )
  ) |>
  scr_points(
    pch = 23,
    fill = "green",
    cex = 3,
    mapping = var_map(
      x = Time,
      y = BM_ORF
    )
  ) |>
  scr_points(
    pch = 23,
    fill = "orange",
    cex = 3,
    mapping = var_map(
      x = Time,
      y = BM_WRF
    )
  ) |>
  scr_points(
    pch = 23,
    fill = "blue",
    cex = 3,
    mapping = var_map(
      x = Time,
      y = BM_MAZ
    )
  ) |>
  scr_lines(size = 1) |>
  scr_points(
    pch = 21,
    fill = "green",
    cex = 3
  ) |>
  scr_lines(
    color = "black",
    mapping = var_map(
      x = Time,
      y = WRF
    )
  ) |>
  scr_points(
    pch = 21,
    fill = "orange",
    cex = 3,
    mapping = var_map(
      x = Time,
      y = WRF
    )
  ) |>
  scr_lines(
    color = "black",
    mapping = var_map(
      x = Time,
      y = MAZ
    )
  ) |>
  scr_points(
    pch = 21,
    fill = "blue",
    cex = 3,
    mapping = var_map(
      x = Time,
      y = MAZ
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "ORF Intervention Started" = list(
        color = "green",
        x = 122.5,
        y = 135
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "WRF Intervention Started" = list(
        color = "orange",
        x = 122.5,
        y = 27.5
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "MAZ Intervention Started" = list(
        color = "blue",
        x = 122.5,
        y = 6
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "Note (1/15/23): Started Tier II Intervention" = list(
        x = 122.5,
        y = 1.5
      )
    )
  ) |>
  scr_legend(
    position = "topright", # Specify legend location
    legend = c(
      "Median Expectation: Oral Reading Fluency (ORF) - Words Correct",
      "Oral Reading Fluency (ORF) - Words Correct",
      "Median Expectation: Word Reading Fluency (WRF)",
      "Word Reading Fluency (WRF)",
      "Median Expectation: Maze (MAZ)",
      "Maze (MAZ)"
    ),
    col = c("black", "black", "black", "black", "black", "black"),
    pt_bg = c(
      "green", "green",
      "orange", "orange",
      "blue", "blue"
    ),
    bg = "white",
    lty = c(
      NA,
      1,
      NA,
      1,
      NA,
      1
    ),
    pch = c(
      23,
      21,
      23,
      21,
      23,
      21
    ),
    bty = "y",
    pt_cex = 2.25,
    cex = 1.5,
    text_col = "black",
    horiz = FALSE,
    box_lty = 1
  )

## ----use_case_1_load----------------------------------------------------------
use_case_1 <- data.frame(
  Session = seq_len(16),
  Target = c(
    runif(3, 10, 20),
    runif(3, 0, 10),
    runif(3, 10, 20),
    runif(7, 2, 8)
  ),
  Phase = c(
    rep("Baseline", 3),
    rep("Intervention", 3),
    rep("Baseline2", 3),
    rep("Intervention2", 7)
  ),
  Participant = rep("1", 16)
)

head(use_case_1, 3)

## ----use_case_1_map-----------------------------------------------------------
scr_plot(
  use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  )
)

## ----use_case_1_series--------------------------------------------------------
scr_plot(use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  )
) |>
  scr_lines() |>
  scr_points()

## ----use_case_1_series_2------------------------------------------------------
scr_plot(use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  ),
  mai = c(
    0.375,
    0.5,
    0.175,
    0.1
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
) |>
  scr_xoverride(
    c(0.5, 16.5),
    xticks = 1:16
  ) |>
  scr_yoverride(
    c(-0.5, 20),
    yticks = c(0, 5, 10, 15, 20),
    ydelta = 5
  ) |>
  scr_lines() |>
  scr_points()

## ----use_case_1_series_3------------------------------------------------------
scr_plot(use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  ),
  mai = c(
    0.375,
    0.5,
    0.175,
    0.1
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
) |>
  scr_xoverride(
    c(0.5, 16.5),
    xticks = 1:16
  ) |>
  scr_yoverride(
    c(-0.5, 20),
    yticks = c(0, 5, 10, 15, 20),
    ydelta = 5
  ) |>
  scr_lines() |>
  scr_points() |>
  scr_plines(
    lty = 1,
    lines = list(
      "A" = list(
        x1 = 3.5,
        y1 = 20,
        y2 = -0.5
      ),
      "B" = list(
        x1 = 6.5,
        y1 = 20,
        y2 = -0.5
      ),
      "C" = list(
        x1 = 9.5,
        y1 = 20,
        y2 = -0.5
      )
    )
  )

## ----use_case_1_series_4------------------------------------------------------
scr_plot(use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  ),
  mai = c(
    0.375,
    0.5,
    0.175,
    0.1
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
) |>
  scr_xoverride(
    c(0.5, 16.5),
    xticks = 1:16
  ) |>
  scr_yoverride(
    c(-0.5, 20),
    yticks = c(0, 5, 10, 15, 20),
    ydelta = 5
  ) |>
  scr_lines() |>
  scr_points() |>
  scr_plines(
    lty = 1,
    lines = list(
      "A" = list(
        x1 = 3.5,
        y1 = 20,
        y2 = -0.5
      ),
      "B" = list(
        x1 = 6.5,
        y1 = 20,
        y2 = -0.5
      ),
      "C" = list(
        x1 = 9.5,
        y1 = 20,
        y2 = -0.5
      )
    )
  ) |>
  scr_label_phase(
    cex = 1,
    face = 2,
    adj = 0.5,
    y = 20,
    labels = list(
      "A" = list(
        x = 2,
        label = "Baseline"
      ),
      "B" = list(
        x = 5,
        label = "Intervention"
      ),
      "C" = list(
        x = 8,
        label = "Baseline"
      ),
      "D" = list(
        x = 13,
        label = "Intervention"
      )
    )
  )

## ----use_case_1_series_5------------------------------------------------------
scr_plot(use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  ),
  mai = c(
    0.375,
    0.5,
    0.175,
    0.1
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
) |>
  scr_xoverride(
    c(0.5, 16.5),
    xticks = 1:16
  ) |>
  scr_yoverride(
    c(-0.5, 20),
    yticks = c(0, 5, 10, 15, 20),
    ydelta = 5
  ) |>
  scr_lines() |>
  scr_points() |>
  scr_plines(
    lty = 1,
    lines = list(
      "A" = list(
        x1 = 3.5,
        y1 = 20,
        y2 = -0.5
      ),
      "B" = list(
        x1 = 6.5,
        y1 = 20,
        y2 = -0.5
      ),
      "C" = list(
        x1 = 9.5,
        y1 = 20,
        y2 = -0.5
      )
    )
  ) |>
  scr_label_phase(
    adj = 0.5,
    y = 20,
    labels = list(
      "A" = list(
        x = 2,
        label = "Baseline"
      ),
      "B" = list(
        x = 5,
        label = "Intervention"
      ),
      "C" = list(
        x = 8,
        label = "Baseline"
      ),
      "D" = list(
        x = 13,
        label = "Intervention"
      )
    )
  ) |>
  scr_xlabel("Daily Sessions") |>
  scr_ylabel("Rates of Target Behavior (per Minute)") |>
  scr_title("Use Case #1",
    face = 2
  )

## ----use_case_1_series_6------------------------------------------------------
scr_plot(use_case_1,
  aesthetics = var_map(
    x = Session,
    y = Target,
    p = Phase
  ),
  mai = c(
    0.375,
    0.5,
    0.175,
    0.1
  ),
  omi = c(
    0.25,
    0.25,
    0.25,
    0.25
  )
) |>
  scr_xoverride(
    c(0.5, 16.5),
    xticks = 1:16
  ) |>
  scr_yoverride(
    c(-0.5, 20),
    yticks = c(0, 5, 10, 15, 20),
    ydelta = 5
  ) |>
  scr_lines() |>
  scr_points() |>
  scr_plines(
    lty = 1,
    lines = list(
      "A" = list(
        x1 = 3.5,
        y1 = 20,
        y2 = -0.5
      ),
      "B" = list(
        x1 = 6.5,
        y1 = 20,
        y2 = -0.5
      ),
      "C" = list(
        x1 = 9.5,
        y1 = 20,
        y2 = -0.5
      )
    )
  ) |>
  scr_label_phase(
    adj = 0.5,
    y = 20,
    labels = list(
      "A" = list(
        x = 2,
        label = "Baseline"
      ),
      "B" = list(
        x = 5,
        label = "Intervention"
      ),
      "C" = list(
        x = 8,
        label = "Baseline"
      ),
      "D" = list(
        x = 13,
        label = "Intervention"
      )
    )
  ) |>
  scr_xlabel("Daily Sessions") |>
  scr_ylabel("Rates of Target Behavior (per Minute)") |>
  scr_title("Use Case #1",
    face = 2
  ) # |>
#   scr_save(
#     name = "use_case_1.png",
#     units = "in",
#     width = 9,
#     height = 6,
#     format = "png"
#   )

## ----use_case_2_load----------------------------------------------------------
use_case_2 <- Gilroyetal2015

head(use_case_2, 3)

## ----use_case_2_map-----------------------------------------------------------
scr_plot(
  use_case_2,
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
  )

## ----use_case_2_series--------------------------------------------------------
scr_plot(
  use_case_2,
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
  scr_lines() |>
  scr_points(cex = 2)

## ----use_case_2_series_3------------------------------------------------------
scr_plot(
  use_case_2,
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
  scr_lines() |>
  scr_points(cex = 2) |>
  scr_plines_mbd(
    lines = list( # plot linked phase lines (note: drawn from top through bottom)
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
    )
  )

## ----use_case_2_series_4------------------------------------------------------
scr_plot(
  use_case_2,
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
  scr_lines() |>
  scr_points(cex = 2) |>
  scr_plines_mbd(
    lines = list( # plot linked phase lines (note: drawn from top through bottom)
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
    )
  ) |>
  scr_label_facet(
    cex = 1.5, # plot labels across facets (not within a single facet)
    adj = 1,
    y = 10,
    labels = list( # list of labels to draw (will use assigned key for label)
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
  scr_label_phase(
    facet = "Andrew", # plot labels on specific facet
    cex = 1.25,
    adj = 0.5,
    y = 107,
    labels = list( # list of labels to draw (will use assigned key for label)
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
  )

## ----use_case_2_series_5------------------------------------------------------
scr_plot(
  use_case_2,
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
  scr_lines() |>
  scr_points(cex = 2) |>
  scr_plines_mbd(
    lines = list( # plot linked phase lines (note: drawn from top through bottom)
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
    )
  ) |> # plot lines, using x/y from aesthetics
  scr_label_phase(
    facet = "Andrew", # plot labels on specific facet
    cex = 1.25,
    adj = 0.5,
    y = 107,
    labels = list( # list of labels to draw (will use assigned key for label)
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
  scr_xlabel("Session") |> # Override x-axis label (bottom only shown by default)
  scr_ylabel("      Percent Accuracy") |> # Override y-axis label (centered, leftmost label)
  scr_title("Rates of Acquisition across Participants",
    face = 2
  )

## ----use_case_2_series_6------------------------------------------------------
scr_plot(
  use_case_2,
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
  scr_lines() |>
  scr_points(cex = 2) |>
  scr_plines_mbd(
    lines = list( # plot linked phase lines (note: drawn from top through bottom)
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
    )
  ) |> # plot lines, using x/y from aesthetics
  scr_label_phase(
    facet = "Andrew", # plot labels on specific facet
    cex = 1.25,
    adj = 0.5,
    y = 107,
    labels = list( # list of labels to draw (will use assigned key for label)
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
  scr_xlabel("Session") |> # Override x-axis label (bottom only shown by default)
  scr_ylabel("      Percent Accuracy") |> # Override y-axis label (centered, leftmost label)
  scr_title("Rates of Acquisition across Participants",
    face = 2
  ) # |>
#   scr_save(
#     name = "use_case_2.png",
#     units = "in",
#     width = 9,
#     height = 6,
#     format = "png"
#   )

