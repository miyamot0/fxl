library(fxl)
library(tidyverse)

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

read_data_wide <- Challenge1Data %>%
  spread(OutcomeName, Y) %>%
  rename(Teacher = `Percentage of Treatment Steps Implemented by Teacher`,
         Student = `Percentage of Intervals of Off-Task Behavior by Student`) %>%
  select(CaseName, X, CondName, Student, Teacher)

colnames(read_data_wide)


scr_plot(read_data_wide,
         aesthetics = var_map(
           x = X,
           y = Teacher,
           p = CondName ,
           facet = CaseName
         ),
         mai = c(0.375, 0.375, 0.0625, .25),
         omi = c(0.25,  0.25,  0.5, 0.25)
) |>
  scr_yoverride(c(-5, 100),
                yticks = 0:10*10
) |>
  scr_ylabel("Percentage") |>
  scr_xlabel("Session Number") |>
  scr_xoverride(
    c(0, 45),
    xticks = c(
      1,
      seq(5, 45,
          by = 5
      )
    )
  ) |>
  scr_lines(
    size = 1
  ) |>
  scr_points(
    pch = 21,
    cex = 2.25
  ) |>
  scr_lines(
    size = 1,
    mapping = var_map(
      x = X,
      y = Student
    )
  ) |>
  scr_points(
    pch = 21,
    cex = 2.25,
    fill = 'white',
    mapping = var_map(
      x = X,
      y = Student
    )
  ) |>
  scr_label_facet(
    cex = 1.25,
    adj = 1,
    y = 3.15,
    face = 2,
    x = 45.25,
    labels = list(
      "Dyad A" = list(
        y = 5
      ),
      "Dyad B",
      "Dyad C",
      "Dyad D" = list(
        y = 120
      )
    )
  ) |>
  scr_label_phase(
    facet = "Dyad A",
    cex = 1.125,
    adj = 0.5,
    face = 2,
    y = 115,
    labels = list(
      "Pre-Training\nBaseline" = list(
        x = 1.75
      ),
      "Training" = list(
        x = 6
      ),
      "Implementation\nBaseline" = list(
        x = 13.5
      ),
      "PF + SR-" = list(
        x = 23
      ),
      "Dynamic Fading" = list(
        x = 37.5
      )
    )
  ) |>
  scr_label_phase(
    facet = "Dyad B",
    cex = 1,
    adj = 0.5,
    face = 1,
    y = 0,
    labels = list(
      "Medication" = list(
        x = 17
      )
    )
  ) |>
  scr_label_phase(
    facet = "Dyad D",
    cex = 1,
    adj = 0,
    face = 1,
    y = 10,
    labels = list(
      "Adjusted Criterion" = list(
        x = 33
      ),
      "Medication" = list(
        x = 28,
        y = 100
      )
    )
  ) |>
  scr_anno_arrows(
    facet = "Dyad B",
    length = 0.1,
    arrows = list(
      "A" = list(
        x0 = 17,
        x1 = 17,
        y0 = 3,
        y1 = 13
      )
    )
  ) |>
  scr_anno_arrows(
    facet = "Dyad D",
    length = 0.1,
    arrows = list(
      "A" = list(
        x0 = 33.5,
        x1 = 33.5,
        y0 = 20,
        y1 = 50
      ),
      "B" = list(
        x0 = 29,
        x1 = 29,
        y0 = 90,
        y1 = 35
      )
    )
  ) |>
  scr_plines_mbd(
    lines = list(
      "A" = list(
        "Dyad A" = list(
          x1 = 3.5,
          y1 = 100
        ),
        "Dyad B" = list(
          x1 = 3.5,
          y1 = 100
        ),
        "Dyad C" = list(
          x1 = 3.5,
          y1 = 100
        ),
        "Dyad D" = list(
          x1 = 3.5,
          y1 = 100,
          y2 = -5
        )
      ),
      "B" = list(
        "Dyad A" = list(
          x1 = 10.5,
          y1 = 100
        ),
        "Dyad B" = list(
          x1 = 9.5,
          y1 = 100
        ),
        "Dyad C" = list(
          x1 = 14.5,
          y1 = 100
        ),
        "Dyad D" = list(
          x1 = 17.5,
          y1 = 100,
          y2 = -5
        )
      ),
      "C" = list(
        "Dyad A" = list(
          x1 = 16.5,
          y1 = 100
        ),
        "Dyad B" = list(
          x1 = 20,
          y1 = 100
        ),
        "Dyad C" = list(
          x1 = 23.5,
          y1 = 100,
          y2 = -20
        ),
        "Dyad D" = list(
          x1 = 32.5,
          y1 = 100,
          y2 = -5
        )
      ),
      "D" = list(
        "Dyad A" = list(
          x1 = 31.5,
          y1 = 100
        ),
        "Dyad B" = list(
          x1 = 27.5,
          y1 = 100
        ),
        "Dyad C" = list(
          x1 = 32.5,
          y1 = 100
        ),
        "Dyad D" = list(
          x1 = 41.5,
          y1 = 100,
          y2 = -5
        )
      )
    )
  ) |>
  scr_anno_guide_line(
    facet = "Dyad D",
    lwd = 1,
    lty = 2,
    coords = list(
      "Dyad D" = list(
        x0 = 32.5,
        x1 = 44,
        y0 = 60,
        y1 = 60
      )
    )
  ) |>
  scr_legend(
    panel = "Dyad A",
    position = "right",
    legend = c(
      "Teacher",
      "Student"
    ),
    col = c(
      "black",
      "black"
    ),
    lty = c(
      1,
      1
    ),
    pch = c(
      21,
      21
    ),
    bg = c(
      "black",
      "black"
    ),
    pt_bg = c(
      "black",
      "white"
    ),
    bty = "n",
    pt_cex = 2.25,
    cex = 1.25,
    text_col = "black",
    horiz = FALSE,
    box_lty = 0
  ) |>
  scr_save(
    name = "../man/figures/challenge_1.png",
    format = "png",
    res = 300,
    height = 8,
    width = 9
  )
