library(fxl)

if ('here' %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

scr_plot(
  data = LozyEtAl2020,
  aesthetics = var_map(
    x = Session,
    y = KM,
    p = Phase,
    facet = Participant
  ),
  ncol = 2,
  mai = c(0.3, 0.3, 0.1, 0.1),
  omi = c(0.25, 0.25, 0.1, 0.1)
) |>
scr_xoverride(
  c(0, 30),
  xdraws = c(
    "Eva",
    "Cali",
    "Ari"
  ),
  xticks = list(
    "Eli" = c(1, 5, 10, 15),
    "Ari" = c(1, 10, 20, 30),
    "Al" = c(1, 5, 10, 15),
    "Ry" = c(1, 5, 10, 15),
    "Eva" = c(1, 5, 10, 15),
    "Cali" = c(1, 5, 10, 15)
  )
) |>
scr_yoverride(
  ydraws = c(
    "Eli",
    "Al",
    "Eva"
  ),
  list(
    "Eli" = list(
      y0 = -1,
      y1 = 15,
      yticks = c(0, 5, 10, 15)
    ),
    "Ari" = list(
      y0 = -1,
      y1 = 15,
      yticks = c(0, 5, 10, 15)
    ),
    "Al" = list(
      y0 = -0.5,
      y1 = 8,
      yticks = c(0, 2, 4, 6, 8)
    ),
    "Ry" = list(
      y0 = -0.5,
      y1 = 8,
      yticks = c(0, 2, 4, 6, 8)
    ),
    "Eva" = list(
      y0 = -0.5,
      y1 = 8,
      yticks = c(0, 2, 4, 6, 8)
    ),
    "Cali" = list(
      y0 = -0.5,
      y1 = 8,
      yticks = c(0, 2, 4, 6, 8)
    )
  )
) |>
scr_cumsum_lines() |>
scr_cumsum_points(
  pch = 24,
  fill = "white",
  cex = 1.75
) |>
scr_cumsum_lines(
  mapping = list(
    x = Session,
    y = TD
  )
) |>
scr_cumsum_points(
  pch = 22,
  fill = "white",
  cex = 1.75,
  mapping = list(
    x = Session,
    y = TD
  )
) |>
scr_label_facet(
  cex = 1.5,
  adj = 1,
  x = 15,
  y = 1,
  labels = list(
    "Eli" = list(
      y = 2
    ),
    "Ari" = list(
      x = 30,
      y = 2
    ),
    "Al",
    "Cali",
    "Ry",
    "Eva"
  )
) |>
scr_plines(
  lty = 3,
  lines = list(
    "Ari" = list(
      "A" = list(
        x1 = 11.5,
        y1 = 15
      )
    ),
    "Al" = list(
      "A" = list(
        x1 = 4.5,
        y1 = 8
      )
    ),
    "Cali" = list(
      "A" = list(
        x1 = 8.5,
        y1 = 8
      )
    ),
    "Ry" = list(
      "A" = list(
        x1 = 9.5,
        y1 = 8
      )
    ),
    "Eva" = list(
      "A" = list(
        x1 = 4.5,
        y1 = 8
      ),
      "B" = list(
        x1 = 8.5,
        y1 = 8
      )
    )
  )
) |>
scr_label_phase(
  facet = "Eli",
  cex = 1.25,
  adj = 0.5,
  x = 6,
  y = 15,
  labels = list(
    "Choice 1"
    )
) |>
scr_label_phase(
  facet = "Ari",
  cex = 1.25,
  adj = 0.5,
  y = 15,
  labels = list(
    "Choice 1" = list(
      x = 5
    ),
    "Choice 2" = list(
      x = 20
    )
  )
) |>
scr_label_phase(
  facet = "Al",
  cex = 1.25,
  adj = 0.5,
  y = 8,
  labels = list(
    "Choice 1" = list(
      x = 2.25
    ),
    "Choice 2" = list(
      x = 11
    )
  )
) |>
scr_label_phase(
  facet = "Ry",
  cex = 1.25,
  adj = 0.5,
  y = 8,
  labels = list(
    "Choice 1" = list(
      x = 5
    ),
    "Choice 2" = list(
      x = 13
    )
  )
) |>
scr_label_phase(
  facet = "Eva",
  cex = 1.25,
  adj = 0.5,
  y = 8,
  labels = list(
    "Choice 1" = list(
      x = 2.25
    ),
    "Choice 2" = list(
      x = 6.5
    ),
    "Choice 3" = list(
      x = 11
    )
  )
) |>
scr_label_phase(
  facet = "Cali",
  cex = 1.25,
  adj = 0.5,
  y = 8,
  labels = list(
    "Choice 1" = list(
      x = 4
    ),
    "Choice 2" = list(
      x = 11
    )
  )
) |>
scr_xlabel("Choice Training Session") |>
scr_ylabel("       Cumulative Number of Selections") |>
scr_legend(
  position = "right",
  panel = "Eli",
  legend = c(
    "KM",
    "TD"),
  col = c(
    "black",
    "black"
  ),
  pt_bg = c(
    "white",
    "white"
  ),
  lty = c(
    1,
    1
  ),
  pch = c(
    24,
    22
  ),
  bty = "y",
  pt_cex = 2.25,
  cex = 1.25,
  text_col = "black",
  horiz = FALSE,
  box_lty = 1
) |>
scr_save(units = "in",
         name = "../man/figures/cumulativeplot.svg",
         format = "svg",
         width = 9,
         height = 6)
