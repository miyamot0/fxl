library(fxl)

oldwd <- getwd()

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

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
  scr_lines() |>
  scr_lines(
    lty = 2,
    mapping = list(
      x = Session,
      y = Reinforcers
    )
  ) |>
  scr_points(
    cex = 2.4
  ) |>
  scr_points(
    cex = 2,
    pch = 24,
    color = c("black"),
    fill = c("white"),
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
    lty = c(
      1,
      2
    ),
    pch = c(
      19,
      24
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
    name = "../man/figures/concurrentfigure.svg",
    format = "svg",
    units = "in",
    height = 6,
    width = 9
  ) |>
  scr_save(
    name = "../man/figures/concurrentfigure.png",
    format = "png",
    res = 600,
    height = 6,
    width = 9
  )

setwd(oldwd)
