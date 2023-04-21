library(fxl)
library(tidyverse)

oldwd <- getwd()

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

read_data <- Challenge2Data

scr_plot(read_data,
         aesthetics = var_map(
           x = Session,
           y = IWS,
           p = Condition,
           facet = Participant
         ),
         omi = c(
           0.5,
           0.3,
           0.5,
           0.125
         ),
         mai = c(
           0.0,
           0.375,
           0,
           0
         ),
         semilog = TRUE,
         semi_color_major_y = "transparent",
         semi_color_midpoint_y = "transparent",
         semi_color_minor_y = "transparent",
         semi_color_major_x = "transparent",
         bty = "l"
) |>
  scr_yoverride(c(0.75, 125),
                yticks = c(1, 5, 10, 50, 100),
                ytickslabs = as.character(c(1, NULL, 10, NULL, 100))) |>
  scr_xoverride(c(1, 73),
                xticks = seq(0, 11) * 7,
                xtickslabs = as.character(seq(0, 11) * 7)) |>
  scr_xlabel("Successive Calendar Days") |>
  scr_ylabel("Correct and Incorrect Word Sequences") |>
  scr_title("Figure Recreation: Datchuk & Kubina (2017)",
            cex = 1.25,
            face = 2) |>
  scr_anno_guide_line(
    facet = "Rachel",
    lwd = 1,
    lty = 1,
    coords = list(
      "A" = list(
        x0 = 1,
        x1 = 7,
        y0 = 25,
        y1 = 24
      ),
      "B" = list(
        x0 = 8,
        x1 = 22,
        y0 = 25,
        y1 = 29
      ),
      "C" = list(
        x0 = 1,
        x1 = 7,
        y0 = 5,
        y1 = 7
      ),
      "D" = list(
        x0 = 8,
        x1 = 22,
        y0 = 5,
        y1 = 1
      )
    )
  ) |>
  scr_anno_guide_line(
    facet = "Kim",
    lwd = 1,
    lty = 1,
    coords = list(
      "A" = list(
        x0 = 1,
        x1 = 11,
        y0 = 25,
        y1 = 22
      ),
      "B" = list(
        x0 = 1,
        x1 = 11,
        y0 = 2,
        y1 = 7
      ),
      "C" = list(
        x0 = 13,
        x1 = 20,
        y0 = 22,
        y1 = 30
      ),
      "D" = list(
        x0 = 13,
        x1 = 20,
        y0 = 8,
        y1 = 1
      )
    )
  ) |>
  scr_anno_guide_line(
    facet = "Bettie",
    lwd = 1,
    lty = 1,
    coords = list(
      "A" = list(
        x0 = 1,
        x1 = 13,
        y0 = 25,
        y1 = 22
      ),
      "B" = list(
        x0 = 1,
        x1 = 13,
        y0 = 1,
        y1 = 4
      ),
      "C" = list(
        x0 = 14,
        x1 = 37,
        y0 = 22,
        y1 = 30
      ),
      "D" = list(
        x0 = 14,
        x1 = 37,
        y0 = 3,
        y1 = 4
      )
    )
  ) |>
  scr_anno_guide_line(
    facet = "Orin",
    lwd = 1,
    lty = 1,
    coords = list(
      "A" = list(
        x0 = 8,
        x1 = 17,
        y0 = 15,
        y1 = 12
      ),
      "B" = list(
        x0 = 8,
        x1 = 17,
        y0 = 3,
        y1 = 7
      ),
      "C" = list(
        x0 = 24,
        x1 = 44,
        y0 = 7,
        y1 = 30
      ),
      "D" = list(
        x0 = 24,
        x1 = 44,
        y0 = 8,
        y1 = 0.5
      )
    )
  ) |>
  scr_points(
    pch = 25,
    fill = 'white',
    cex = 2.5
  ) |>
  scr_points(
    pch = 21,
    fill = 'black',
    cex = 2.5,
    mapping = var_map(
      x = Session,
      y = CWS,
    )
  )  |>
  scr_label_facet(
    cex = 1.5,
    adj = 1,
    face = 2,
    y = 90,
    x = 76,
    labels = list(
      "Rachel" = list(),
      "Kim" = list(),
      "Bettie" = list(),
      "Orin" = list()
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0.5,
    y = 95,
    facet = "Rachel",
    labels = list(
      "Baseline" = list(
        x = 3.5
      ),
      "SI and FBPC" = list(
        x = 15
      ),
      "PI" = list(
        x = 26.5
      ),
      "Maintenance" = list(
        x = 33,
        adj = 0
      )
    )
  ) |>
  scr_plines_mbd(
    lines = list( # plot linked phase lines (note: drawn from top through bottom)
      "A" = list(
        "Rachel" = list(
          x1 = 7.5,
          y1 = 100,
          y2 = 0.1
        ),
        "Kim" = list(
          x1 = 12,
          y1 = 80,
          y2 = 0.1
        ),
        "Bettie" = list(
          x1 = 13.5,
          y1 = 80,
          y2 = 0.1
        ),
        "Orin" = list(
          x1 = 22,
          y1 = 80,
          y2 = 0.1
        )
      )
    )
  ) |>
  scr_plines(
    lty = 1,
    lines = list(
      "Rachel" = list(
        "A" = list(
          x1 = 22.5,
          y1 = 100,
          y2 = 0.1
        ),
        "B" = list(
          x1 = 30.5,
          y1 = 100,
          y2 = 0.1
        )
      ),
      "Kim" = list(
        "A" = list(
          x1 = 20.5,
          y1 = 100,
          y2 = 0.1
        ),
        "B" = list(
          x1 = 39.5,
          y1 = 100,
          y2 = 0.1
        )
      ),
      "Bettie" = list(
        "A" = list(
          x1 = 37.5,
          y1 = 100,
          y2 = 0.1
        ),
        "B" = list(
          x1 = 40.5,
          y1 = 100,
          y2 = 0.1
        )
      ),
      "Orin" = list(
        "A" = list(
          x1 = 44.5,
          y1 = 100,
          y2 = 0.1
        ),
        "B" = list(
          x1 = 47.5,
          y1 = 100,
          y2 = 0.1
        )
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 1,
    x = 63.5,
    facet = "Rachel",
    labels = list(
      "Correct Word Sequences" = list(
        y = 30
      ),
      "Incorrect Word Sequences" = list(
        y = 5
      )
    )
  ) |>
  scr_anno_arrows(
    facet = "Rachel",
    length = 0.1,
    arrows = list(
      "A" = list(
        x0 = 64,
        x1 = 69,
        y0 = 30,
        y1 = 30
      ),
      "B" = list(
        x0 = 64,
        x1 = 70,
        y0 = 5,
        y1 = 5
      )
    )
  ) |>
  scr_save(
    name = "../man/figures/challenge_2.png",
    format = "png",
    res = 300,
    height = 8,
    width = 11
  )

setwd(oldwd)
