library(dplyr)
library(fxl)
library(scales)

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

read_data <- read.csv("../data/MathCombined.csv") %>%
  mutate(Participant =  factor(Participant,
                          levels = c("Hamza", "Ivory", "Brynley", "Logan"))) %>%
  arrange(Participant)

bar_styler <- function(data_frame, ...) {
  input_list <- list(...)

  local_frame <- input_list[["plot_frame"]]

  trans <- 0.5

  col_orange <- alpha("red", trans)
  col_light_grn <- alpha("orange", trans)
  col_green <- alpha("green", trans)

  local_frame$col <- col_orange

  local_frame[local_frame$pct >= .95, "col"] <- col_green
  local_frame[local_frame$pct < .95 & local_frame$pct >= .80, "col"] <- col_light_grn
  local_frame[local_frame$pct < .80, "col"] <- col_orange

  rect(local_frame$X - 0.25,
       0,
       local_frame$X + 0.25,
       local_frame$mod_y,
       border = local_frame$col,
       col = local_frame$col
  )
}

scr_plot(
  read_data,
  aesthetics = var_map(
    x = Session,
    y = Dot.Number,
    p = Phase,
    facet = Participant
  ),
  mai = c(
    0.375,
    0.5,
    0.0,
    0.5
  ),
  omi = c(
    0.375,
    0.25,
    0.375,
    0.25
  )
) |>
scr_yoverride(c(-3, 52),
              yticks = seq(0, 50, by = 10),
              ytickslabs = as.character(seq(0, 50, by = 10))
) |>
scr_ylabel("Correct Digits Per Minute") |>
scr_xoverride(c(0, 56),
              xticks = c(1, (1:11) * 5),
              xtickslabs = as.character(c(1, (1:11) * 5))
) |>
scr_xlabel("Session",
           line = 1) |>
scr_bar_support(
  color = rgb(.8, .8, .8, alpha = 1),
  guide_line = 80,
  guide_line_color = "transparent",
  guide_line_type = 2,
  guide_line_size = 1,
  #width = 1,
  styler = bar_styler,
  label = "Percentage Correct",
  mapping = list(
    x = Session,
    y = Dot.Number_Accuracy
  )
) |>
scr_points(
  cex = 1.25
) |>
scr_lines() |>
scr_label_facet(
  cex = 1.25,
  adj = 1,
  face = 2,
  y = 30,
  x = 55,
  labels = list(
    "Brynley" = list(
      y = 50
    ),
    "Hamza" = list(),
    "Ivory" = list(
      y = 50
    ),
    "Logan" = list(
      y = 50
    )
  )
) |>
scr_plines_mbd(
  lines = list( # plot linked phase lines (note: drawn from top through bottom)
    "A" = list(
      "Hamza" = list(
        x1 = 5.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory" = list(
        x1 = 7.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley" = list(
        x1 = 11.5,
        y1 = 45,
        y2 = -1
      ),
      "Logan" = list(
        x1 = 12.5,
        y1 = 50,
        y2 = -3
      )
    ),
    "B" = list(
      "Hamza" = list(
        x1 = 14.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory" = list(
        x1 = 10.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley" = list(
        x1 = 16.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan" = list(
        x1 = 26.5,
        y1 = 38,
        y2 = -3
      )
    ),
    "C" = list(
      "Hamza" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory" = list(
        x1 = 21.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley" = list(
        x1 = 23.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan" = list(
        x1 = 29.5,
        y1 = 42,
        y2 = -5
      )
    ),
    "D" = list(
      "Hamza" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory" = list(
        x1 = 31.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan" = list(
        x1 = 39.5,
        y1 = 46,
        y2 = -3
      )
    ),
    "E" = list(
      "Hamza" = list(
        x1 = 44.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory" = list(
        x1 = 41.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan" = list(
        x1 = 49.5,
        y1 = 50,
        y2 = -3
      )
    )
  )
) |>
scr_label_phase(
  cex = 1,
  adj = 0.5,
  y = 65,
  facet = "Hamza",
  labels = list(
    "Baseline" = list(
      x = 2.5
    ),
    "Dot Number" = list(
      x = 10
    ),
    "Dot Number Set A" = list(
      x = 21
    ),
    "Dot Number Set B" = list(
      x = 32.5
    ),
    "Dot Number\nSet C" = list(
      x = 41,
      y = 61
    ),
    "Number Total" = list(
      x = 49
    )
  )
) |>
scr_save(
  name = "../man/figures/challenge_4.png",
  format = "png",
  res = 600,
  height = 6,
  width = 8
)
