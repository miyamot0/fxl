library(dplyr)
library(fxl)
library(scales)

oldwd <- getwd()

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

read_data <- Challenge4Data %>%
  mutate(Participant =  factor(Participant,
                          levels = c("Hamza", "Ivory", "Brynley", "Logan"))) %>%
  arrange(Participant)

read_data_h_f1 <- read_data %>%
  filter(Participant == "Hamza") %>%
  mutate(Facet = "Hamza-1",
         Primary = Dot.Number,
         Secondary = Dot.Number_Accuracy)

read_data_h_f2 <- read_data %>%
  filter(Participant == "Hamza") %>%
  mutate(Facet = "Hamza-2",
         Primary = Dot.Number.Total,
         Secondary = Dot.Number.Total_Accuracy)

read_data_h_f3 <- read_data %>%
  filter(Participant == "Hamza")  %>%
  mutate(Facet = "Hamza-3",
         Primary = Number.Total,
         Secondary = Number.Total_Accuracy)

read_data_i_f1 <- read_data %>%
  filter(Participant == "Ivory") %>%
  mutate(Facet = "Ivory-1",
         Primary = Dot.Number,
         Secondary = Dot.Number_Accuracy)

read_data_i_f2 <- read_data %>%
  filter(Participant == "Ivory") %>%
  mutate(Facet = "Ivory-2",
         Primary = Dot.Number.Total,
         Secondary = Dot.Number.Total_Accuracy)

read_data_i_f3 <- read_data %>%
  filter(Participant == "Ivory")  %>%
  mutate(Facet = "Ivory-3",
         Primary = Number.Total,
         Secondary = Number.Total_Accuracy)

read_data_b_f1 <- read_data %>%
  filter(Participant == "Brynley") %>%
  mutate(Facet = "Brynley-1",
         Primary = Dot.Number,
         Secondary = Dot.Number_Accuracy)

read_data_b_f2 <- read_data %>%
  filter(Participant == "Brynley") %>%
  mutate(Facet = "Brynley-2",
         Primary = Dot.Number.Total,
         Secondary = Dot.Number.Total_Accuracy)

read_data_b_f3 <- read_data %>%
  filter(Participant == "Brynley")  %>%
  mutate(Facet = "Brynley-3",
         Primary = Number.Total,
         Secondary = Number.Total_Accuracy)

read_data_l_f1 <- read_data %>%
  filter(Participant == "Logan") %>%
  mutate(Facet = "Logan-1",
         Primary = Dot.Number,
         Secondary = Dot.Number_Accuracy)

read_data_l_f2 <- read_data %>%
  filter(Participant == "Logan") %>%
  mutate(Facet = "Logan-2",
         Primary = Dot.Number.Total,
         Secondary = Dot.Number.Total_Accuracy)

read_data_l_f3 <- read_data %>%
  filter(Participant == "Logan")  %>%
  mutate(Facet = "Logan-3",
         Primary = Number.Total,
         Secondary = Number.Total_Accuracy)

hack_comb <- rbind(
  read_data_h_f1,
  read_data_h_f2,
  read_data_h_f3,
  read_data_i_f1,
  read_data_i_f2,
  read_data_i_f3,
  read_data_b_f1,
  read_data_b_f2,
  read_data_b_f3,
  read_data_l_f1,
  read_data_l_f2,
  read_data_l_f3
)

bar_styler <- function(data_frame, ...) {
  input_list <- list(...)

  local_frame <- input_list[["plot_frame"]]

  local_frame <- local_frame[!is.na(local_frame$mod_y),]

  if (nrow(local_frame) < 1) return(0)

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
  hack_comb,
  aesthetics = var_map(
    x = Session,
    y = Primary,
    p = Phase,
    facet = Facet
  ),
  mai = c(
    0.125, #b, l, t, r
    0.5,
    0,
    0.5
  ),
  omi = c(
    0.35,
    0.25,
    0.25,
    0.25
  )
) |>
scr_yoverride(c(-3, 52),
              yticks = seq(0, 50, by = 10),
              ytickslabs = as.character(seq(0, 50, by = 10))
) |>
scr_ylabel("Correct Digits Per Minute") |>
scr_xoverride(c(0, 61),
              xticks = c(1, (1:12) * 5),
              xtickslabs = as.character(c(1, (1:12) * 5))
) |>
scr_xlabel("Session",
           line = 1) |>
scr_bar_support(
  color = rgb(.8, .8, .8, alpha = 1),
  guide_line = 80,
  guide_line_color = "transparent",
  guide_line_type = 2,
  guide_line_size = 1,
  styler = bar_styler,
  label = "Percentage Correct",
  mapping = list(
    x = Session,
    y = Secondary
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
  y = 15,
  x = 60,
  labels = list(
    "Brynley-1" = list(
      label = "Brynley\nDot Number"
    ),
    "Brynley-2" = list(
      label = "Brynley\nDot Number Total"
    ),
    "Brynley-3" = list(
      label = "Brynley\nNumber Total"
    ),
    "Hamza-1" = list(
      label = "Hamza\nDot Number"
    ),
    "Hamza-2" = list(
      label = "Hamza\nDot Number Total"
    ),
    "Hamza-3" = list(
      label = "Hamza\nNumber Total"
    ),
    "Ivory-1" = list(
      label = "Ivory\nDot Number"
    ),
    "Ivory-2" = list(
      label = "Ivory\nDot Number Total"
    ),
    "Ivory-3" = list(
      label = "Ivory\nNumber Total"
    ),
    "Logan-1" = list(
      label = "Logan\nDot Number"
    ),
    "Logan-2" = list(
      label = "Logan\nDot Number Total"
    ),
    "Logan-3" = list(
      label = "Logan\nNumber Total"
    )
  )
) |>
scr_plines_mbd(
  lines = list( # plot linked phase lines (note: drawn from top through bottom)
    "A" = list(
      "Hamza-1" = list(
        x1 = 5.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-2" = list(
        x1 = 5.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-3" = list(
        x1 = 5.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-1" = list(
        x1 = 7.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-2" = list(
        x1 = 7.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-3" = list(
        x1 = 7.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-1" = list(
        x1 = 11.5,
        y1 = 45,
        y2 = -1
      ),
      "Brynley-2" = list(
        x1 = 11.5,
        y1 = 45,
        y2 = -1
      ),
      "Brynley-3" = list(
        x1 = 11.5,
        y1 = 45,
        y2 = -1
      ),
      "Logan-1" = list(
        x1 = 12.5,
        y1 = 50,
        y2 = -3
      ),
      "Logan-2" = list(
        x1 = 12.5,
        y1 = 50,
        y2 = -3
      ),
      "Logan-3" = list(
        x1 = 12.5,
        y1 = 50,
        y2 = -3
      )
    ),
    "B" = list(
      "Hamza-1" = list(
        x1 = 14.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-2" = list(
        x1 = 14.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-3" = list(
        x1 = 14.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-1" = list(
        x1 = 10.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-2" = list(
        x1 = 10.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-3" = list(
        x1 = 10.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-1" = list(
        x1 = 16.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-2" = list(
        x1 = 16.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-3" = list(
        x1 = 16.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan-1" = list(
        x1 = 26.5,
        y1 = 38,
        y2 = -3
      ),
      "Logan-2" = list(
        x1 = 26.5,
        y1 = 38,
        y2 = -3
      ),
      "Logan-3" = list(
        x1 = 26.5,
        y1 = 38,
        y2 = -3
      )
    ),
    "C" = list(
      "Hamza-1" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-2" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-3" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-1" = list(
        x1 = 21.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-2" = list(
        x1 = 21.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-3" = list(
        x1 = 21.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-1" = list(
        x1 = 23.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-2" = list(
        x1 = 23.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-3" = list(
        x1 = 23.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan-1" = list(
        x1 = 29.5,
        y1 = 42,
        y2 = -5
      ),
      "Logan-2" = list(
        x1 = 29.5,
        y1 = 42,
        y2 = -5
      ),
      "Logan-3" = list(
        x1 = 29.5,
        y1 = 42,
        y2 = -5
      )
    ),
    "D" = list(
      "Hamza-1" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-2" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-3" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-1" = list(
        x1 = 31.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-2" = list(
        x1 = 31.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-3" = list(
        x1 = 31.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-1" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-2" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-3" = list(
        x1 = 27.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan-1" = list(
        x1 = 39.5,
        y1 = 46,
        y2 = -3
      ),
      "Logan-2" = list(
        x1 = 39.5,
        y1 = 46,
        y2 = -3
      ),
      "Logan-3" = list(
        x1 = 39.5,
        y1 = 46,
        y2 = -3
      )
    ),
    "E" = list(
      "Hamza-1" = list(
        x1 = 44.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-2" = list(
        x1 = 44.5,
        y1 = 50,
        y2 = -1
      ),
      "Hamza-3" = list(
        x1 = 44.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-1" = list(
        x1 = 41.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-2" = list(
        x1 = 41.5,
        y1 = 50,
        y2 = -1
      ),
      "Ivory-3" = list(
        x1 = 41.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-1" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-2" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Brynley-3" = list(
        x1 = 37.5,
        y1 = 50,
        y2 = -1
      ),
      "Logan-1" = list(
        x1 = 49.5,
        y1 = 50,
        y2 = -3
      ),
      "Logan-2" = list(
        x1 = 49.5,
        y1 = 50,
        y2 = -3
      ),
      "Logan-3" = list(
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
  facet = "Hamza-1",
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
    "Dot Number Set C" = list(
      x = 41
    ),
    "Number Total" = list(
      x = 49
    )
  )
) |>
scr_save(
  name = "../man/figures/challenge_4.png",
  format = "png",
  res = 300,
  height = 11,
  width = 9.5
)

setwd(oldwd)
