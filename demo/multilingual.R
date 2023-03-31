library(fxl)

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

# Grey out errorless phase

current_data <- read.csv("mult_ling_data.csv")
current_data_gen <- current_data

current_data <- current_data[current_data$Generalization == 'N',]
current_data <- current_data[current_data$Phase != 'Errorless',]
current_data <- current_data[current_data$Phase != 'DiscriminationTest',]

current_data_gen <- current_data_gen[current_data_gen$Generalization == 'Y',]
current_data_gen <- current_data_gen[current_data_gen$Phase != 'DiscriminationTest',]

x_ticks <- c(1, (1:12)*5)

scr_plot(current_data,
    aesthetics = var_map(
      x = Session,
      y = Pct.Eng,
      p = Phase,
      facet = Facet
    ),
    mai = c(0.0625, 0.25,  0.25, 0.25),
    omi = c(0.5,    0.25,  0.25, 0.25)
  ) |>
  scr_yoverride(
    c(-.1, 1),
    ydelta = 0.25,
    yticks = c(0, 0.25, .5, .75, 1),
    ytickslabs = as.character(c(0, 0.25, .5, .75, 1))
  ) |>
  scr_xoverride(
    c(-0.5, max(x_ticks)),
    xticks = x_ticks,
    xtickslabs = as.character(x_ticks)
  ) |>
  scr_rect(
    facet = "1",
    x1 = 7.5,
    x2 = 10,
    y1 = -0.09,
    y2 = 1,
    fill = "gray",
    color = "gray"
  ) |>
  scr_rect(
    facet = "2",
    x1 = 22.5,
    x2 = 25,
    y1 = -0.09,
    y2 = 1,
    fill = "gray",
    color = "gray"
  ) |>
  scr_rect(
    facet = "3",
    x1 = 40.5,
    x2 = 43,
    y1 = -0.09,
    y2 = 1,
    fill = "gray",
    color = "gray"
  ) |>
  scr_ylabel("Accuracy") |>
  scr_xlabel("Session",
             line = 2) |>
  scr_lines() |>
  scr_points(
    pch = 23,
    cex = 2,
    fill = 'white'
  ) |>
  scr_points(
    pch = 23,
    cex = 2,
    fill = 'green',
    data = current_data_gen
  ) |>
  scr_lines(
    mapping = list(
      x = Session,
      y = Pct.Spanish
    )
  ) |>
  scr_points(
    pch = 22,
    cex = 2,
    fill = 'white',
    mapping = list(
      x = Session,
      y = Pct.Spanish
    )
  ) |>
  scr_points(
    pch = 22,
    cex = 2,
    fill = 'green',
    mapping = list(
      x = Session,
      y = Pct.Spanish
    ),
    data = current_data_gen
  ) |>
  scr_plines_mbd(
    lines = list(
      "A" = list(
        "1" = list(
          x1 = 7.5,
          y1 = 1,
          y2 = -0.1
        ),
        "2" = list(
          x1 = 22.5,
          y1 = 1,
          y2 = -0.1
        ),
        "3" = list(
          x1 = 40.5,
          y1 = 1,
          y2 = -0.1
        )
      )
    )
  ) |>
  scr_label_facet(
    cex = 1.5,
    adj = 1,
    y = 1,
    face = 2,
    x = 60,
    labels = list(
      "1" = list(
        label = "Bubble Gun"
      ),
      "2" = list(
        label = "Slinky"
      ),
      "3" = list(
        label = "Light-up Top"
      )
    )
  ) |>
  scr_label_phase(
    facet = "1",
    cex = 1,
    adj = 0.5,
    y = 1,
    labels = list(
      "Baseline" = list(
        x = 3.5
      ),
      "Intervention" = list(
        x = 19
      ),
      "Errorless Training" = list(
        x = 18,
        y = 0.35
      )
    )
  ) |>
  scr_arrows(
    facet = "1",
    length = 0.075,
    code = 1,
    color = 'black',
    lty = 1,
    lwd = 1,
    arrows = list(
      "A" = list(
        x0 = 10.5,
        y0 = 0.35,
        x1 = 13,
        y1 = 0.35
      )
    )
  ) |>
  scr_legend(
    panel = "1",
    position = "right", # Specify legend location
    legend = c(
      "English",
      "Spanish",
      "English (G)",
      "Spanish (G)"
    ),
    col = c(
      "black",
      "black",
      "black",
      "black"
    ),
    pt_bg = c(
      "white",
      "white",
      "green",
      "green"
    ),
    lty = c(
      1,
      1,
      1,
      1
    ),
    pch = c(
      23,
      22,
      23,
      22
    ),
    bty = "n",
    pt_cex = 2.25,
    cex = 1.25,
    text_col = "black",
    horiz = FALSE,
    box_lty = 0
  )
