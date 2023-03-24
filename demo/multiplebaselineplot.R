library(fxl)

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

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
  scr_title("Rates of Acquisition across Participants") |>
  scr_save(
    name = "../man/figures/multiplebaselinefigure.svg",
    format = "svg",
    units = "in",
    height = 6,
    width = 9
  ) |>
  scr_save(
    name = "../man/figures/multiplebaselinefigure.png",
    format = "png",
    res = 600,
    height = 6,
    width = 9
  )
