library(fxl)

oldwd <- getwd()

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

lightestGray <- "#CCCCCC"
mediumGray <- "#999999"
darkGray <- "#333333"

scr_plot(consumptionData,
  aesthetics = var_map(
    x = Session,
    y = Baseline,
    p = Condition,
    facet = Participant
  ),
  mai = c(0.375, 0.375, 0.1, 0.1),
  omi = c(0.25, 0.25, 0.25, 0.25)
) |>
  scr_xoverride(
    c(0.4, 40),
    xticks = c(1, 5, 10, 15, 20, 25, 30, 35, 40)
  ) |>
  scr_yoverride(
    list(
      "P1" = list(
        y0 = -0.75,
        y1 = 10,
        yticks = c(0, 5, 10)
      ),
      "P2" = list(
        y0 = -0.75,
        y1 = 10,
        yticks = c(0, 5, 10)
      ),
      "P3" = list(
        y0 = -1.5,
        y1 = 30,
        yticks = c(0, 10, 20, 30)
      )
    ),
    ydelta = 5
  ) |>
  scr_lines() |>
  scr_lines(mapping = list(
    x = Session,
    y = Control
  )) |>
  scr_lines(mapping = list(
    x = Session,
    y = FR1
  )) |>
  scr_lines(mapping = list(
    x = Session,
    y = FR2
  )) |>
  scr_lines(mapping = list(
    x = Session,
    y = VR2
  )) |>
  scr_points(
    cex = 2,
    pch = 16
  ) |>
  scr_points(
    cex = 2,
    pch = 16,
    mapping = list(
      x = Session,
      y = Control
    )
  ) |>
  scr_points(
    cex = 2,
    pch = 24,
    fill = lightestGray,
    mapping = list(
      x = Session,
      y = FR1
    )
  ) |>
  scr_points(
    cex = 2,
    pch = 23,
    fill = mediumGray,
    mapping = list(
      x = Session,
      y = FR2
    )
  ) |>
  scr_points(
    cex = 2,
    pch = 22,
    fill = darkGray,
    mapping = list(
      x = Session,
      y = VR2
    )
  ) |>
  scr_label_phase(
    facet = "P1",
    cex = 1.25,
    adj = 0.5,
    y = 10.75,
    labels = list(
      "Baseline"          = list(x = 2.4),
      "ATD"               = list(x = 14),
      "Concurrent Chains" = list(x = 29)
    )
  ) |>
  scr_label_facet(
    cex = 1.5,
    adj = 1,
    x = 35,
    labels = list(
      "P1" = list(y = 2),
      "P2" = list(y = 2),
      "P3" = list(y = 2)
    )
  ) |>
  scr_plines_mbd(lines = list(
    "A" = list(
      "P1" = list(x1 = 4.5, y1 = 10),
      "P2" = list(x1 = 4.5, y1 = 10),
      "P3" = list(x1 = 9.5, y1 = 25, y2 = -1.5)
    ),
    "B" = list(
      "P1" = list(x1 = 24.5, y1 = 10),
      "P2" = list(x1 = 24.5, y1 = 10),
      "P3" = list(x1 = 33.5, y1 = 25, y2 = -1.5)
    )
  )) |>
  scr_xlabel("Session") |>
  scr_ylabel("          Work Output") |>
  scr_legend(
    panel = "P2",
    position = "topright",
    legend = c("Baseline", "FR1", "FR2", "VR2"),
    col = c("black", "black", "black", "black"),
    pt_bg = c("black", lightestGray, mediumGray, darkGray),
    lty = c(1, 1, 1, 1),
    pch = c(16, 24, 23, 22),
    bg = c("black", "black", "black", "black"),
    bty = "n",
    pt_cex = 2.25,
    cex = 1.25,
    text_col = "black",
    horiz = F,
    box_lty = 0
  )

# |>
#   scr_save(name = "Figure-ATD with CO.png",
#            format = "png",
#            units = "in",
#            height = 6,
#            width = 9)

setwd(oldwd)
