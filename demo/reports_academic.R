library(fxl)

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

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
  ) |>
  scr_save(
    name = "../man/figures/celeration_academic.svg",
    format = "svg",
    units = "in",
    height = 6,
    width = 9
  ) |>
  scr_save(
    name = "../man/figures/celeration_academic.png",
    format = "png",
    res = 600,
    height = 6,
    width = 9
  )
