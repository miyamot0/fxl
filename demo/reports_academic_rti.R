library(fxl)

if ('here' %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

# labelList <- list()
# labelList[[fluencyString]] <- list(
#   x = 130,
#   y = 11.5,
#   color = "orange"
# )
# labelList2 <- list()
# labelList2[[accuracyString]] <- list(
#  x = 130,
#  y = .115,
#  color = "red"
# )

hypotheticalReadingFluency <- data.frame(
  Time   = c( 1, 30, 60, 90,  120, 150, 180,  210, 240),
  BM_ORF = c(73, NA, NA, NA,  105, NA,  NA,   NA,  114),
  ORF    = c(60, 64, 68, 72,  74,  NA,  NA,   NA,  NA),
  BM_WRF = c(40, NA, NA, NA,  50,  NA,  NA,   NA,  55),
  WRF    = c(32, 33, 35, 36,  38,  NA,  NA,   NA,  NA),
  BM_MAZ = c(8,  NA, NA, NA,  12,  NA,  NA,   NA,  15.5),
  MAZ    = c(6,  6,  7,  8,   8,   NA,  NA,   NA,  NA)
)


scr_plot(
  hypotheticalReadingFluency,
  aesthetics = var_map(
    x = Time,
    y = ORF
  ),
  omi = c(
    0.5,
    0.35,
    0.5,
    0.25
  ),
  mai = c(
    0.0,
    0.25,
    0.0,
    0.25
  ),
  semilog = TRUE
) |>
  scr_yoverride(c(1, 1000)) |>
  scr_xoverride(c(1, 243),
                xticks = c(1, 30, 60, 90, 120, 150, 180, 210, 240),
                xtickslabs = c(
                  'September',
                  'October',
                  'November',
                  'December',
                  'January',
                  'February',
                  'March',
                  'April',
                  'May'
                )) |>
  scr_title("Response-to-Intervention: Core Reading Indicators") |>
  scr_xlabel("Days Into Academic School Year") |>
  scr_ylabel("Fluency") |>
  scr_guide_line(
    lty = 1,
    coords = list(
      "ORF" = list(
        x0 = 120,
        x1 = 240,
        y0 = 74,
        y1 = 114,
        col = "green",
        lty = 2,
        lwd = 2.5
      ),
      "WRF" = list(
        x0 = 120,
        x1 = 240,
        y0 = 38,
        y1 = 55,
        col = "orange",
        lty = 2,
        lwd = 2.5
      ),
      "MAX" = list(
        x0 = 120,
        x1 = 240,
        y0 = 8,
        y1 = 15.5,
        col = "blue",
        lty = 2,
        lwd = 2.5
      )
    )
  ) |>
  scr_points(
    pch = 23,
    fill = 'green',
    cex = 3,
    mapping = var_map(
      x = Time,
      y = BM_ORF
    )
  ) |>
  scr_points(
    pch = 23,
    fill = 'orange',
    cex = 3,
    mapping = var_map(
      x = Time,
      y = BM_WRF
    )
  ) |>
  scr_points(
    pch = 23,
    fill = 'blue',
    cex = 3,
    mapping = var_map(
      x = Time,
      y = BM_MAZ
    )
  ) |>
  scr_lines(size = 1) |>
  scr_points(
    pch = 21,
    fill = 'green',
    cex = 3
  ) |>
  scr_lines(color = 'black',
            mapping = var_map(
              x = Time,
              y = WRF
            )) |>
  scr_points(
    pch = 21,
    fill = 'orange',
    cex = 3,
    mapping = var_map(
      x = Time,
      y = WRF
    )
  ) |>
  scr_lines(color = 'black',
            mapping = var_map(
              x = Time,
              y = MAZ
            )) |>
  scr_points(
    pch = 21,
    fill = 'blue',
    cex = 3,
    mapping = var_map(
      x = Time,
      y = MAZ
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "ORF Intervention Started" = list(
        color = "green",
        x = 122.5,
        y = 135
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "WRF Intervention Started" = list(
        color = "orange",
        x = 122.5,
        y = 27.5
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "MAZ Intervention Started" = list(
        color = "blue",
        x = 122.5,
        y = 6
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0,
    labels = list(
      "Note (1/15/23): Started Tier II Intervention" = list(
        x = 122.5,
        y = 1.5
      )
    )
  ) |>
  scr_legend(
    position = list(
      x = 0.75,
      y = 1250
    ), # Specify legend location
    legend = c(
      "Median Expectation: Oral Reading Fluency (ORF) - Words Correct",
      "Oral Reading Fluency (ORF) - Words Correct",
      "Median Expectation: Word Reading Fluency (WRF)",
      "Word Reading Fluency (WRF)",
      "Median Expectation: Maze (MAZ)",
      "Maze (MAZ)"
    ),
    col = c("black", "black", "black", "black", "black", "black"),
    pt_bg = c("green", "green",
              "orange", "orange",
              "blue", "blue"),
    bg = "white",
    lty = c(
      NA,
      1,
      NA,
      1,
      NA,
      1
    ),
    pch = c(
      23,
      21,
      23,
      21,
      23,
      21
    ),
    bty = "y",
    pt_cex = 2,
    cex = 1.25,
    text_col = "black",
    horiz = FALSE,
    box_lty = 1
  ) |>
  scr_save(name = "../man/figures/celeration_academic_rti.svg",
           format = "svg",
           units = "in",
           height = 7,
           width = 9)
