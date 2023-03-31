current_data <- Gilroyetal2019Tx
current_data$Condition <- paste0(current_data$Condition, current_data$PhaseNum)
current_data$Function <- current_data$Participant
current_data$AFCR <- current_data$FCR
current_data$EFCR <- current_data$FCR2

describe("Annotated Plot", {
  it("Should render as normal", {
    expect_no_error(
      scr_plot(current_data,
        aesthetics = var_map(
          x = Session,
          y = CTB,
          p = Condition,
          facet = Function
        ),
        mai = c(0.375, 0.375, 0.1, 0),
        omi = c(0.25, 0.25, 0.25, 0.05)
      ) |>
        scr_yoverride(
          c(0, 3)
        ) |>
        scr_xoverride(
          c(0, 100),
          xdelta = 10
        ) |>
        scr_lines(
          size = 1
        ) |>
        scr_lines(
          mapping = list(
            x = Session,
            y = AFCR
          ),
          size = 1,
          lty = 2
        ) |>
        scr_lines(
          mapping = list(
            x = Session,
            y = EFCR
          ),
          size = 1,
          lty = 3
        ) |>
        scr_points(
          fill = "white",
          pch = 21
        ) |>
        scr_points(
          mapping = list(
            x = Session,
            y = AFCR
          ),
          cex = 1,
          pch = 20,
          fill = "black"
        ) |>
        scr_points(
          mapping = list(
            x = Session,
            y = EFCR
          ),
          cex = 0.75,
          pch = 24,
          fill = "black"
        ) |>
        scr_plines_mbd(
          lines = list(
            "A" = list(
              "Attention" = list(
                x1 = 13.5,
                y1 = 3.15
              ),
              "Demand" = list(
                x1 = 20,
                y1 = 3
              )
            )
          )
        ) |>
        scr_plines(
          lty = 1,
          lines = list(
            "Attention" = list(
              "A" = list(
                x1 = 25.5,
                y1 = 3
              ),
              "B" = list(
                x1 = 26.5,
                y1 = 3
              ),
              "C" = list(
                x1 = 60.5,
                y1 = 3,
                lty = 3
              ),
              "D" = list(
                x1 = 76.5,
                y1 = 3,
                lty = 3
              )
            ),
            "Demand" = list(
              "A" = list(
                x1 = 34.5,
                y1 = 3
              ),
              "B" = list(
                x1 = 37.5,
                y1 = 3
              ),
              "C" = list(
                x1 = 41.5,
                y1 = 3
              ),
              "D" = list(
                x1 = 50.5,
                y1 = 3,
                lty = 3
              ),
              "E" = list(
                x1 = 72.5,
                y1 = 3,
                lty = 3
              )
            )
          )
        ) |>
        scr_label_facet(
          cex = 1.25,
          adj = 1,
          y = 3.15,
          x = 100,
          labels = list(
            "Attention",
            "Demand"
          )
        ) |>
        scr_label_phase(
          facet = "Attention",
          cex = 0.6,
          adj = 0.5,
          y = 3,
          labels = list(
            "Baseline" = list(
              x = 14,
              y = 3.5
            ),
            "FCR-A + EXT" = list(
              x = 19
            ),
            "FCR-A + EXT" = list(
              x = 32
            ),
            "Parent Implementation" = list(
              x = 68.5
            ),
            "Generalization" = list(
              x = 82
            ),
            "Problem Behavior" = list(
              x = 7,
              y = 1.8
            ),
            "FCR-A" = list(
              x = 20,
              y = 2.5
            ),
            "Add FCR\nOptions" = list(
              x = 31,
              y = 2.5
            )
          )
        ) |>
        scr_label_phase(
          facet = "Attention",
          cex = 0.6,
          adj = 0.5,
          labels = list(
            "5s" = list(
              x = 39,
              y = 2.4
            ),
            "Schedule Thinning" = list(
              x = 54,
              y = 2.4
            ),
            "300s" = list(
              x = 75,
              y = 2.4
            )
          )
        ) |>
        scr_label_phase(
          facet = "Demand",
          cex = 0.6,
          adj = 0.5,
          y = 3,
          labels = list(
            "FCR-E + EXT" = list(
              x = 30,
              y = 3.45
            ),
            "FCR-A P = 0.1" = list(
              x = 36,
              y = 2,
              srt = 90
            ),
            "FCR-A/E + EXT" = list(
              x = 47,
              y = 3.35
            ),
            "Parent Implementation" = list(
              x = 58.5
            ),
            "Generalization" = list(
              x = 78
            ),
            "FCR-E" = list(
              x = 24,
              y = 2.5
            ),
            "FCR-A\nP = 0.1\n200% SR" = list(
              x = 46,
              y = 2
            )
          )
        ) |>
        scr_label_phase(
          facet = "Demand",
          cex = 0.6,
          adj = 0.5,
          y = 1.375,
          labels = list(
            "1" = list(
              x = 30
            ),
            "Demand Fading" = list(
              x = 56
            ),
            "6" = list(
              x = 71.5
            )
          )
        ) |>
        scr_arrows(
          facet = "Attention",
          length = 0.1,
          arrows = list(
            "A" = list(
              x0 = 7,
              x1 = 7,
              y0 = 1.5,
              y1 = 1
            ),
            "B" = list(
              x0 = 20,
              x1 = 20,
              y0 = 2.25,
              y1 = 2
            ),
            "C" = list(
              x0 = 31,
              x1 = 31,
              y0 = 2.25,
              y1 = 2
            )
          )
        ) |>
        scr_arrows(
          facet = "Demand",
          length = 0.1,
          arrows = list(
            "A" = list(
              x0 = 24,
              x1 = 24,
              y0 = 2.25,
              y1 = 1.5
            ),
            "B" = list(
              x0 = 36,
              x1 = 36,
              y0 = 1.3,
              y1 = 0.75
            ),
            "C" = list(
              x0 = 46,
              x1 = 46,
              y0 = 1.5,
              y1 = 0.75
            )
          )
        ) |>
        scr_brackets(
          facet = "Attention",
          length = 0.1,
          brackets = list(
            "A" = list(
              x0 = 8,
              x1 = 26,
              y0 = 3.3,
              y1 = 3
            ),
            "B" = list(
              x0 = 38,
              x1 = 76,
              y0 = 2.25,
              y1 = 1.5,
              lty = 3
            )
          )
        ) |>
        scr_brackets(
          facet = "Demand",
          length = 0.1,
          brackets = list(
            "A" = list(
              x0 = 23,
              x1 = 40,
              y0 = 3.3,
              y1 = 3
            ),
            "B" = list(
              x0 = 36,
              x1 = 47,
              y0 = 3.2,
              y1 = 2.9
            ),
            "C" = list(
              x0 = 29,
              x1 = 72,
              y0 = 1.25,
              y1 = 0.5,
              lty = 3
            )
          )
        ) |>
        scr_guide_line(
          color = "red",
          lty = 3,
          facet = "Attention",
          coords = list(
            "A" = list(
              x0 = 14,
              x1 = 25.5,
              y0 = 0.1
            ),
            "B" = list(
              x0 = 26.5,
              x1 = 100,
              y0 = 0.1
            )
          )
        ) |>
        scr_guide_line(
          color = "red",
          lty = 3,
          facet = "Demand",
          coords = list(
            "A" = list(
              x0 = 20,
              x1 = 100,
              y0 = 0.1
            )
          )
        ) |>
        print()
    )
  })
})
