
lozy_et_al_single <- LozyEtAl2020
lozy_et_al_single <- lozy_et_al_single[lozy_et_al_single$Participant == "Eli", ]

describe("Cumulative Plot Style", {
  it("Should render as normal", {
    expect_error(
      scr_plot(
        data = lozy_et_al_single,
        aesthetics = var_map(
          x = Session,
          y = KM
        ),
        ncol = 2,
        mai = c(0.3, 0.3, 0.0, 0.1),
        omi = c(0.25, 0.25, 0.1, 0)
      ) |>
        scr_cumsum_lines() |>
        scr_cumsum_points(
          pch = 24,
          fill = "white",
          cex = 1.75
        ) |>
        scr_cumsum_lines(
          mapping = list(
            x = Session,
            y = TD
          )
        ) |>
        scr_cumsum_points(
          pch = 22,
          fill = "white",
          cex = 1.75,
          mapping = list(
            x = Session,
            y = TD
          )
        ) |>
        scr_label_facet(
          cex = 1.5,
          adj = 1,
          x = 15,
          y = 1,
          labels = list(
            "Eli" = list(
              y = 2
            ),
            "Joe" = list(
              y = 2
            )
        )) |>
        scr_label_phase(
          cex = 1.25,
          adj = 0.5,
          x = 6,
          y = 15,
          labels = list(
            "Choice 1"
          )
        ) |>
        scr_xlabel("Choice Training Session") |>
        scr_ylabel("Cumulative Number of Selections") |>
        scr_legend(
          position = "topright",
          legend = c(
            "KM",
            "TD"
          ),
          col = c(
            "black",
            "black"
          ),
          pt_bg = c(
            "black",
            "black"
          ),
          lty = c(
            1,
            1
          ),
          pch = c(
            24,
            22
          ),
          bty = "y",
          pt_cex = 2.25,
          cex = 1.25,
          text_col = "black",
          horiz = FALSE,
          box_lty = 1
        ) |>
        print(),
      "No facet name provided"
    )
  })
})
