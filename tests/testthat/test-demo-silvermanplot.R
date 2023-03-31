koffarnus_et_al_mod <- KoffarnusEtAl2011
koffarnus_et_al_mod$facet <- ifelse(koffarnus_et_al_mod$ID < 40,
  "1",
  ifelse(koffarnus_et_al_mod$ID > 83,
    "3",
    "2"
  )
)

describe("Silverman Plot Style", {
  it("Should render as normal", {
    expect_no_error(
      scr_plot(
        koffarnus_et_al_mod,
        aesthetics = var_map(
          x = X,
          y = ID,
          p = Code,
          facet = facet
        ),
        mai = c(
          0.0,
          0.0,
          0.35,
          0.0
        ),
        omi = c(
          0.725,
          0.725,
          0.0,
          0.0
        )
      ) |>
        scr_yoverride(
          list( # manually override y-axis and tick interval
            "1" = list(
              y0 = 0,
              y1 = 40,
              yticks = c(5, 10, 15, 20, 25, 30, 35)
            ),
            "2" = list(
              y0 = 40,
              y1 = 83,
              yticks = c(40, 45, 50, 55, 60, 65, 70, 75, 80)
            ),
            "3" = list(
              y0 = 82,
              y1 = 127,
              yticks = c(85, 90, 95, 100, 105, 110, 115, 120, 125)
            )
          ),
          ydelta = 5
        ) |>
        scr_ylabel("Participant Number", line = 3) |>
        scr_xoverride(
          var = c(1, 135),
          xdelta = 5,
          xticks = c(
            0, 10, 20, 30, 40, 50,
            60, 70, 80, 90, 100, 110,
            120, 130
          )
        ) |>
        scr_xlabel("Consecutive Work Day", line = 3) |>
        scr_points(
          cex = list(
            "0" = 1,
            "1" = 1,
            "2" = 0.5
          ),
          pch = list(
            "0" = 22,
            "1" = 22,
            "2" = 22
          ),
          fill = list( # override point marker colors (match FA conventions)
            "0" = "black",
            "1" = "white",
            "2" = rgb(.8,
              .8,
              .8,
              alpha = 0.75
            )
          ),
          color = list( # override point marker colors (match FA conventions)
            "0" = "transparent",
            "1" = "black",
            "2" = "transparent"
          )
        ) |>
        scr_label_facet(
          cex = 1.25,
          adj = 0,
          x = 1,
          labels = list( # list of labels to draw (will use assigned key for label)
            "1" = list(
              y = 43.5,
              label = "Unpaid Training (n = 39)"
            ),
            "2" = list(
              y = 86.5,
              label = "Paid Training (n = 42)"
            ),
            "3" = list(
              y = 130.5,
              label = "Contingent Paid Training (n = 42)"
            )
          )
        ) |>
        scr_legend(
          panel = "3",
          position = list(
            x = 60,
            y = 136
          ),
          adj = c(0, 0.5),
          legend = c(
            "Present: Positive",
            "Present: Negative",
            "Absent"
          ),
          col = c(
            "black",
            "black",
            "transparent"
          ),
          pt_bg = c(
            "white",
            "black",
            rgb(.8,
              .8,
              .8,
              alpha = 0.75
            )
          ),
          lty = NULL,
          pch = c(
            22,
            22,
            22
          ),
          bty = "n",
          cex = 1.5,
          pt_cex = c(1, 1, 0.5),
          horiz = TRUE,
          box_lty = 0
        ) |>
        print()
    )
  })
})
