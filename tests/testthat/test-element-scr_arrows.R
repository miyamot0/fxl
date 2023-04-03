current_data <- Gilroyetal2019Tx
current_data$Condition <- paste0(current_data$Condition, current_data$PhaseNum)
current_data$Function <- current_data$Participant
current_data$AFCR <- current_data$FCR
current_data$EFCR <- current_data$FCR2

describe("scr_arrows", {
  it("Should fail at length", {
    expect_error(
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
        scr_lines(
          size = 1
        ) |>
        scr_arrows(
          facet = "Attention",
          length = NULL,
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
        print(),
      "length must not be NULL"
    )
  })

  it("Should fail at angle", {
    expect_error(
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
        scr_lines(
          size = 1
        ) |>
        scr_arrows(
          facet = "Attention",
          length = 0.1,
          angle = NULL,
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
        print(),
      "angle must not be NULL"
    )
  })

  it("Should fail at code", {
    expect_error(
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
        scr_lines(
          size = 1
        ) |>
        scr_arrows(
          facet = "Attention",
          length = 0.1,
          code = NULL,
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
        print(),
      "code must not be NULL"
    )
  })

  it("Should fail at lwd", {
    expect_error(
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
        scr_lines(
          size = 1
        ) |>
        scr_arrows(
          facet = "Attention",
          length = 0.1,
          lwd = NULL,
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
        print(),
      "lwd must not be NULL"
    )
  })

  it("Should fail at lty", {
    expect_error(
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
        scr_lines(
          size = 1
        ) |>
        scr_arrows(
          facet = "Attention",
          length = 0.1,
          lty = NULL,
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
        print(),
      "lty must not be NULL"
    )
  })
})
