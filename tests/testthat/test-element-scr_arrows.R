
current_data <- Gilroyetal2019Tx %>%
  mutate(Condition = paste0(Condition, PhaseNum)) %>%
  rename(Function = Participant,
         AFCR = FCR,
         EFCR = FCR2) %>%
  select(-c(PhaseNum, LineOff))

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
      ) %>%
        scr_lines(
          size = 1
        ) %>%
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
        ) %>%
        print(),
      "Parameter: length should NOT be set to a null value."
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
      ) %>%
        scr_lines(
          size = 1
        ) %>%
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
        ) %>%
        print(),
      "Parameter: angle should NOT be set to a null value."
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
      ) %>%
        scr_lines(
          size = 1
        ) %>%
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
        ) %>%
        print(),
      "Parameter: code should NOT be set to a null value."
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
      ) %>%
        scr_lines(
          size = 1
        ) %>%
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
        ) %>%
        print(),
      "Parameter: lwd should NOT be set to a null value."
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
      ) %>%
        scr_lines(
          size = 1
        ) %>%
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
        ) %>%
        print(),
      "Parameter: lty should NOT be set to a null value."
    )
  })

})
