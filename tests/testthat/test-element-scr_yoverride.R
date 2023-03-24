describe("scr_yoverride", {
  it("Should render as normal", {
    expect_error(
      scr_plot(
        data = LozyEtAl2020,
        aesthetics = var_map(
          x = Session,
          y = KM,
          p = Phase,
          facet = Participant
        ),
        ncol = 2,
        mai = c(0.3, 0.3, 0.0, 0.1),
        omi = c(0.25, 0.25, 0.1, 0)
      ) |>
        scr_xoverride(
          c(1, 30),
          xdraws = c(
            "Eva",
            "Cali",
            "Ari"
          ),
          xticks = list(
            "Eli" = c(1, 5, 10, 15),
            "Ari" = c(1, 10, 20, 30),
            "Al" = c(1, 5, 10, 15),
            "Ry" = c(1, 5, 10, 15),
            "Eva" = c(1, 5, 10, 15),
            "Cali" = c(1, 5, 10, 15)
          )
        ) |>
        scr_yoverride(
          list(
            "Eli" = list(
              y0 = 0,
              y1 = 15,
              yticks = c(0, 5, 10, 15)
            ),
            "Ari" = list(
              y0 = 0,
              y1 = 15,
              yticks = c(0, 5, 10, 15)
            ),
            "Al" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Ry" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Eva" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Cali" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            )
          ),
          ydraws = c(
            "Eli",
            "Al",
            "Eva"
          )
        ) |>
        print(),
      NA
    )
  })

  it("xdraws assert: char type", {
    expect_error(
      scr_plot(
        data = LozyEtAl2020,
        aesthetics = var_map(
          x = Session,
          y = KM,
          p = Phase,
          facet = Participant
        ),
        ncol = 2,
        mai = c(0.3, 0.3, 0.0, 0.1),
        omi = c(0.25, 0.25, 0.1, 0)
      ) |>
        scr_xoverride(
          c(1, 30),
          xticks = list(
            "Eli" = c(1, 5, 10, 15),
            "Ari" = c(1, 10, 20, 30),
            "Al" = c(1, 5, 10, 15),
            "Ry" = c(1, 5, 10, 15),
            "Eva" = c(1, 5, 10, 15),
            "Cali" = c(1, 5, 10, 15)
          )
        ) |>
        scr_yoverride(
          list(
            "Eli" = list(
              y0 = 0,
              y1 = 15,
              yticks = c(0, 5, 10, 15)
            ),
            "Ari" = list(
              y0 = 0,
              y1 = 15,
              yticks = c(0, 5, 10, 15)
            ),
            "Al" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Ry" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Eva" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Cali" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            )
          ),
          ydraws = TRUE
        ) |>
        print(),
      "Parameter: ydraws should be of a character type."
    )
  })

  it("var assert: present", {
    expect_error(
      scr_plot(
        data = LozyEtAl2020,
        aesthetics = var_map(
          x = Session,
          y = KM,
          p = Phase,
          facet = Participant
        ),
        ncol = 2,
        mai = c(0.3, 0.3, 0.0, 0.1),
        omi = c(0.25, 0.25, 0.1, 0)
      ) |>
        scr_xoverride(
          c(0, 30),
          xdraws = TRUE,
          xticks = list(
            "Eli" = c(1, 5, 10, 15),
            "Ari" = c(1, 10, 20, 30),
            "Al" = c(1, 5, 10, 15),
            "Ry" = c(1, 5, 10, 15),
            "Eva" = c(1, 5, 10, 15),
            "Cali" = c(1, 5, 10, 15)
          )
        ) |>
        scr_yoverride(
          ydraws = c(
            "Eli",
            "Al",
            "Eva"
          )
        ) |>
        print(),
      "argument \"var\" is missing, with no default"
    )
  })

  it("var assert: length", {
    expect_error(
      scr_plot(
        data = LozyEtAl2020,
        aesthetics = var_map(
          x = Session,
          y = KM,
          p = Phase,
          facet = Participant
        ),
        ncol = 2,
        mai = c(0.3, 0.3, 0.0, 0.1),
        omi = c(0.25, 0.25, 0.1, 0)
      ) |>
        scr_xoverride(
          c(1, 30, 10),
          xdraws = TRUE,
          xticks = list(
            "Eli" = c(1, 5, 10, 15),
            "Ari" = c(1, 10, 20, 30),
            "Al" = c(1, 5, 10, 15),
            "Ry" = c(1, 5, 10, 15),
            "Eva" = c(1, 5, 10, 15),
            "Cali" = c(1, 5, 10, 15)
          )
        ) |>
        scr_yoverride(
          c(1, 5, 10),
          ydraws = c(
            "Eli",
            "Al",
            "Eva"
          )
        ) |>
        print(),
      "Parameter: scr_yoverride should have 2 entries but has 3 ."
    )
  })

  it("ytickslabs: number", {
    expect_error(
      scr_plot(
        data = LozyEtAl2020,
        aesthetics = var_map(
          x = Session,
          y = KM,
          p = Phase,
          facet = Participant
        ),
        ncol = 2,
        mai = c(0.3, 0.3, 0.0, 0.1),
        omi = c(0.25, 0.25, 0.1, 0)
      ) |>
        scr_xoverride(
          c(1, 30),
          xticks = list(
            "Eli" = c(1, 5, 10, 15),
            "Ari" = c(1, 10, 20, 30),
            "Al" = c(1, 5, 10, 15),
            "Ry" = c(1, 5, 10, 15),
            "Eva" = c(1, 5, 10, 15),
            "Cali" = c(1, 5, 10, 15)
          )
        ) |>
        scr_yoverride(
          list(
            "Eli" = list(
              y0 = 0,
              y1 = 15,
              yticks = c(0, 5, 10, 15)
            ),
            "Ari" = list(
              y0 = 0,
              y1 = 15,
              yticks = c(0, 5, 10, 15)
            ),
            "Al" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Ry" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Eva" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            ),
            "Cali" = list(
              y0 = 0,
              y1 = 8,
              yticks = c(0, 2, 4, 6, 8)
            )
          ),
          ytickslabs = c(1, 5, 10),
          ydraws = c(
            "Eli",
            "Al",
            "Eva"
          )
        ) |>
        print(),
      "Parameter: ytickslabs should be of a character type."
    )
  })
})
