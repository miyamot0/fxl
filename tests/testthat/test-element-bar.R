
current_data <- Gilroyetal2019

current_data$Integrity <- sample(
  80:100,
  nrow(current_data),
  replace = TRUE
)

describe("scr_bar_support", {
  it("Should fail at length: alpha", {
    expect_error(
      scr_plot(
        data,
        aesthetics = var_map(
          x = Session,
          y = CTB,
          p = Condition
        ),
        mai = c(
          0.5,
          0.5,
          0.1,
          0.5
        ),
        omi = c(
          0.25,
          0.25,
          0.25,
          0.25
        )
      ) %>%
        scr_yoverride(c(-.175, 5),
                      yticks = c(0, 1, 2, 3, 4, 5),
                      ytickslabs = c("0", "1", "2", "3", "4", "5")
        ) %>%
        scr_xoverride(c(0.5, 15.5),
                      xticks = 1:15,
                      xtickslabs = as.character(1:15)
        ) %>%
        scr_bar_support(
          mapping = list(
            x = Session,
            y = Integrity
          ),
          alpha = NULL,
          color = rgb(.8, .8, .8, alpha = 1),
          label = "Procedural Fidelity"
        ) %>%
        scr_lines(
          size = 1
        ) %>%
        scr_points(
          cex = 2,
          pch = list(
            "Toy Play" = 16,
            "Attention" = 22,
            "Demand" = 24,
            "Tangible" = 8
          ),
          fill = list(
            "Toy Play" = "black",
            "Attention" = "white",
            "Demand" = "white",
            "Tangible" = "black"
          )
        ) %>%
        print(),
      "Parameter: alpha should NOT be set to a null value."
    )
  })

  it("Should fail at type: label", {
    expect_error(
      scr_plot(
        data,
        aesthetics = var_map(
          x = Session,
          y = CTB,
          p = Condition
        ),
        mai = c(
          0.5,
          0.5,
          0.1,
          0.5
        ),
        omi = c(
          0.25,
          0.25,
          0.25,
          0.25
        )
      ) %>%
        scr_yoverride(c(-.175, 5),
                      yticks = c(0, 1, 2, 3, 4, 5),
                      ytickslabs = c("0", "1", "2", "3", "4", "5")
        ) %>%
        scr_xoverride(c(0.5, 15.5),
                      xticks = 1:15,
                      xtickslabs = as.character(1:15)
        ) %>%
        scr_bar_support(
          mapping = list(
            x = Session,
            y = Integrity
          ),
          color = rgb(.8, .8, .8, alpha = 1),
          label = 0
        ) %>%
        scr_lines(
          size = 1
        ) %>%
        scr_points(
          cex = 2,
          pch = list(
            "Toy Play" = 16,
            "Attention" = 22,
            "Demand" = 24,
            "Tangible" = 8
          ),
          fill = list(
            "Toy Play" = "black",
            "Attention" = "white",
            "Demand" = "white",
            "Tangible" = "black"
          )
        ) %>%
        print(),
      "Parameter: label should be of a character type."
    )
  })

  it("Should fail at length: width", {
    expect_error(
      scr_plot(
        data,
        aesthetics = var_map(
          x = Session,
          y = CTB,
          p = Condition
        ),
        mai = c(
          0.5,
          0.5,
          0.1,
          0.5
        ),
        omi = c(
          0.25,
          0.25,
          0.25,
          0.25
        )
      ) %>%
        scr_yoverride(c(-.175, 5),
                      yticks = c(0, 1, 2, 3, 4, 5),
                      ytickslabs = c("0", "1", "2", "3", "4", "5")
        ) %>%
        scr_xoverride(c(0.5, 15.5),
                      xticks = 1:15,
                      xtickslabs = as.character(1:15)
        ) %>%
        scr_bar_support(
          mapping = list(
            x = Session,
            y = Integrity
          ),
          width = NULL,
          color = rgb(.8, .8, .8, alpha = 1),
          label = "Procedural Fidelity"
        ) %>%
        scr_lines(
          size = 1
        ) %>%
        scr_points(
          cex = 2,
          pch = list(
            "Toy Play" = 16,
            "Attention" = 22,
            "Demand" = 24,
            "Tangible" = 8
          ),
          fill = list(
            "Toy Play" = "black",
            "Attention" = "white",
            "Demand" = "white",
            "Tangible" = "black"
          )
        ) %>%
        print(),
      "Parameter: alpha should NOT be set to a null value."
    )
  })
})
