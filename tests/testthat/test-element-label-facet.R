current_data <- data.frame(
  sessions = seq_len(5),
  behavior = seq_len(5)
)

describe("element-label-facet", {
  it("Should fail if color is not a character vector", {
    expect_error(
      scr_plot(
        data = current_data,
        aesthetics = var_map(
          x = sessions,
          y = behavior
        ),
      ) |>
        scr_label_facet(
          labels = c(""),
          color = 1
        ),
      "Error ('color'): was of type numeric, check the data type supplied",
      fixed = TRUE
    )
  })

  it("Should fail if cex is not a numeric vector", {
    expect_error(
      scr_plot(
        data = current_data,
        aesthetics = var_map(
          x = sessions,
          y = behavior
        ),
      ) |>
        scr_label_facet(
          labels = c(""),
          cex = "1"
        ),
      "Error ('cex'): was of type character, check the data type supplied",
      fixed = TRUE
    )
  })

  it("Should fail if color is not a numeric vector", {
    expect_error(
      scr_plot(
        data = current_data,
        aesthetics = var_map(
          x = sessions,
          y = behavior
        ),
      ) |>
        scr_label_facet(
          labels = c(""),
          adj = "1"
        ),
      "Error ('adj'): was of type character, check the data type supplied",
      fixed = TRUE
    )
  })

  it("Should fail if color is not a numeric vector", {
    expect_error(
      scr_plot(
        data = current_data,
        aesthetics = var_map(
          x = sessions,
          y = behavior
        ),
      ) |>
        scr_label_facet(
          labels = c(""),
          face = "1"
        ),
      "Error ('face'): was of type character, check the data type supplied",
      fixed = TRUE
    )
  })

  it("Should fail if labels is not a list and not character vector", {
    expect_error(
      scr_plot(
        data = current_data,
        aesthetics = var_map(
          x = sessions,
          y = behavior
        ),
      ) |>
      scr_label_facet(
        labels = c(1)
      ),
      "Error ('labels'): was of type numeric, check the data type supplied",
      fixed = TRUE
    )
  })
})
