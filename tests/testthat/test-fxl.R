
current_data <- data.frame(
  sessions = seq_len(5),
  behavior = seq_len(5)
)

describe("scr_plot", {
  describe("data input", {
    it("Should PASS if data is a dataframe", {
      expect_error(
        scr_plot(data = current_data,
                 aesthetics = var_map(
                   x = sessions,
                   y = behavior
                 ),
                ),
        NA
      )
    })

    it("Should fail if data MISSING", {
      expect_error(
        scr_plot(aesthetics = var_map(
          x = sessions,
          y = behavior
        ),),
        "argument \"data\" is missing, with no default"
      )
    })

    it("Should fail if data NULL", {
      expect_error(
        scr_plot(
          data = NULL,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          )),
        "Parameter: data should NOT be set to a null value."
      )
    })

    it("Should fail if data BLANK", {
      expect_error(
        scr_plot(
          data = data.frame(
            sessions = numeric(0),
            behavior = numeric(0)
          ),
          aesthetics = var_map(
            x = sessions,
            y = behavior
          )),
        "Parameter: data contains no data."
      )
    })

    it("Should fail if aesthetics MISSING", {
      expect_error(
        scr_plot(data = current_data),
        "Parameter: aesthetics cannot be set to a null value."
      )
    })

    it("Should fail if aesthetics [y] MISSING", {
      expect_error(
        scr_plot(data = current_data,
                 aesthetics = var_map(
                   x = sessions
                 )),
        "Parameter: aesthetics must contain a mapping for y."
      )
    })

    it("Should fail if aesthetics [x] MISSING", {
      expect_error(
        scr_plot(data = current_data,
                 aesthetics = var_map(
                   y = behavior
                 )),
        "Parameter: aesthetics must contain a mapping for x."
      )
    })
  })

  describe("mai input", {
    it("Should fail if mai is NULL", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          mai = NULL
        ),
        "Parameter: mai should NOT be set to a null value."
      )
    })

    it("Should fail if mai is NOT NUMERIC", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          mai = c("1", "1", "1", "1")
        ),
        "Parameter: mai should be of a numeric type."
      )
    })
  })

  describe("omi input", {
    it("Should fail if omi is NULL", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          omi = NULL
        ),
        "Parameter: omi should NOT be set to a null value."
      )
    })

    it("Should fail if omi is NOT NUMERIC", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          omi = c("1", "1", "1", "1")
        ),
        "Parameter: omi should be of a numeric type."
      )
    })
  })

  describe("xaxs input", {
    it("Should PASS if valid", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          xaxs = "i"
        ),
        NA
      )

      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          xaxs = "r"
        ),
        NA
      )
    })

    it("Should fail if not a string", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          xaxs = "p"
        ),
        "Parameter: xaxs must be set to either \"i\" or \"r\""
      )
    })
  })

  describe("yaxs input", {
    it("Should PASS if valid", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          yaxs = "i"
        ),
        NA
      )

      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          yaxs = "r"
        ),
        NA
      )
    })

    it("Should fail if not a string", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          yaxs = TRUE
        ),
        "Parameter: yaxs must be a single-character value."
      )
    })

    it("Should fail if not a correct string type", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          yaxs = "p"
        ),
        "Parameter: yaxs must be set to either \"i\" or \"r\""
      )
    })
  })

  describe("family input", {
    it("Should PASS if valid", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          family = "serif"
        ),
        NA
      )
    })

    it("Should fail if not a string: NULL", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          family = NULL
        ),
        "Parameter: family should NOT be set to a null value."
      )
    })

    it("Should fail if not a string: NULL", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          family = 1
        ),
        "Parameter: family should be of a character type."
      )
    })
  })

  describe("semilog input", {
    it("Should PASS if valid: FALSE", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = FALSE
        ),
        NA
      )
    })

    it("Should PASS if valid: TRUE", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = TRUE
        ),
        NA
      )
    })

    it("Should fail if not a logical: NULL", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = NULL
        ),
        "Parameter: semilog should NOT be set to a null value."
      )
    })

    it("Should fail if not a logical: CHARACTER", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = "TRUE"
        ),
        "Parameter: semilog should be of a logical type."
      )
    })

    it("Should fail if not a single logical", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = c(TRUE, TRUE)
        ),
        "Parameter: semilog should have 1 entries but has 2 ."
      )
    })

    it("Should fail if x override is not a vector", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = c(TRUE, TRUE)
        ) |>
        scr_xoverride(14),
        "Parameter: scr_xoverride should have 2 entries but has 1 ."
      )
    })

    it("Should fail if y override is not a vector", {
      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = var_map(
            x = sessions,
            y = behavior
          ),
          semilog = c(TRUE, TRUE)
        ) |>
          scr_yoverride(14),
        "Parameter: scr_yoverride should have 2 entries but has 1 ."
      )
    })
  })
})
