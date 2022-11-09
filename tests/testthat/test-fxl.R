library(fxl)

current_data <- data.frame(
  sessions = seq_len(5),
  behavior = seq_len(5)
)

describe("scr_plot", {
  describe("data input", {
    it("Should PASS if data is a dataframe", {
      expect_error(
        scr_plot(data = current_data,
                 aesthetics = expr(
                   list(
                     x = sessions,
                     y = behavior
                   )
                 )),
        NA
      )
    })

    it("Should fail if data MISSING", {
      expect_error(
        scr_plot(aesthetics = expr(
          list(
            x = sessions,
            y = behavior
          )
        )),
        "argument \"data\" is missing, with no default"
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
                 aesthetics = expr(
                   list(
                     x = sessions,
                   )
                 )),
        "Parameter: aesthetics must contain a mapping for y."
      )
    })

    it("Should fail if aesthetics [x] MISSING", {
      expect_error(
        scr_plot(data = current_data,
                 aesthetics = expr(
                   list(
                     y = behavior
                   )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
          ),
          xaxs = "i"
        ),
        NA
      )

      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
          ),
          yaxs = "i"
        ),
        NA
      )

      expect_error(
        scr_plot(
          data = current_data,
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
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
          aesthetics = expr(
            list(
              x = sessions,
              y = behavior
            )
          ),
          yaxs = "p"
        ),
        "Parameter: yaxs must be set to either \"i\" or \"r\""
      )
    })
  })
})
