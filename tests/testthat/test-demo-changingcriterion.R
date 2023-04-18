
set.seed(65535)

n_phases = 6

mults <- c(seq_len(n_phases - 2),
           n_phases - 3,
           n_phases - 2)

multiplier <- 12

n_sessions = rep(4, n_phases)
criteria = mults * multiplier
noise <- 2.5

seed_frame = data.frame(
  Phase = seq_len(n_phases),
  Sessions = n_sessions,
  Criterion = criteria
)

final_frame = data.frame(
  Phase = character(0),
  Session = numeric(0),
  Criterion = numeric(0)
)

for (index in seq_len(nrow(seed_frame))) {
  values <- seed_frame[index, ]

  novel_frame = data.frame(
    Phase = rep(as.character(values[1, "Phase"]), values[1, "Sessions"]),
    Session = rep(0, values[1, "Sessions"]),
    Criterion = rep(values[1, "Criterion"], values[1, "Sessions"])
  )

  final_frame <- rbind(final_frame,
                       novel_frame)
}

final_frame$Session <- seq_len(nrow(final_frame))
final_frame$Responding <- final_frame$Criterion + rnorm(nrow(final_frame),
                                                        0,
                                                        noise)

final_frame[1:4, "Responding"] <- rnorm(4,
                                        3,
                                        noise)

final_frame$Facet <- "1"

describe("Changing criterion plot", {
  it("Should render as normal without facet", {
    expect_no_error(
      scr_plot(final_frame,
               aesthetics = var_map(
                 x = Session,
                 y = Responding,
                 p = Phase
               ),
               mai = c(0.50, 0.50, 0.375, 0.25),
               omi = c(0.25, 0.25, 0.25, 0.25)
      ) |>
        scr_yoverride(
          c(-2.5, 60),
          yticks = (0:6) * 10,
        ) |>
        scr_xoverride(
          c(0, 25),
          xdelta = 10,
          xticks = c(
            1,
            seq(5, 25,
                by = 5
            )
          )
        ) |>
        scr_label_phase(
          y = 62.5,
          cex = 1.25,
          labels = list(
            "Baseline" = list(
              x = 2.25
            ),
            "Intervention" = list(
              x = 7.5
            ),
            "Participant #1" = list(
              x = 25,
              y = 2.5,
              adj = 1,
              font = 2
            )
          )
        ) |>
        scr_label_phase(
          color = "blue",
          cex = 1.25,
          labels = list(
            "A" = list(
              x = 6.5,
              y = 18,
              label = 24
            ),
            "B" = list(
              x = 10.5,
              y = 30,
              label = 36
            ),
            "C" = list(
              x = 14.5,
              y = 42,
              label = 48
            ),
            "D" = list(
              x = 18.5,
              y = 30,
              label = 36
            ),
            "E" = list(
              x = 22.5,
              y = 42,
              label = 48
            )
          )
        ) |>
        scr_ylabel("Work Problems Completed",
                   cex = 1.25) |>
        scr_xlabel("Sessions",
                   cex = 1.25) |>
        scr_title("Changing Criterion Example Figure",
                  cex = 1.5,
                  face = 2) |>
        scr_criterion_lines(
          lty = 1,
          size = 2,
          color = "blue",
          lines = list(
            "1" = list(
              level = 2 * multiplier,
              x1 = 5,
              x2 = 8
            ),
            "2" = list(
              level = 3 * multiplier,
              x1 = 9,
              x2 = 12
            ),
            "3" = list(
              level = 4 * multiplier,
              x1 = 13,
              x2 = 16
            ),
            "4" = list(
              level = 3 * multiplier,
              x1 = 17,
              x2 = 20
            ),
            "5" = list(
              level = 4 * multiplier,
              x1 = 21,
              x2 = 24
            )
          )
        ) |>
        scr_points(cex = 1.25) |>
        scr_lines() |>
        scr_plines(
          lines = list(
            "A" = list(
              x1 = 4.5,
              y1 = 60,
              y2 = -2.5
            )
          )
        ) |>
        print()
    )
  })

  it("Should render as normal with facet", {
    expect_no_error(
      scr_plot(final_frame,
               aesthetics = var_map(
                 x = Session,
                 y = Responding,
                 p = Phase,
                 facet = Facet
               ),
               mai = c(0.50, 0.50, 0.375, 0.25),
               omi = c(0.25, 0.25, 0.25, 0.25)
      ) |>
        scr_yoverride(
          c(-2.5, 60),
          yticks = (0:6) * 10,
        ) |>
        scr_xoverride(
          c(0, 25),
          xdelta = 10,
          xticks = c(
            1,
            seq(5, 25,
                by = 5
            )
          )
        ) |>
        scr_label_phase(
          facet = "1",
          y = 62.5,
          cex = 1.25,
          labels = list(
            "Baseline" = list(
              x = 2.25
            ),
            "Intervention" = list(
              x = 7.5
            ),
            "Participant #1" = list(
              x = 25,
              y = 2.5,
              adj = 1,
              font = 2
            )
          )
        ) |>
        scr_label_phase(
          facet = "1",
          color = "blue",
          cex = 1.25,
          labels = list(
            "A" = list(
              x = 6.5,
              y = 18,
              label = 24
            ),
            "B" = list(
              x = 10.5,
              y = 30,
              label = 36
            ),
            "C" = list(
              x = 14.5,
              y = 42,
              label = 48
            ),
            "D" = list(
              x = 18.5,
              y = 30,
              label = 36
            ),
            "E" = list(
              x = 22.5,
              y = 42,
              label = 48
            )
          )
        ) |>
        scr_ylabel("Work Problems Completed",
                   cex = 1.25) |>
        scr_xlabel("Sessions",
                   cex = 1.25) |>
        scr_title("Changing Criterion Example Figure",
                  cex = 1.5,
                  face = 2) |>
        scr_criterion_lines(
          lty = 1,
          size = 2,
          color = "blue",
          lines = list(
            "1" = list(
              "1" = list(
                level = 2 * multiplier,
                x1 = 5,
                x2 = 8
              ),
              "2" = list(
                level = 3 * multiplier,
                x1 = 9,
                x2 = 12
              ),
              "3" = list(
                level = 4 * multiplier,
                x1 = 13,
                x2 = 16
              ),
              "4" = list(
                level = 3 * multiplier,
                x1 = 17,
                x2 = 20
              ),
              "5" = list(
                level = 4 * multiplier,
                x1 = 21,
                x2 = 24
              )
            )
          )
        ) |>
        scr_points(cex = 1.25) |>
        scr_lines() |>
        scr_plines(
          lines = list(
            "A" = list(
              x1 = 4.5,
              y1 = 60,
              y2 = -2.5
            )
          )
        ) |>
        print()
    )
  })
})
