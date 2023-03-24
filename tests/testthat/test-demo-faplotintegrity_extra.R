
set.seed(65535)

participants <- seq_len(3)

sessions_per_cond <- 4

ctrl_sim <- abs(rnorm(max(participants) * sessions_per_cond, 0, 0.125))
attn_sim <- abs(rnorm(max(participants) * sessions_per_cond, 1, 0.25))
dmnd_sim <- abs(rnorm(max(participants) * sessions_per_cond, 1, 0.25))
tang_sim <- abs(rnorm(max(participants) * sessions_per_cond, 0, 0.125))

data_frame <- data.frame(
  Participant = numeric(0),
  Condition = character(0),
  CTB = numeric(0),
  Session = numeric(0)
)

for (p in participants) {
  conditions <- c(
    rep("Control", sessions_per_cond),
    rep("Attention", sessions_per_cond),
    rep("Demand", sessions_per_cond),
    rep("Tangible", sessions_per_cond)
  )

  indices <- (p - 1) * sessions_per_cond + 1
  indices <- c(
    indices,
    indices + 1,
    indices + 2,
    indices + 3
  )

  rates <- c(
    ctrl_sim[indices],
    attn_sim[indices],
    dmnd_sim[indices],
    tang_sim[indices]
  )

  new_p <- data.frame(
    Participant = rep(p, sessions_per_cond * 4),
    Condition = conditions,
    CTB = rates
  )

  new_p_shuffled = new_p[sample(1:nrow(new_p)), ]
  new_p_shuffled$Session <- 1:nrow(new_p)

  data_frame <- rbind(
    data_frame,
    new_p_shuffled
  )
}

data_frame$Integrity <- sample(
  70:100,
  nrow(data_frame),
  replace = TRUE
)

bar_styler <- function(data_frame, ...) {
  input_list <- list(...)

  local_frame <- input_list[['plot_frame']]
  local_frame$col <- 'orange'

  local_frame[local_frame$pct >= .95, 'col'] <- 'green'
  local_frame[local_frame$pct < .95 & local_frame$pct >= .80, 'col'] <- 'lightgreen'
  local_frame[local_frame$pct < .80, 'col'] <- 'orange'

  rect(local_frame$X - 0.25,
       0,
       local_frame$X + 0.25,
       local_frame$mod_y,
       col = local_frame$col)
}

describe("FA Plot with Integrity", {
  it("Should render as normal", {
    expect_no_error(
      scr_plot(
        data_frame,
        aesthetics = var_map(
          x = Session,
          y = CTB,
          p = Condition,
          facet = Participant
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
      ) |>
        scr_yoverride(c(-.175, 5),
                      yticks = c(0, 1, 2, 3, 4, 5),
                      ytickslabs = c("0", "1", "2", "3", "4", "5")
        ) |>
        scr_xoverride(c(0.5, 16.5),
                      xticks = 1:16,
                      xtickslabs = as.character(1:16)
        ) |>
        scr_bar_support(
          color = rgb(.8, .8, .8, alpha = 1),
          guide_line = 80,
          guide_line_color = 'blue',
          guide_line_type = 2,
          guide_line_size = 1,
          styler = bar_styler,
          label = "Procedural Fidelity",
          mapping = list(
            x = Session,
            y = Integrity
          )
        ) |>
        scr_lines(
          size = 1
        ) |>
        scr_points(
          cex = 2,
          pch = list(
            "Control" = 21,
            "Attention" = 22,
            "Demand" = 24,
            "Tangible" = 8
          ),
          fill = list(
            "Control" = "black",
            "Attention" = "white",
            "Demand" = "white",
            "Tangible" = "black"
          ),
          color = list(
            "Control" = "black",
            "Attention" = "black",
            "Demand" = "black",
            "Tangible" = "black"
          )
        ) |>
        scr_xlabel("Session") |>
        scr_ylabel("Combined Target Behavior (Per Minute)") |>
        scr_title("Analog Functional Analyses an Assocated Procedural Fidelity") |>
        scr_legend(
          panel = "3",
          position = list(
            x = 0.85,
            y = 4.75
          ), # Specify legend location
          legend = c(
            "Toy Play", # labels to include (ordered)
            "Attention",
            "Demand",
            "Tangible"
          ),
          col = c(
            "black", # color of markers (ordered)
            "black",
            "black",
            "black"
          ),
          bg = "white",
          pt_bg = c(
            "black", # color of markers (ordered)
            "white",
            "white",
            "black"
          ),
          lty = c(1, 1, 1, 1), # line types (ordered)
          pch = c(16, 22, 24, 8), # marker types (ordered)
          bty = "y", # remove border
          pt_cex = 2.25, # point size scale
          cex = 1.25, # text size scale
          text_col = "black", # text color
          horiz = FALSE, # list items vertically
          box_lty = 1) |>
    print()
  )
  })
})
