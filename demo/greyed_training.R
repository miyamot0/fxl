library(fxl)

oldwd <- getwd()

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

set.seed(65535)

n_sessions <- 80
n_targets <- 4
var_targets <- runif(n_targets,
  min = 0.1,
  max = 0.25
)
bl_targets <- runif(n_targets,
  min = 0,
  max = 0.2
)
pl_shifts <- runif(n_targets,
  min = 0.4,
  max = 0.8
)

pl_change <- c(15, 30, 45, 60)

session_target <- 0

main_data_frame <- data.frame(
  Session = numeric(0),
  Phase = character(0),
  Facet = character(0),
  Percentage = numeric(0),
  Target = character(0)
)

for (t in seq_len(n_sessions)) {
  session_target <- session_target + 1
  session_target <- ifelse(session_target > 4, 1, session_target)

  rel_var <- var_targets[session_target]
  rel_bl <- bl_targets[session_target]
  rel_pls <- pl_shifts[session_target]
  rel_plc <- pl_change[session_target]

  phase <- ifelse(t < rel_plc, "Baseline", "Intervention")
  phase_dummy <- ifelse(t < rel_plc, 0, 1)

  pct <- rel_bl + rel_pls * phase_dummy + rnorm(1, 0, rel_var)
  pct <- pct * 100

  pct <- ifelse(pct > 100, 100, pct)
  pct <- ifelse(pct < 0, 0, pct)

  data_frame_addendum <- data.frame(
    Session = t,
    Phase = phase,
    Facet = as.character(session_target),
    Percentage = pct,
    Target = "Main"
  )

  main_data_frame <- rbind(
    main_data_frame,
    data_frame_addendum
  )
}

main_data_frame$Therapist <- "Primary"

main_data_frame_backup <- main_data_frame

t <- 1

for (t in seq_len(n_targets)) {
  rel_facet <- as.character(t)
  rel_plc <- pl_change[t]
  rel_top <- rel_plc + (n_targets * 3)

  main_data_frame <- subset(
    main_data_frame,
    !(Facet == rel_facet &
      Session >= rel_plc &
      Session < rel_top)
  )
}

gen_session <- c(
  5, 6, 7, 8,
  17, 18, 19, 20,
  25, 26, 27, 28,
  37, 38, 39, 40,
  45, 46, 47, 48,
  57, 58, 59, 60,
  65, 66, 67, 68,
  77, 78, 79, 80
)

main_data_frame[main_data_frame$Session %in% gen_session, "Therapist"] <- "Generalization"

x_ticks <- c(1, (1:16) * 5)

scr_plot(
  subset(
    main_data_frame,
    Therapist == "Primary"
  ),
  aesthetics = var_map(
    x = Session,
    y = Percentage,
    p = Phase,
    facet = Facet
  ),
  mai = c(0.125, 0.375, 0.25, 0.25),
  omi = c(0.5, 0.25, 0.25, 0.25)
) |>
  scr_yoverride(
    c(-10, 100),
    ydelta = 25,
    yticks = c(0, 25, 50, 75, 100),
    ytickslabs = as.character(c(0, 25, 50, 75, 100))
  ) |>
  scr_xoverride(
    c(0, n_sessions),
    xticks = x_ticks
  ) |>
  scr_anno_rect(
    rects = list(
      "1" = list(
        x0 = pl_change[1],
        x1 = pl_change[1] + 11,
        y0 = -9,
        y1 = 100
      ),
      "2" = list(
        x0 = pl_change[2],
        x1 = pl_change[2] + 11,
        y0 = -9,
        y1 = 100
      ),
      "3" = list(
        x0 = pl_change[3],
        x1 = pl_change[3] + 11,
        y0 = -9,
        y1 = 100
      ),
      "4" = list(
        x0 = pl_change[4],
        x1 = pl_change[4] + 11,
        y0 = -9,
        y1 = 100
      )
    ),
    fill = "gray",
    color = "gray"
  ) |>
  scr_ylabel("Accuracy") |>
  scr_xlabel("Session",
    line = 2
  ) |>
  scr_lines() |>
  scr_points(
    pch = 23,
    cex = 2,
    fill = "white"
  ) |>
  scr_points(
    pch = 22,
    cex = 2,
    fill = "green",
    mapping = list(
      x = Session,
      y = Percentage
    ),
    data = subset(
      main_data_frame,
      Therapist == "Generalization"
    )
  ) |>
  scr_plines_mbd(
    lines = list(
      "A" = list(
        "1" = list(
          x1 = pl_change[1],
          y1 = 100,
          y2 = -30
        ),
        "2" = list(
          x1 = pl_change[2],
          y1 = 100,
          y2 = -30
        ),
        "3" = list(
          x1 = pl_change[3],
          y1 = 100,
          y2 = -30
        ),
        "4" = list(
          x1 = pl_change[4],
          y1 = 100,
          y2 = -10
        )
      )
    )
  ) |>
  scr_label_facet(
    cex = 1.25,
    adj = 1,
    y = 115,
    face = 2,
    x = 80,
    labels = list(
      "1" = list(
        label = "Participant 1"
      ),
      "2" = list(
        label = "Participant 2"
      ),
      "3" = list(
        label = "Participant 3"
      ),
      "4" = list(
        label = "Participant 4"
      )
    )
  ) |>
  scr_label_phase(
    facet = "1",
    cex = 1.125,
    adj = 0.5,
    y = 110,
    labels = list(
      "Baseline" = list(
        x = 7.5
      ),
      "Intervention" = list(
        x = 37.5
      )
    )
  ) |>
  scr_label_phase(
    facet = "1",
    cex = 1.125,
    adj = 0,
    y = 110,
    labels = list(
      "Errorless Training" = list(
        x = 35,
        y = 5,
        adj = 0
      )
    )
  ) |>
  scr_anno_arrows(
    facet = "1",
    length = 0.075,
    code = 1,
    color = "black",
    lty = 1,
    lwd = 1,
    arrows = list(
      "A" = list(
        x0 = 27,
        y0 = 5,
        x1 = 34,
        y1 = 5
      )
    )
  ) |>
  scr_legend(
    panel = "1",
    position = "bottomright",
    legend = c(
      "Independence",
      "Generalization Probe"
    ),
    col = c(
      "black",
      "black"
    ),
    pt_bg = c(
      "white",
      "green"
    ),
    lty = c(
      1,
      1
    ),
    pch = c(
      23,
      22
    ),
    bty = "n",
    pt_cex = 2.25,
    cex = 1.25,
    text_col = "black",
    horiz = FALSE,
    box_lty = 0
  ) |>
  # scr_save(
  #   name = "../man/figures/training_off_main_graph.svg",
  #   format = "svg",
  #   units = "in",
  #   height = 6,
  #   width = 9
  # ) |>
  scr_save(
    name = "../man/figures/training_off_main_graph.png",
    format = "png",
    res = 300,
    height = 6,
    width = 9
  )

setwd(oldwd)
