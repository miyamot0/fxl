library(fxl)   # core plotting methods

dragon <- readPicture("../man/figures/dragon-cairo.ps.xml")
scorpion <- readPicture("../man/figures/scorpion-cairo.ps.xml")
skinner <- readPicture("../man/figures/skinner-cairo.ps.xml")

dataFrame <- data.frame(
  Time = seq_len(30),
  Control = c(c(4,3,2,4,2,1),
              rep(NA, 14),
              c(8,6,4,3,2,1),
              rep(NA, 4)),
  TreatA = c(rep(NA, 6),
             c(5,NA,6,NA,6,NA,8,NA,9,NA,11,NA,10,NA),
             rep(NA, 6),
             c(NA,NA,NA,NA)),
  TreatB = c(rep(NA, 6),
             c(NA,4,NA,8,NA,12,NA,10,NA,14,NA,14,NA,15),
             rep(NA, 6),
             c(10,11,14,15)),
  Phase = c(rep('First', 20),
            rep('Half', 10)),
  Facet = rep('P1', 30)
)

scr_plot(dataFrame,
         aesthetics = var_map(
           x = Time,
           y = Control,
           p = Phase,
           facet = Facet
         ),
         mai = c(0.375, 0.375, 0.1, 0.1),
         omi = c(0.25, 0.25, 0.25, 0.25)) |>
  scr_xoverride(
    c(0.4, 32),
    xticks = c(1, 5, 10, 15, 20, 25, 30)
  ) |>
  scr_yoverride(c(-0.5, 20),
                yticks = c(0, 5, 10, 15, 20),
                ydelta = 5) |>
  scr_lines() |>
  scr_lines(mapping = var_map(x = Time,
                              y = TreatA,
                              p = Phase)) |>
  scr_lines(mapping = var_map(x = Time,
                           y = TreatB)) |>
  scr_images(cex = 0.06,
             image = skinner) |>
  scr_images(cex = 0.06,
             image = dragon,
             mapping = var_map(x = Time,
                               y = TreatA)) |>
  scr_images(cex = 0.06,
             image = scorpion,
             mapping = var_map(x = Time,
                               y = TreatB)) |>
  scr_label_phase(facet = "P1",
                  cex    = 1.25,
                  adj    = 0.5,
                  y      = 20,
                  labels = list(
                    "Baseline"          = list(x = 3.5),
                    "ATD"               = list(x = 13.5),
                    "Baseline2"         = list(x = 23.5,
                                               label = "Baseline"),
                    "Best Treatment"    = list(x = 30)
                  )) |>
  scr_plines(
    lty = 1,
    lines = list(
      "P1" = list(
        "A" = list(
          x1 = 6.5,
          y1 = 20,
          y2 = -0.5
        ),
        "B" = list(
          x1 = 20.5,
          y1 = 20,
          y2 = -0.5
        ),
        "C" = list(
          x1 = 26.5,
          y1 = 20,
          y2 = -0.5
        )
      )
    )
  ) |>
  scr_xlabel("Session") |>
  scr_ylabel("          Work Output") |>
  scr_save(name = "../man/figures/atd_example_fun.svg",
           format = "svg",
           units = "in",
           height = 5.5,
           width = 11)
