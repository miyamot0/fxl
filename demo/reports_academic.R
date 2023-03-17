


# times <-
#
#
# rates <- c(0.025)
# rate <- rates[1]
#
# ests <-
# ests

library(fxl)
#
# randomId = 626237
#
# #set.seed(76066)
#
# set.seed(randomId)
#
# ids <- seq_len(24)
# rates <- runif(length(ids),
#                min = 0.0001,
#                max = 0.01)
# starts <- runif(length(ids),
#                min = 0,
#                max = 2)
# jitter <- runif(length(ids),
#                 min = -3,
#                 max = 3)
# times = c(1, 30, 60, 90, 120, 150, 180)
#
# simulations <- expand.grid(
#   Rates = rates,
#   Times = times
# )
#
# simulations$index <- match(simulations$Rates, rates)
# simulations$starts <- starts[match(simulations$Rates, rates)]
# simulations$jitter <- jitter[match(simulations$Rates, rates)]
#
# range <- 80
# simulations$pred <- simulations$starts - range * (exp(-simulations$Rates * 1 * simulations$Times) - 1)
# simulations$err <- rnorm(nrow(simulations), 0, 6)
#
# simulations$pred <- simulations$pred + (simulations$pred/45) * simulations$err
#
# simulations$pred <- ifelse(simulations$pred < 0.1, 0, simulations$pred)
# simulations$Times <- simulations$Times + simulations$jitter
#
# SimulatedAcademicFluency <- simulations
#
# #SimulatedAcademicFluency
#
# save(SimulatedAcademicFluency, file = "SimulatedAcademicFluency.RData")

needFluency <- SimulatedAcademicFluency[SimulatedAcademicFluency$Times > 160 &
                                          SimulatedAcademicFluency$pred < 40 &
                                          SimulatedAcademicFluency$pred > 10, 'index']

needAccuracy <- SimulatedAcademicFluency[SimulatedAcademicFluency$Times > 160 &
                                           SimulatedAcademicFluency$pred < 10, 'index']

fluencyString <- paste("Consider Fluency Intervention:", paste(needFluency, collapse = ', '))
accuracyString <- paste("Consider Accuracy Intervention:", paste(needAccuracy, collapse = ', '))

labelList <- list()
labelList[[fluencyString]] <- list(
  x = 130,
  y = 11.5,
  color = "orange"
)
labelList2 <- list()
labelList2[[accuracyString]] <- list(
 x = 130,
 y = .115,
 color = "red"
)

scr_plot(
  SimulatedAcademicFluency,
  aesthetics = var_map(
    x = Times,
    y = pred,
    p = index
  ),
  omi = c(
    0.0,
    0.35,
    0.2,
    0.25
  ),
  mai = c(
    0.0,
    0.25,
    0.1,
    0.25
  ),
  semilog = TRUE
) |>
  scr_yoverride(c(0.1, 100)) |>
  scr_xoverride(c(1, 183),
                xticks = c(1, 30, 60, 90, 120, 150, 180),
                xtickslabs = c(
                  'September',
                  'October',
                  'November',
                  'December',
                  'January',
                  'February',
                  'March'
                )) |>
  scr_title("Addition Sums to 12: Monthly Class-wide Screening") |>
  scr_xlabel("Days Into Academic School Year") |>
  scr_ylabel("Digits Correct Per Minute (DCPM)") |>
  scr_guide_line(
    color = "green",
    lty = 1,
    lwd = 2,
    coords = list(
      "A" = list(
        x0 = -3,
        x1 = 190,
        y0 = 41,
        y1 = 41
      ),
      "B" = list(
        x0 = -3,
        x1 = 190,
        y0 = 100,
        y1 = 100
      ),
      "C" = list(
        x0 = -3,
        x1 = -3,
        y0 = 41,
        y1 = 100
      ),
      "D" = list(
        x0 = 190,
        x1 = 190,
        y0 = 41,
        y1 = 100
      )
    )
  ) |>
  scr_guide_line(
    color = "orange",
    lty = 1,
    lwd = 2,
    coords = list(
      "A" = list(
        x0 = -3,
        x1 = 190,
        y0 = 10,
        y1 = 10
      ),
      "B" = list(
        x0 = -3,
        x1 = 190,
        y0 = 39,
        y1 = 39
      ),
      "C" = list(
        x0 = -3,
        x1 = -3,
        y0 = 39,
        y1 = 10
      ),
      "D" = list(
        x0 = 190,
        x1 = 190,
        y0 = 39,
        y1 = 10
      )
    )
  ) |>
  scr_guide_line(
    color = "red",
    lty = 1,
    lwd = 2,
    coords = list(
      "A" = list(
        x0 = -3,
        x1 = 190,
        y0 = .1,
        y1 = .1
      ),
      "B" = list(
        x0 = -3,
        x1 = 190,
        y0 = 9.5,
        y1 = 9.5
      ),
      "C" = list(
        x0 = -3,
        x1 = -3,
        y0 = 9.5,
        y1 = .1
      ),
      "D" = list(
        x0 = 190,
        x1 = 190,
        y0 = 9.5,
        y1 = .1
      )
    )
  ) |>
  scr_lines() |>
  scr_points(
    pch = 21,
    fill = 'white',
    cex = 1.5
  ) |>
  scr_label_phase(
    cex = 1,
    adj = 0,
    labels = labelList
  ) |>
  scr_label_phase(
    cex = 1,
    adj = 0,
    labels = labelList2
  ) |>
  scr_save(name = "../man/figures/celeration_academic.svg",
           format = "svg",
           units = "in",
           height = 6,
           width = 9)
