library(fxl)

oldwd <- getwd()

if (require("here")) {
  setwd(paste(here::here("demo")))
}

# N>  override print in beez object

results <- FitCurves(
  apt,
  equation = "hs",
  detailed = FALSE
)

class(results) <- "data.frame"

# add in columns for preds

min_x <- 0
max_x <- 500
length_out <- 2500

x_points <- seq(
  min_x,
  max_x,
  length.out = length_out
)

pre_pop <- rep(NA, length(results$id) * length(x_points))

pre_pop <- matrix(pre_pop,
  nrow = length(results$id),
  ncol = length(x_points)
)

pre_pop <- as.data.frame(pre_pop)

colnames(pre_pop) <- as.character(x_points)

# results <- cbind(results, pre_pop)

for (row_num in seq_len(nrow(results))) {
  pre_pop[row_num, ] <- log10(results[row_num, "Q0d"]) +
    results[row_num, "K"] * exp(
      (-results[row_num, "Alpha"] *
        results[row_num, "Q0d"] *
        x_points) - 1
    )
}

pre_pop$id <- results$id

library(tidyverse)

pre_pop_gather <- pre_pop %>%
  gather(Price, Demand, -id) %>%
  mutate(
    Price = as.numeric(Price),
    Demand = as.numeric(Demand)
  )

# ggplot(pre_pop_gather, aes(Price, Demand, group = id)) +
#   geom_line() +
#   scale_x_log10() +
#   scale_y_log10()

library(fxl)

scr_plot(pre_pop_gather,
  aesthetics = list(
    x = Price,
    y = Demand
  )
) %>%
  scr_xoverride(xticks = c(0.01, 0.1, 1, 10, 100, 1000))

setwd(oldwd)
