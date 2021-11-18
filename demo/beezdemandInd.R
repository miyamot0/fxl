library(beezdemand)

apt

#N>  override print in beez object

results = FitCurves(apt, equation = "hs", detailed = FALSE)
class(results) = "data.frame"

# add in columns for preds

minX      = 0
maxX      = 500
lengthOut = 2500

xPoints = seq(minX, maxX,
              length.out = lengthOut)

prePop = rep(NA, length(results$id) * length(xPoints))
prePop = matrix(prePop,
                nrow = length(results$id),
                ncol = length(xPoints))
prePop = as.data.frame(prePop)
colnames(prePop) = as.character(xPoints)

#results = cbind(results, prePop)

for (rowNum in 1:nrow(results))
  prePop[rowNum, ] = log10(results[rowNum, "Q0d"]) +
    results[rowNum, "K"] * exp(
      (-results[rowNum, "Alpha"] * results[rowNum, "Q0d"] * xPoints) - 1)

prePop$id = results$id

library(tidyverse)

prePop.gather = prePop %>%
  gather(Price, Demand, -id) %>%
  mutate(Price  = as.numeric(Price),
         Demand = as.numeric(Demand))

# ggplot(prePop.gather, aes(Price, Demand, group = id)) +
#   geom_line() +
#   scale_x_log10() +
#   scale_y_log10()

library(fxl)

scr_plot(prePop.gather, aesthetics = list(x = Price,
                                          y = Demand)) +
  scr_xoverride(xticks = c(0.01, 0.1, 1, 10, 100, 1000))
