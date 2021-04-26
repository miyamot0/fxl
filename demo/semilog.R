

minX = 1
maxX = 30

set.seed(65535)

xs  <- 1:30
ys  <- 0 + 5*xs + rnorm(length(xs), 0, 2)

ys2 <- 0.2 + 0.1*xs + rnorm(length(xs), 0, 0.25)

ys2[2] <- 0

plotDF = data.frame(
  X = xs,
  Y = ys2
)
