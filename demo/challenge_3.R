library(fxl)
library(tidyverse)

oldwd <- getwd()

## TODO: add to examples once final data imported

if ("here" %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

days <- seq(0, 365)

l_year <- 365
n_panels <- seq_len(1)
n_years <- seq_len(26)

final_frame <- data.frame(
  Days = numeric(0),
  Panel = numeric(0),
  Year = numeric(0),
  Bills = numeric(0),
  delta_bills = numeric(0)
)

  for (y in n_years) {

    new_frame <- data.frame(
      Days = 1:l_year,
      Year = rep(y, length(1:l_year)),
      Panel = rep(1, length(1:l_year))
    )

    new_frame$delta_bills <- new_frame$Days * ((new_frame$Days/365) * 1.5)

    prev_bills <- 0
    prev_days <- 0

    if (nrow(final_frame) > 0) {
      prev_bills <- max(final_frame$Bills)
      prev_days <- max(final_frame$Days)
    }

    new_frame$Bills <- new_frame$delta_bills + prev_bills
    new_frame$Days <- new_frame$Days + prev_days

    final_frame <- rbind(
      final_frame,
      new_frame
    )
  }


# final_frame

final_frame$RawDays <- final_frame$Days

final_frame$Panel <- ifelse(final_frame$Days > l_year * 9,  2, final_frame$Panel)
final_frame$Panel <- ifelse(final_frame$Days > l_year * 13, 3, final_frame$Panel)
final_frame$Panel <- ifelse(final_frame$Days > l_year * 17, 4, final_frame$Panel)

final_frame$Days <- ifelse(final_frame$RawDays > l_year * 9,  final_frame$RawDays - l_year * 9,  final_frame$Days)
final_frame$Days <- ifelse(final_frame$RawDays > l_year * 13, final_frame$RawDays - l_year * 13, final_frame$Days)
final_frame$Days <- ifelse(final_frame$RawDays > l_year * 17, final_frame$RawDays - l_year * 17, final_frame$Days)

final_frame$BillsRecorder <- final_frame$Bills %% 1000
final_frame$BillsScallops <- round(final_frame$Bills/1000, 0)
final_frame$BillsYears <- round(final_frame$RawDays/365, 0)

png("../man/figures/challenge_3.png",
    units = "in",
    width = 11,
    height = 8,
    res = 600
)

layout_mat  <- matrix(c(1,1,
                        2,5,
                        3,6,
                        4,4),
                      nrow = 4,
                      ncol = 2,
                      byrow = TRUE)

layout_vec_h <- c(1, 1)
layout_vec_v <- c(1, 1, 1, 1)

str_map <- function(x) {
  return(
    paste(
      "Session", x
    )
  )
}

str_th <- function(x) {
  return(
    paste0(
      x, "th"
    )
  )
}

scr_plot(final_frame,
         aesthetics = var_map(
           x = Days,
           y = BillsRecorder,
           p = Year,
           g = Year,
           facet = Panel
         ),
         omi = c(
           0.375,
           0.375,
           0.5,
           0.5
         ),
         mai = c(
           0.375,
           0.5,
           0.25,
           0.25
         ),
         layout = layout_mat,
         layout_v = layout_vec_v,
         layout_h = layout_vec_h
) |>
scr_yoverride(c(-100, 1100),
              yticks = c(0, 200, 400, 600, 800, 1000),
              ytickslabs = as.character(c(0, 200, 400, 600, 800, 1000))) |>
scr_xoverride(c(-100, 9.5 * l_year),
              xdraws = c(
                "1",
                "2",
                "3",
                "4"
              ),
              xticks = list(
                "1" = c((c(0:8) + 0.5) * l_year, 9 * l_year),
                "2" = c((c(0:3) + 0.5) * l_year, 4 * l_year),
                "3" = c((c(0:3) + 0.5) * l_year, 4 * l_year),
                "4" = c((c(0:8) + 0.5) * l_year, 9 * l_year)
              ),
              xtickslabs = list(
                "1" = c(str_map(81:89), ""),
                "2" = c(str_map(90:93), ""),
                "3" = c(str_map(94:97), ""),
                "4" = c(str_map(98:106), "")
              )) |>
scr_xlabel("Congress") |>
scr_ylabel("Cumulative Bills") |>
scr_title("Figure Recreation: Critchfield et al. (2003)",
          cex = 1.25,
          face = 2) |>
scr_points(
 pch = 21,
 fill = 'black',
 cex = 1
)

sub_plot_1_frame <- data.frame(
  Session = 80:106
)

sub_plot_1_frame$Session1 <- NA
sub_plot_1_frame$Session2 <- NA

set.seed(65535)

sub_plot_1_frame[1:8,  "Session1"] <- abs(rnorm(8,  0.025,  0.05))
sub_plot_1_frame[9:27, "Session1"] <- abs(rnorm(19, 0.12, 0.025))
sub_plot_1_frame[, "Session2"]     <- abs(rnorm(27, 0.025,  0.01))

plot(NULL,
     xlim = c(79, 107),
     ylim = c(-0.05, 0.35),
     ylab = "Prop. Total Bills",
     xlab = "Congress",
     frame.plot = FALSE,
     family = "sans",
     cex.lab = 1.25,
     las = 1,
     xaxt = "n",
     yaxt = "n"
)

box(bty = "l")

axis(1,
     labels = FALSE,
     las = 2,
     at = x_axis_ticks
)

x_axis_ticks <- seq(80, 105, by = 5)

axis(1,
     labels = str_th(x_axis_ticks),
     cex    = 1,
     at     = x_axis_ticks
)

axis(2,
     labels = c(0.0, 0.1, 0.2, 0.3),
     las    = 1,
     at     = c(0.0, 0.1, 0.2, 0.3)
)

lines(sub_plot_1_frame$Session,
      sub_plot_1_frame$Session1)

points(sub_plot_1_frame$Session,
       sub_plot_1_frame$Session1,
       bg = 'white',
       cex = 2,
       pch = 21)

lines(sub_plot_1_frame$Session,
      sub_plot_1_frame$Session2)

points(sub_plot_1_frame$Session,
       sub_plot_1_frame$Session2,
       bg = 'black',
       cex = 2,
       pch = 21)


sub_plot_2_frame <- data.frame(
  Session = 80:106
)

sub_plot_2_frame$Session1 <- NA
sub_plot_2_frame$Session2 <- NA

sub_plot_2_frame[, "Session1"] <- abs(rnorm(27, 0.4,  0.15))
sub_plot_2_frame[, "Session2"] <- abs(rnorm(27, 0.35,  0.15))

legend(
  "topleft",
  legend = c("Nov-Dec 1st Session",
             "Jan-Feb 2nd Session"),
  pch = c(21, 21),
  pt.cex = c(1.25, 1.25),
  bty = "n",
  cex = c(1.25, 1.25),
  pt.bg = c("white", "black")
)

plot(NULL,
     xlim = c(79, 107),
     ylim = c(-0.1, 1),
     ylab = "Index of Curvature",
     xlab = "Congress",
     frame.plot = FALSE,
     family = "sans",
     las = 1,
     cex.lab = 1.25,
     xaxt = "n",
     yaxt = "n"
)

box(bty = "l")

axis(1,
     labels = FALSE,
     las = 2,
     at = x_axis_ticks
)

x_axis_ticks <- seq(80, 105, by = 5)

axis(1,
     labels = str_th(x_axis_ticks),
     cex    = 1,
     at     = x_axis_ticks
)

axis(2,
     labels = c(0.0, 0.5, 1.0),
     las    = 1,
     at     = c(0.0, 0.5, 1.0)
)

lines(sub_plot_2_frame$Session,
      sub_plot_2_frame$Session1)

points(sub_plot_2_frame$Session,
       sub_plot_2_frame$Session1,
       bg = 'white',
       cex = 2,
       pch = 21)

lines(sub_plot_2_frame$Session,
      sub_plot_2_frame$Session2)

points(sub_plot_2_frame$Session,
       sub_plot_2_frame$Session2,
       bg = 'black',
       cex = 2,
       pch = 21)

legend(
  "topright",
  legend = c("By Session", "By Congress"),
  pch = c(21, 21),
  pt.cex = c(1.25, 1.25),
  bty = "n",
  cex = c(1.25, 1.25),
  pt.bg = c("white", "black")
)

dev.off()

#   scr_save(
#     name = "../man/figures/challenge_2.png",
#     format = "png",
#     res = 600,
#     height = 8,
#     width = 11
#   )

setwd(oldwd)
