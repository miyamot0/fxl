library(fxl)

if ('here' %in% installed.packages()) {
  setwd(paste(here::here("demo")))
}

set.seed(65535)

student_count = 8
class_count = 3
grade_count = 1
run_time = 24

data_frame <- data.frame(
  Student = numeric(student_count * class_count * grade_count),
  Classroom = numeric(student_count * class_count * grade_count),
  Grade = numeric(student_count * class_count * grade_count),
  Time = numeric(student_count * class_count * grade_count),
  Baseline = numeric(student_count * class_count * grade_count),
  Lx = numeric(student_count * class_count * grade_count),
  Value = numeric(student_count * class_count * grade_count),
  OTRs = numeric(student_count * class_count * grade_count),
  Var = numeric(student_count * class_count * grade_count)
)

intervention_starts <- c(4, 8, 12)

bl_starts_value <- runif(student_count * class_count * grade_count, 0, 8)
bl_grd_growth   <- c(0, 5, 10)
bl_rates_value  <- runif(student_count * class_count * grade_count, 0, 0.725)
bl_var_value    <- rnorm(student_count * class_count * grade_count, 4, 1.25)
bl_otrs_value   <- runif(class_count, 5, 10)

row_number   <- 1
class_number <- 1

for (grade in seq_len(grade_count)) {

  grd_adjustment <- (grade - 1) * bl_grd_growth[grade]

  for (class in seq_len(class_count)) {

    otr_value <- bl_otrs_value[class]

    for (student in seq_len(student_count)) {

      bl_value  <- bl_starts_value[row_number] + grd_adjustment
      lx_value  <- bl_rates_value[row_number]
      var_value <- bl_var_value[row_number]

      data_frame[row_number, "Student"]   = row_number
      data_frame[row_number, "Classroom"] = class_number
      data_frame[row_number, "Grade"]     = grade
      data_frame[row_number, "Time"]      = 0
      data_frame[row_number, "Baseline"]  = bl_value
      data_frame[row_number, "Lx"]        = lx_value
      data_frame[row_number, "Value"]     = bl_value
      data_frame[row_number, "OTRs"]      = otr_value
      data_frame[row_number, "Var"]       = var_value

      row_number <- row_number + 1
    }

    class_number <- class_number + 1
  }
}

for (student in seq_len(row_number - 1)) {

  student_og  <- data_frame[data_frame[["Student"]] == student & data_frame[["Time"]] == 0,]

  student_lx   <- student_og[1, "Lx"]
  student_bl   <- student_og[1, "Baseline"]
  student_otr  <- student_og[1, "OTRs"]
  student_var  <- student_og[1, "Var"]
  student_cls  <- student_og[1, "Classroom"]
  student_grd  <- student_og[1, "Grade"]

  noise_error  <- rnorm(run_time, 0, student_var)

  for (time in seq_len(run_time)) {

    time_score <- time - intervention_starts[student_cls]
    time_sign  <- ifelse(time_score > 0, 1, 0)

    yhat <- student_bl + time_score * student_lx * student_otr * time_sign + noise_error[time]

    new_data <- data.frame(
      Student = student,
      Classroom = student_cls,
      Grade = student_grd,
      Time = time,
      Baseline = student_bl,
      Lx = student_lx,
      Value = yhat,
      OTRs = student_otr,
      Var = student_var
    )

    data_frame = rbind(
      data_frame,
      new_data
    )

  }
}

data_frame$ClassName <- paste0("Classroom #", data_frame$Classroom)
data_frame$GradeName <- paste0("Grade ", data_frame$Grade)

data_frame$Phase <- "Baseline"

data_frame[data_frame$Classroom == 1 & data_frame$Time > intervention_starts[1], "Phase"] <- "Intervention"
data_frame[data_frame$Classroom == 2 & data_frame$Time > intervention_starts[2], "Phase"] <- "Intervention"
data_frame[data_frame$Classroom == 3 & data_frame$Time > intervention_starts[3], "Phase"] <- "Intervention"

data_frame <- data_frame[data_frame$Time > 0, ]
data_frame$Time  <- data_frame$Time + rnorm(nrow(data_frame), 0, .125)
data_frame$Value <- data_frame$Value * (1 + rnorm(nrow(data_frame), 0, .01))

data_frame[data_frame$Value < 1, "Value"] <- 0

point_styler <- function(data_frame, ...) {
  input_list <- list(...)
  local_frame <- data_frame

  local_frame[local_frame$Y > 40, 'col'] <- 'green'
  local_frame[local_frame$Y < 40 & local_frame$Y > 10, 'col'] <- 'orange'
  local_frame[local_frame$Y < 11, 'col'] <- 'red'

  local_frame$cex <- 2 + local_frame$Y / 40

  points(local_frame$X, local_frame$Y,
         pch = input_list[["pch"]],
         cex = local_frame$cex,
         bg  = local_frame$col,
         col = input_list[["col"]])
}

scr_plot(
  data_frame,
  aesthetics = var_map(
    x = Time,
    y = Value,
    p = Phase,
    g = Student,
    facet = Classroom
  ),
  omi = c(
    0.5,
    0.3,
    0.5,
    0.25
  ),
  mai = c(
    0.0,
    0.5,
    0,
    0.25
  ),
  semilog = TRUE
) |>
  scr_title("Semi-log Chart: Grade-level Acquisition in Schools") |>
  scr_xlabel("Weeks of Classroom Instruction") |>
  scr_ylabel("Oral Reading Fluency") |>
  scr_yoverride(c(1, 100)) |>
  scr_xoverride(c(1, 24)) |>
  scr_lines(
    color = "#000005",
    size = 0.5
  ) |>
  scr_points(
    pch = 21,
    fill = "white",
    cex = 2.5,
    styler = point_styler
  ) |>
  scr_label_facet(
    cex = 1.5,
    adj = 1,
    y = 1.35,
    x = 24,
    labels = list(
      "1" = list(
        label = "Classroom #1"
      ),
      "2" = list(
        label = "Classroom #2"
      ),
      "3" = list(
        label = "Classroom #3"
      )
    )
  ) |>
  scr_label_phase(
    cex = 1.5,
    adj = 0.5,
    y = 78,
    facet = '1',
    labels = list(
      "Baseline" = list(
        x = 2.5
      ),
      "Intervention" = list(
        x = 7.5
      )
    )
  ) |>
  scr_plines_mbd(
    lines = list(# plot linked phase lines (note: drawn from top through bottom)
      "A" = list(
        "1" = list(
          x1 = 4.5,
          y1 = 110,
          y2 = 0.1
        ),
        "2" = list(
          x1 = 8.5,
          y1 = 110,
          y2 = 0.1
        ),
        "3" = list(
          x1 = 12.5,
          y1 = 110,
          y2 = 0.1
        )
      )
    )
  ) |>
  scr_legend(
    position = list(
      x = 0.5,
      y = 105
    ),
    panel = '3',
    legend = c(
      "On Target Reader",
      "Emerging Reader",
      "At-Risk Reader"
    ),
    col = c(
      "black",
      "black",
      "black"
    ),
    pt_bg = c(
      "green",
      "orange",
      "red"
    ),
    bg = "white",
    lty = c(
      1,
      1,
      1
    ), # line types (ordered)
    pch = c(
      21,
      21,
      21
    ), # marker types (ordered)
    bty = "n", # remove border
    pt_cex = 2.25, # point size scale
    cex = 1.5, # text size scale
    text_col = "black", # text color
    horiz = FALSE, # list items vertically
    box_lty = 1
  ) |>
  scr_save(
    name = "../man/figures/celeration_classwide.svg",
    format = "svg",
    units = "in",
    width = 9,
    height = 7.5
  ) |>
  scr_save(
    name = "../man/figures/celeration_classwide.png",
    format = "png",
    units = "in",
    width = 9,
    height = 7.5
  )
