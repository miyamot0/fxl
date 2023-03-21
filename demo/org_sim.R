
library(fxl)

set.seed(65535)

student_count = 30
class_count = 3
grade_count = 1
run_time = 5

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

bl_starts_value <- runif(student_count * class_count * grade_count, 0, 10)
bl_grd_growth   <- runif(grade_count, 0, 15)
bl_rates_value  <- runif(student_count * class_count * grade_count, 0, 1.25)
bl_var_value    <- rnorm(student_count * class_count * grade_count, 6, 2.5)
bl_otrs_value   <- runif(class_count, 15, 25)

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
    yhat <- student_bl + time * student_lx * student_otr + noise_error[time]

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

data_frame

data_frame$ClassName <- paste0("Classroom #", data_frame$Classroom)
data_frame$GradeName <- paste0("Grade ", data_frame$Grade)

library(fxl)

scr_plot(
  data_frame,
  aesthetics = var_map(
    x = Time,
    y = Value,
    p = Student,
    facet = Classroom
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
  scr_yoverride(c(0.1, 1000)) |>
  scr_title("Semi-log Chart: Grade-level Acquisition in Schools") |>
  scr_xlabel("Weeks of Classroom Instruction") |>
  scr_ylabel("Oral Reading Fluency") |>
  scr_lines(
    color = "#000005",
    size = 0.5
  ) |>
  scr_points(
    pch = 21,
    fill = "white",
    cex = 2
  )
#|>
  # scr_lines(
  #   mapping = list(
  #     x = Session,
  #     y = SkillB
  #   )
  # ) |>
  # scr_points(
  #   pch = 21,
  #   fill = "gray",
  #   cex = 2,
  #   mapping = list(
  #     x = Session,
  #     y = SkillB
  #   )
  # ) |>
  # scr_label_phase(
  #   cex = 1,
  #   adj = 0.5,
  #   labels = list(
  #     "Annotated Labels on Plot" = list(
  #       x = 5,
  #       y = 150
  #     ),
  #     "Labels for Phase Changes" = list(
  #       x = 18,
  #       y = 15
  #     ),
  #     "Logarithmic Plotting, With Zero Visualizations" = list(
  #       x = 6,
  #       y = 3,
  #       cex = 1
  #     )
  #   )
  # ) |>
  # scr_arrows(
  #   length = 0.1,
  #   arrows = list(
  #     "A" = list(
  #       x0 = 5,
  #       x1 = 7,
  #       y0 = 125,
  #       y1 = 50
  #     ),
  #     "B" = list(
  #       x0 = 18,
  #       x1 = 14,
  #       y0 = 21,
  #       y1 = 55
  #     )
  #   )
  # ) |>
  # scr_brackets(
  #   length = 0.1,
  #   brackets = list(
  #     "A" = list(
  #       x0 = 1.5,
  #       x1 = 10.5,
  #       y0 = 2.5,
  #       y1 = 1.5
  #     )
  #   )
  # ) |>
  # scr_guide_line(
  #   color = "black",
  #   lty = 1,
  #   coords = list(
  #     "A" = list(
  #       x0 = 9.5,
  #       x1 = 9.5,
  #       y0 = 10,
  #       y1 = 500
  #     ),
  #     "B" = list(
  #       x0 = 1,
  #       x1 = 9,
  #       y0 = 8,
  #       y1 = 50,
  #       col = "green",
  #       lty = 2,
  #       lwd = 3
  #     ),
  #     "C" = list(
  #       x0 = 10,
  #       x1 = 30,
  #       y0 = 50,
  #       y1 = 150,
  #       col = "orange",
  #       lty = 2,
  #       lwd = 3)
  #   )
  # ) |>
  # scr_legend(
  #   position = "topright", # Specify legend location
  #   legend = c(
  #     "Skill A", # labels to include (ordered)
  #     "Skill B"
  #   ),
  #   col = c(
  #     "black", # color of markers (ordered)
  #     "black"
  #   ),
  #   pt_bg = c(
  #     "black", # color of markers (ordered)
  #     "gray"
  #   ),
  #   bg = "white",
  #   lty = c(
  #     1,
  #     1
  #   ), # line types (ordered)
  #   pch = c(
  #     21,
  #     21
  #   ), # marker types (ordered)
  #   bty = "y", # remove border
  #   pt_cex = 2.25, # point size scale
  #   cex = 1.5, # text size scale
  #   text_col = "black", # text color
  #   horiz = FALSE, # list items vertically
  #   box_lty = 1
  # )
