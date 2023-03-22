
library(fxl)

set.seed(65535)

custom_print <- function(x, ...) {

  message('custom print')

  # Holders for phase coords
  plot_tops <- list()
  plot_bots <- list()
  index_num <- list()

  facets <- NULL
  n_facets <- 1
  n_facets_draw <- n_facets
  n_cols <- 1
  lookup <- FALSE
  req_draw <- FALSE

  if ("facet" %in% names(x$aes)) {
    facets <- unique(x$data[[as.character(x$aes["facet"])]])
    n_facets <- length(facets)
    n_facets_draw <- n_facets
    n_cols <- x[["dims"]][["ncol"]]
    n_facets_draw <- as.integer(n_facets / n_cols)
    lookup <- TRUE
  }

  font_family <- ifelse(is.null(x[["family"]]),
                        "serif",
                        x[["family"]])

  set.par <- par(mfrow  = c(n_facets_draw, n_cols), # Dynamic facet numbers/cols,
      family = font_family,
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
      xaxs   = "r",
      yaxs   = "r",
      xpd    = FALSE)


  build_vector = numeric(0)
  current_id <- 1

  for (item in seq_len(n_facets_draw)) {
    build_vector <- c(
      build_vector,
      rep(current_id, 5)
    )

    current_id <- current_id + 1

    build_vector <- c(
      build_vector,
      current_id
    )

    current_id <- current_id + 1
  }

  # Set layouts
  layout(matrix(build_vector,
                nrow = 6 * n_facets_draw,
                ncol = 1,
                byrow = FALSE))

  for (facetIndex in 1:n_facets) {
    # Defaults, per data
    current_facet <- NA

    x$dims[["min.local.x"]] <- min(
      x$data[[as.character(x$aes["x"])]],
      na.rm = TRUE)
    x$dims[["max.local.x"]] <- max(
      x$data[[as.character(x$aes["x"])]],
      na.rm = TRUE)

    # Facet override
    if (lookup)  current_facet <- facets[facetIndex]

    x$dims[["min.local.x"]] <- min(
      x$data[[
        as.character(x$aes["x"])]],
      na.rm = TRUE)

    x$dims[["max.local.x"]] <- max(
      x$data[[
        as.character(x$aes["x"])]],
      na.rm = TRUE)

    # X Overrides

    if (!is.null(x$dims[["global.min.x"]]))
      x$dims[["min.local.x"]] <- x$dims[["global.min.x"]]

    if (!is.null(x$dims[["global.max.x"]]))
      x$dims[["max.local.x"]] <- x$dims[["global.max.x"]]

    # X axis
    x_axis_ticks <- seq(x$dims[["global.min.x"]],
                        x$dims[["global.max.x"]],
                        by = x$dims[["xdelta"]])

    if (!is.null(x$dims[["xticks"]]) && !is.list(x$dims[["xticks"]])) {
      x_axis_ticks <- as.integer(x$dims[["xticks"]])
    }

    # X axes
    x_axis_draw  <- TRUE

    # Y axis

    x$dims[["min.local.y"]] <- ifelse(is.null(x$dims[["global.min.y"]]),
                                      min(x$data[[as.character(x$aes["y"])]]),
                                      x$dims[["global.min.y"]])
    x$dims[["max.local.y"]] <- ifelse(is.null(x$dims[["global.min.y"]]),
                                      max(x$data[[as.character(x$aes["y"])]]),
                                      x$dims[["global.max.y"]])

    par(set.par, new = FALSE)

    # Top plot
    plot(NULL,
         ylim = c(x$dims[["min.local.y"]],
                  x$dims[["max.local.y"]]),
         xlim = c(x$dims[["min.local.x"]],
                  x$dims[["max.local.x"]]),
         ylab = "",
         xlab = "",
         xaxt = "n",
         yaxt = "n",
         frame.plot = FALSE,
         log = "y",
         las = 1)

    mtext(x$labs[["title"]],
          side = 3,
          outer = TRUE,
          adj = 0.5,
          line = 1)


    breaks  <- as.vector(c(2:10) %o% 10^(log10(x$dims[["min.local.y"]]):log10(x$dims[["max.local.y"]])))

    label_logicals <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
    labels <- as.character(breaks * label_logicals)
    labels <- gsub("^0$", "", labels)

    axis(1,
         at     = x_axis_ticks,
         labels = NA)

    axis(2,
         at = c(0.1,
                as.vector(c(1) %o%
                            10^(
                              log10(
                                x$dims[["min.local.y"]]):log10(
                                  x$dims[["max.local.y"]])))),
         las    = 1,
         tcl    = par("tcl"),
         labels = c(0.1,
                    as.vector(c(1) %o%
                                10^(
                                  log10(
                                    x$dims[["min.local.y"]]):log10(
                                      x$dims[["max.local.y"]])))))

    abline(h = c(0.1, breaks),
           lty = 1,
           col = "cadetblue")

    abline(h = c(0.1,
                 as.vector(c(1) %o%
                             10^(
                               log10(
                                 x$dims[["min.local.y"]]):log10(
                                   x$dims[["max.local.y"]])))),
           lty = 1,
           col = "cadetblue")

    abline(h = c(0.1,
                 as.vector(c(5) %o%
                             10^(
                               log10(
                                 x$dims[["min.local.y"]]):log10(
                                   x$dims[["max.local.y"]])))),
           lty = 3,
           col = "cadetblue")

    abline(v   = x_axis_ticks,
           lty = 1,
           col = "cadetblue")

    if (length(x[["layers"]]) > 0) {
      for (i in seq_len(length(x[["layers"]]))) {

        current_layer <- x$layers[[i]]

        if (current_layer$type == "arrows")      draw_arrows(x,
                                                             current_layer,
                                                             current_facet)
        if (current_layer$type == "brackets")    draw_brackets(x,
                                                               current_layer,
                                                               current_facet)
        if (current_layer$type == "guide_line")  draw_guide_line(x,
                                                                 current_layer,
                                                                 current_facet)
        if (current_layer$type == "line") {
          draw_lines(x, current_layer, current_facet)
        }

        if (current_layer$type == "phase_label") draw_label_phase(x,
                                                                  current_layer,
                                                                  current_facet)

        if (current_layer$type == "facet_label")
          draw_label_facet(x,
                           current_layer,
                           current_facet)

        if (current_layer$type == "point") {
          draw_points(x, current_layer, current_facet)
        }

        if (current_layer$type == "mbd_phase_lines") {

          plines <- names(current_layer$lines)

          for (pname in plines) {

            # start of index for named list
            if (!(pname %in% names(index_num))) index_num[[pname]] <- 1

            current_index <- which(
              names(current_layer$lines[[pname]]) ==  current_facet)

            if (length(current_index) == 0) next

            tmp_x1 <- current_layer$lines[[pname]][[current_index]][["x1"]]

            tmp_x2 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["x2"]]),
              tmp_x1,
              current_layer$lines[[pname]][[current_index]][["x2"]])

            tmp_y1 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["y1"]]),
              0,
              current_layer$lines[[pname]][[current_index]][["y1"]])

            tmp_y2 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["y2"]]),
              0,
              current_layer$lines[[pname]][[current_index]][["y2"]])

            current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
              tmp_x1,
              x$dims[["max.local.y"]])

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2)

            tmp_point_top_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["topDraw"]]$dev,
              input = "dev")

            tmp_point_bot_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["botDraw"]]$dev,
              input = "dev")

            #abline(v = tmp_point_top_dev$usr$x)

            segments(tmp_point_top_dev$usr$x,
                     tmp_point_top_dev$usr$y,
                     tmp_point_bot_dev$usr$x,
                     tmp_point_bot_dev$usr$y,
                     col = "black")

            current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
              tmp_x1,
              tmp_y2)$dev

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2)$dev

            plot_tops[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
              tmp_x1,
              tmp_y1)

            plot_bots[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2)

            print(paste(pname,
                        'x1:', tmp_x1,
                        'x2:', tmp_x2,
                        'y1:', tmp_y1,
                        'y2:', tmp_y2))

            index_num[[pname]] <- index_num[[pname]] + 1
          }

          req_draw <- TRUE
        }
      }
    }

    box(bty = "l")

    if (!is.null(x$dims[["xticklabs"]]) &&
        !is.list(x$dims[["xticklabs"]]) &&
        x_axis_draw) {

      x_axis_draw <- x$dims[["xticklabs"]]
    }

    #if (!is.null(x[["legendpars"]]))  draw_legend(x)

    plot(NULL,
         ylim = c(0, 0),
         xlim = c(x$dims[["min.local.x"]],
                  x$dims[["max.local.x"]]),
         ylab = "",
         xlab = "",
         xaxt = "n",
         yaxt = "n",
         frame.plot = FALSE,
         las = 1)

    x_labels_holder <- x_axis_draw

    if (facetIndex != n_facets) {
      x_labels_holder <- NA
    }

    axis(1,
         labels = x_labels_holder,
         at     = x_axis_ticks,
         pos = 0)

    axis(2,
         labels = c(0),
         las    = 1,
         tcl    = 0,
         at     = c(0))

    abline(h = 0,
           lty = 1,
           col = "black")

    if (length(x[["layers"]]) > 0)
      for (i in seq_len(length(x[["layers"]])))
        if (x$layers[[i]]$type == "point")
          draw_points(x, x$layers[[i]], NA, zero_axis = TRUE)

        if (current_layer$type == "mbd_phase_lines") {
          plines <- names(current_layer$lines)

          tmp_x1 <- current_layer$lines[[pname]][[current_index]][["x1"]]

          abline(v = tmp_x1,
                 lty = 1,
                 col = 'black')
#
#           for (pname in plines) {
#
#             # start of index for named list
#             if (!(pname %in% names(index_num))) index_num[[pname]] <- 1
#
#             current_index <- which(
#               names(current_layer$lines[[pname]]) ==  current_facet)
#
#             if (length(current_index) == 0) next
#
#             tmp_x1 <- current_layer$lines[[pname]][[current_index]][["x1"]]
#
#             tmp_x2 <- ifelse(is.null(
#               current_layer$lines[[pname]][[current_index]][["x2"]]),
#               tmp_x1,
#               current_layer$lines[[pname]][[current_index]][["x2"]])
#
#             # tmp_y1 <- 0
#             # tmp_y2 <- -5
#             tmp_y1 <- 0
#
#             tmp_y2 <- ifelse(is.null(
#               current_layer$lines[[pname]][[current_index]][["y2"]]),
#               0,
#               current_layer$lines[[pname]][[current_index]][["y2"]])
#
#             current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
#               tmp_x1,
#               0)
#
#             current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
#               tmp_x2,
#               tmp_y2)
#
#             tmp_point_top_dev <- cnvrt_coords(
#               current_layer$lines[[pname]][[current_index]][["topDraw"]]$dev,
#               input = "dev")
#
#             tmp_point_bot_dev <- cnvrt_coords(
#               current_layer$lines[[pname]][[current_index]][["botDraw"]]$dev,
#               input = "dev")
#
#             segments(tmp_point_top_dev$usr$x,
#                      tmp_point_top_dev$usr$y,
#                      tmp_point_bot_dev$usr$x,
#                      tmp_point_bot_dev$usr$y,
#                      col = "black")
#
#             current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
#               tmp_x1,
#               tmp_y2)$dev
#
#             current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
#               tmp_x2,
#               tmp_y2)$dev
#
#             plot_tops[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
#               tmp_x1,
#               tmp_y1)
#
#             plot_bots[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
#               tmp_x2,
#               tmp_y2)
#
#             index_num[[pname]] <- index_num[[pname]] + 1
#           }

          req_draw <- TRUE
        }

  }

  mtext(x$labs[["ylab"]],
        side = 2,
        outer = TRUE)

  mtext(x$labs[["xlab"]],
        side = 1,
        line = 2,
        outer = TRUE)

  # Note: final overlays, once facets are drawn/coords cached
  if (req_draw) {

    print(plot_tops)
    print(plot_bots)

    n_phase_lines <- unique(names(plot_bots))

    for (pl in seq_len(length(n_phase_lines))) {

      n_facets <- length(plot_tops[[n_phase_lines[pl]]])

      for (plfacet in 2:n_facets) {

        #pts.pre <- plot_tops[[n_phase_lines[pl]]][[plfacet - 1]]
        pbs_pre <- plot_bots[[n_phase_lines[pl]]][[plfacet - 1]]

        pts <- plot_tops[[n_phase_lines[pl]]][[plfacet]]
        pbs <- plot_bots[[n_phase_lines[pl]]][[plfacet]]

        #tmp.point.top.pre.dev <- cnvrt_coords(pts.pre$dev, input = "dev")
        tmp_point_bot_pre_dev <- cnvrt_coords(pbs_pre$dev, input = "dev")

        tmp_point_top_dev <- cnvrt_coords(pts$dev, input = "dev")
        tmp_point_bot_dev <- cnvrt_coords(pbs$dev, input = "dev")

        segments(tmp_point_bot_pre_dev$usr$x, tmp_point_bot_pre_dev$usr$y,
                 tmp_point_bot_pre_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                                 tmp_point_top_dev$usr$y) / 2,
                 col = "black")

        segments(tmp_point_bot_pre_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                                 tmp_point_top_dev$usr$y) / 2,
                 tmp_point_top_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                             tmp_point_top_dev$usr$y) / 2,
                 col = "black")

        segments(tmp_point_top_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
                                             tmp_point_top_dev$usr$y) / 2,
                 tmp_point_top_dev$usr$x, tmp_point_top_dev$usr$y,
                 col = "black")
      }
    }
  }

}

student_count = 8
class_count = 3
grade_count = 1
run_time = 8

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

bl_starts_value <- runif(student_count * class_count * grade_count, 0, 6)
bl_grd_growth   <- c(0, 5, 10)
bl_rates_value  <- runif(student_count * class_count * grade_count, 0, 1.25)
bl_var_value    <- rnorm(student_count * class_count * grade_count, 6, 1.5)
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

data_frame[data_frame$Value < 1, "Value"] <- 0

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
  scr_title("Semi-log Chart: Grade-level Acquisition in Schools") |>
  scr_xlabel("Weeks of Classroom Instruction") |>
  scr_ylabel("Oral Reading Fluency") |>
  scr_yoverride(c(1, 100)) |>
  scr_lines(
    color = "#000005",
    size = 0.5
  ) |>
  scr_points(
    pch = 21,
    fill = "white",
    cex = 2
  ) |>
  scr_label_facet(
    cex = 1.5,
    adj = 1,
    y = 0.2,
    x = 8,
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
  scr_plines_mbd(
    lines = list(# plot linked phase lines (note: drawn from top through bottom)
      "A" = list(
        "1" = list(
          x1 = 2.5,
          y1 = 100,
          y2 = 0.1
        ),
        "2" = list(
          x1 = 4.5,
          y1 = 100,
          y2 = 0.1
        ),
        "3" = list(
          x1 = 7.5,
          y1 = 100,
          y2 = 0.1
        )
      )
    )
  ) |>
  custom_print()
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
