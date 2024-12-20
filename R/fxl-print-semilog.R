#' print.fxlsemilog
#'
#' Override the final call to print the fxl object.
#' catches the obj and prints out layers in the sequence laid out by the user
#'
#' @param x fxlsemilog object
#' @param ... inherits from generic
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export print.fxlsemilog
#'
#' @returns no return, executed for side effects
#'
#' @export
print.fxlsemilog <- function(x, ...) {
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

  opar <- par(no.readonly = TRUE)
  on.exit(par(opar))

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
    x[["family"]]
  )

  par(
    mfrow = c(n_facets_draw, n_cols), # Dynamic facet numbers/cols,
    family = font_family,
    omi = x[["dims"]][["omi"]],
    mai = x[["dims"]][["mai"]],
    xaxs = "r",
    yaxs = "r",
    xpd = FALSE
  )

  build_vector <- numeric(0)
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
    byrow = FALSE
  ))

  for (facetIndex in 1:n_facets) {
    # Defaults, per data
    current_facet <- NA

    x$dims[["min.local.x"]] <- min(
      x$data[[as.character(x$aes["x"])]],
      na.rm = TRUE
    )
    x$dims[["max.local.x"]] <- max(
      x$data[[as.character(x$aes["x"])]],
      na.rm = TRUE
    )

    # Facet override
    if (lookup) current_facet <- facets[facetIndex]

    x$dims[["min.local.x"]] <- min(
      x$data[[
        as.character(x$aes["x"])
      ]],
      na.rm = TRUE
    )

    x$dims[["max.local.x"]] <- max(
      x$data[[
        as.character(x$aes["x"])
      ]],
      na.rm = TRUE
    )

    # X Overrides

    if (!is.null(x$dims[["global.min.x"]])) {
      x$dims[["min.local.x"]] <- x$dims[["global.min.x"]]
    }

    if (!is.null(x$dims[["global.max.x"]])) {
      x$dims[["max.local.x"]] <- x$dims[["global.max.x"]]
    }

    # X axis
    x_axis_ticks <- seq(x$dims[["global.min.x"]],
      x$dims[["global.max.x"]],
      by = x$dims[["xdelta"]]
    )

    if (!is.null(x$dims[["xticks"]]) && !is.list(x$dims[["xticks"]])) {
      x_axis_ticks <- as.integer(x$dims[["xticks"]])
    }

    # X axes
    x_axis_draw <- TRUE

    # Y axis

    x$dims[["min.local.y"]] <- ifelse(is.null(x$dims[["global.min.y"]]),
      min(x$data[[as.character(x$aes["y"])]]),
      x$dims[["global.min.y"]]
    )
    x$dims[["max.local.y"]] <- ifelse(is.null(x$dims[["global.min.y"]]),
      max(x$data[[as.character(x$aes["y"])]]),
      x$dims[["global.max.y"]]
    )

    # Top plot
    plot(NULL,
      ylim = c(
        x$dims[["min.local.y"]],
        x$dims[["max.local.y"]]
      ),
      xlim = c(
        x$dims[["min.local.x"]],
        x$dims[["max.local.x"]]
      ),
      ylab = "",
      xlab = "",
      xaxt = "n",
      yaxt = "n",
      frame.plot = FALSE,
      log = "y",
      las = 1
    )

    if (facetIndex == 1) {
      title_color <- x$labs[["title_color"]]
      title_cex <- x$labs[["title_cex"]]
      title_adj <- x$labs[["title_adj"]]

      mtext(x$labs[["title"]],
        side  = 3,
        outer = TRUE,
        col   = title_color,
        adj   = title_adj,
        cex   = title_cex,
        line  = 1
      )
    }

    breaks <- as.vector(c(2:10) %o% 10^(log10(x$dims[["min.local.y"]]):log10(x$dims[["max.local.y"]])))

    label_logicals <- c(TRUE, FALSE, FALSE, TRUE, FALSE, FALSE, FALSE, FALSE, TRUE)
    labels <- as.character(breaks * label_logicals)
    labels <- gsub("^0$", "", labels)

    axis(1,
      at       = x_axis_ticks,
      cex.axis = x$dims[["xlab.cex"]],
      labels   = NA
    )

    local_y_ticks_major <-  c(
      0.1,
      as.vector(c(1) %o%
                  10^(
                    log10(
                      x$dims[["min.local.y"]]
                    ):log10(
                      x$dims[["max.local.y"]]
                    )))
    )

    if (!is.null(x$dims[["yticks"]]) && !is.list(
      x$dims[["yticks"]]
    )) {

      local_y_ticks_major <- as.integer(x$dims[["yticks"]])
    }

    local_y_ticks_major_labs <- as.character(local_y_ticks_major)

    if (!is.null(x$dims[["ytickslabs"]]) && !is.list(
      x$dims[["ytickslabs"]]
    )) {

      local_y_ticks_major_labs <- as.character(x$dims[["ytickslabs"]])
    }

    axis(2,
      at = local_y_ticks_major,
      las = 1,
      tcl = par("tcl"),
      labels = local_y_ticks_major_labs,
      cex.axis = x$dims[["ylab.cex"]]
    )

    abline(
      h = c(0.1, breaks),
      lty = 1,
      col = x[["semi_color_minor_y"]]
    )

    abline(
      h = local_y_ticks_major,
      lty = 1,
      col = x[["semi_color_major_y"]]
    )

    abline(
      h = c(
        0.1,
        as.vector(c(5) %o%
          10^(
            log10(
              x$dims[["min.local.y"]]
            ):log10(
              x$dims[["max.local.y"]]
            )))
      ),
      lty = 3,
      col = x[["semi_color_midpoint_y"]]
    )

    abline(
      v = x_axis_ticks,
      lty = 1,
      col = x[["semi_color_major_x"]]
    )

    if (length(x[["layers"]]) > 0) {
      for (i in seq_len(length(x[["layers"]]))) {
        current_layer <- x$layers[[i]]

        if (is.na(current_facet)) {
          current_layer$facet <- "hack"
          current_facet <- NA
        }

        if (current_layer$type == "arrows") {
          draw_arrows(x, current_layer, current_facet)
        }

        if (current_layer$type == "brackets") {
          draw_brackets(x, current_layer, current_facet)
        }

        if (current_layer$type == "guide_line") {
          draw_guide_line(x, current_layer, current_facet)
        }

        if (current_layer$type == "line") {
          draw_lines(x, current_layer, current_facet)
        }

        if (current_layer$type == "phase_label") {
          draw_label_phase(x, current_layer, current_facet)
        }

        if (current_layer$type == "phase_lines") {
          draw_scr_plines(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "facet_label") {
          draw_label_facet(x, current_layer, current_facet)
        }

        if (current_layer$type == "criterion_line") {
          draw_scr_criterion(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "point") {
          draw_points(x, current_layer, current_facet)
        }

        if (current_layer$type == "criterion_line") {
          scr_criterion_lines(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "mbd_phase_lines") {
          plines <- names(current_layer$lines)

          for (pname in plines) {
            # start of index for named list
            if (!(pname %in% names(index_num))) index_num[[pname]] <- 1

            current_index <- which(
              names(current_layer$lines[[pname]]) == current_facet
            )

            if (length(current_index) == 0) next

            tmp_x1 <- current_layer$lines[[pname]][[current_index]][["x1"]]

            tmp_x2 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["x2"]]
            ),
            tmp_x1,
            current_layer$lines[[pname]][[current_index]][["x2"]]
            )

            tmp_y1 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["y1"]]
            ),
            0,
            current_layer$lines[[pname]][[current_index]][["y1"]]
            )

            tmp_y2 <- ifelse(is.null(
              current_layer$lines[[pname]][[current_index]][["y2"]]
            ),
            0,
            current_layer$lines[[pname]][[current_index]][["y2"]]
            )

            segments(tmp_x1,
              tmp_y1,
              tmp_x2,
              tmp_y2,
              col = "black"
            )

            index_num[[pname]] <- index_num[[pname]] + 1

            if (current_index > 1) {
              pre_tmp_x1 <- ifelse(is.null(
                current_layer$lines[[pname]][[current_index - 1]][["x1"]]
              ),
              0,
              current_layer$lines[[pname]][[current_index - 1]][["x1"]]
              )

              segments(tmp_x1,
                tmp_y1,
                tmp_x2,
                tmp_y2,
                col = "black"
              )

              segments(pre_tmp_x1,
                tmp_y1,
                tmp_x1,
                tmp_y1,
                col = "black"
              )

              segments(pre_tmp_x1,
                tmp_y1,
                pre_tmp_x1,
                tmp_y1 * 10,
                col = "black"
              )
            }
          }
        }
      }
    }

    box(bty = x[["bty"]])

    if (!is.null(x$dims[["xticklabs"]]) &&
      !is.list(x$dims[["xticklabs"]]) &&
      x_axis_draw) {
      x_axis_draw <- x$dims[["xticklabs"]]
    }

    if (!is.null(x[["legendpars"]])) {
      if (lookup && x$legendpars[["panel"]] == current_facet) {
        draw_legend(x)
      } else if (is.na(x$legendpars[["panel"]])) {
        draw_legend(x)
      }
    }

    plot(NULL,
      ylim = c(0, 0),
      xlim = c(
        x$dims[["min.local.x"]],
        x$dims[["max.local.x"]]
      ),
      ylab = "",
      xlab = "",
      xaxt = "n",
      yaxt = "n",
      frame.plot = FALSE,
      las = 1
    )

    x_labels_holder <- x_axis_draw

    if (facetIndex != n_facets) {
      x_labels_holder <- NA
    }

    axis(1,
      labels = x_labels_holder,
      at     = x_axis_ticks,
      cex.axis = x$dims[["xlab.cex"]],
      pos = 0
    )

    axis(2,
      labels = c(0),
      las    = 1,
      tcl    = 0,
      at     = c(0),
      cex.axis = x$dims[["ylab.cex"]]
    )

    abline(
      h = 0,
      lty = 1,
      col = "black"
    )

    if (length(x[["layers"]]) > 0) {
      for (i in seq_len(length(x[["layers"]]))) {
        current_layer <- x$layers[[i]]

        if (is.na(current_facet)) {
          current_layer$facet <- "hack"
          current_facet <- NA
        }

        if (x$layers[[i]]$type == "point") {
          draw_points(x, current_layer, current_facet, zero_axis = TRUE)
        }

        if (current_layer$type == "phase_lines") {
          draw_scr_plines(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "mbd_phase_lines") {
          tmp_x1 <- current_layer$lines[[pname]][[current_index]][["x1"]]

          if (current_index == length(current_layer$lines[[pname]])) {
            lines(x = c(tmp_x1, tmp_x1),
                  y = c(0, par('usr')[4]),
                  lty = 1,
                  col = "black")
          } else {
            abline(
              v = tmp_x1,
              lty = 1,
              col = "black"
            )
          }
        }
      }
    }
  }

  xlab_color <- x$labs[["xlab_color"]]
  xlab_cex <- x$labs[["xlab_cex"]]
  xlab_adj <- x$labs[["xlab_adj"]]
  xlab_face <- x$labs[["xlab_face"]]

  ylab_color <- x$labs[["ylab_color"]]
  ylab_cex <- x$labs[["ylab_cex"]]
  ylab_adj <- x$labs[["ylab_adj"]]
  ylab_face <- x$labs[["ylab_face"]]

  mtext(x$labs[["ylab"]],
    side  = 2,
    line  = 1,
    cex   = ylab_cex,
    col   = ylab_color,
    adj   = ylab_adj,
    font  = ylab_face,
    outer = TRUE
  )

  mtext(x$labs[["xlab"]],
    side  = 1,
    line  = 2,
    cex   = xlab_cex,
    col   = xlab_color,
    adj   = xlab_adj,
    font  = xlab_face,
    outer = TRUE
  )
}
