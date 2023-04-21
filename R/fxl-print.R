#' print.fxl
#'
#' Override the final call to print the fxl object. catches the obj and
#' prints out layers in the sequence laid out by the user
#'
#' @param x fxl object
#' @param ... inherits from generic
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @export print.fxl
#'
#' @importFrom graphics layout
#'
#' @returns no return, executed for side effects
#'
#' @export
print.fxl <- function(x, ...) {
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
    x[["family"]]
  )

  if (!is.na(x[["layout"]]) &&
      !is.na(x[["layout_h"]]) &&
      !is.na(x[["layout_v"]])) {

    layout(x[["layout"]],
           x[["layout_h"]],
           x[["layout_v"]])

    par(
      family = font_family,
      omi = x[["dims"]][["omi"]],
      mai = x[["dims"]][["mai"]],
      xaxs = x[["dims"]][["xaxs"]],
      yaxs = x[["dims"]][["yaxs"]],
      xpd = NA
    )

  } else {
    par(
      mfrow = c(n_facets_draw, n_cols),
      family = font_family,
      omi = x[["dims"]][["omi"]],
      mai = x[["dims"]][["mai"]],
      xaxs = x[["dims"]][["xaxs"]],
      yaxs = x[["dims"]][["yaxs"]],
      xpd = NA
    )

  }

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

    # X overrides
    if (!is.null(x$dims[["global.min.x"]])) {
      x$dims[["min.local.x"]] <- x$dims[["global.min.x"]]
    }

    if (!is.null(x$dims[["global.max.x"]])) {
      x$dims[["max.local.x"]] <- x$dims[["global.max.x"]]
    }

    # X axes
    x_axis_draw <- (facetIndex == n_facets)

    if (!is.null(x$dims[["xdraws"]])) {
      x_axis_draw <- current_facet %in% x$dims[["xdraws"]]
    }

    # Note: Round UP, so as to scuttle the space near origin over
    x_axis_ticks <- seq(ceiling(x$dims[["global.min.x"]]),
      ceiling(x$dims[["global.max.x"]]),
      by = x$dims[["xdelta"]]
    )

    if (!is.null(x$dims[["xticks"]]) && !is.list(
      x$dims[["xticks"]]
    )) {
      x_axis_ticks <- as.integer(x$dims[["xticks"]])
    }

    if (!is.null(x$dims[["xticks"]]) && is.list(
      x$dims[["xticks"]]
    )) {
      x_axis_ticks <- x$dims[["xticks"]][[current_facet]]

      x$dims[["max.local.x"]] <- max(
        as.numeric(x$dims[["xticks"]][[current_facet]])
      )
    }

    # Y axes

    y_axis_draw <- NULL

    if (!is.null(x$dims[["ydraws"]])) {
      y_axis_draw <- current_facet %in% x$dims[["ydraws"]]
    }

    y_axis_ticks <- seq(ceiling(x$dims[["global.min.y"]]),
      ceiling(x$dims[["global.max.y"]]),
      by = x$dims[["ydelta"]]
    )

    y_axis_draw <- as.character(y_axis_ticks)

    if (!is.null(x$dims[["local.dims"]])) {
      x$dims[["min.local.y"]] <- x$dims[["local.dims"]][[current_facet]]$y0
      x$dims[["max.local.y"]] <- x$dims[["local.dims"]][[current_facet]]$y1

      y_axis_ticks <- seq(x$dims[["min.local.y"]],
        x$dims[["max.local.y"]],
        by = x$dims[["ydelta"]]
      )

      y_axis_draw <- as.character(y_axis_ticks)

      if ("yticks" %in% names(x$dims[["local.dims"]][[current_facet]])) {
        y_axis_ticks <- x$dims[["local.dims"]][[current_facet]][["yticks"]]
        y_axis_draw <- as.character(y_axis_ticks)
      }
    } else {
      x$dims[["min.local.y"]] <- ifelse(is.null(
        x$dims[["global.min.y"]]
      ),
      min(x$data[[as.character(x$aes["y"])]]),
      x$dims[["global.min.y"]]
      )

      x$dims[["max.local.y"]] <- ifelse(is.null(
        x$dims[["global.max.y"]]
      ),
      max(x$data[[as.character(x$aes["y"])]]),
      x$dims[["global.max.y"]]
      )

      y_axis_ticks <- seq(x$dims[["min.local.y"]],
        x$dims[["max.local.y"]],
        by = x$dims[["ydelta"]]
      )

      y_axis_draw <- as.character(y_axis_ticks)
    }

    if (!is.null(x$dims[["yticks"]]) && !is.list(
      x$dims[["yticks"]]
    )) {
      y_axis_ticks <- as.integer(x$dims[["yticks"]])

      y_axis_draw <- as.character(y_axis_ticks)
    }

    plot(NULL,
      xlim = c(
        x$dims[["min.local.x"]],
        x$dims[["max.local.x"]]
      ),
      ylim = c(
        x$dims[["min.local.y"]],
        x$dims[["max.local.y"]]
      ),
      ylab = "",
      xlab = "",
      frame.plot = FALSE,
      family = font_family,
      las = 1,
      xaxt = "n",
      yaxt = "n"
    )

    box(bty = "l")

    if (!is.null(x$dims[["xticklabs"]]) &&
      !is.list(x$dims[["xticklabs"]]) &&
      x_axis_draw) {
      x_axis_draw <- x$dims[["xticklabs"]]
    }

    if (!is.null(x$dims[["xticklabs"]]) && is.list(
      x$dims[["xticklabs"]]
    )) {
      x_axis_draw <- x$dims[["xticklabs"]][[current_facet]]
    }

    if (!is.null(x$dims[["yticklabs"]]) &&
      !is.list(x$dims[["yticklabs"]])) {
      y_axis_draw <- x$dims[["yticklabs"]]
    }

    x_lab_cex <- x$dims[["xlab.cex"]]

    if (!is.null(x$dims[["xlab.rotation"]]) &&
      !is.null(x$dims[["xlab.offset"]]) &&
      !is.null(x$dims[["xticklabs.offset"]])) {
      x_lab_rotation <- x$dims[["xlab.rotation"]]
      x_lab_offset <- x$dims[["xlab.offset"]]
      x_lab_adj <- x$dims[["xticklabs.offset"]]

      axis(1,
        labels = FALSE,
        las = 2,
        at = x_axis_ticks
      )

      ## Draw the x-axis labels.
      text(
        x = x_axis_ticks,
        y = par("usr")[3] - x_lab_offset,
        labels = x_axis_draw,
        xpd = NA,
        srt = x_lab_rotation,
        family = font_family,
        adj = x_lab_adj,
        cex = x_lab_cex
      )
    } else {
      axis(1,
        labels = x_axis_draw,
        cex    = x_lab_cex,
        at     = x_axis_ticks
      )
    }

    axis(2,
      labels = y_axis_draw,
      las    = 1,
      at     = y_axis_ticks
    )

    if (length(x[["layers"]]) > 0) {
      for (i in seq_len(length(x[["layers"]]))) {
        current_layer <- x$layers[[i]]

        if (current_layer$type == "arrows") {
          draw_arrows(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "brackets") {
          draw_brackets(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "bar_support") {
          draw_bar_support(
            x,
            current_layer,
            current_facet,
            x$dims[["max.local.y"]]
          )
        }

        if (current_layer$type == "cum_sum_lines") {
          draw_cumsum_lines(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "cum_sum_points") {
          draw_cumsum_points(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "facet_label") {
          draw_label_facet(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "guide_line") {
          draw_guide_line(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "line") {
          draw_lines(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "phase_label") {
          draw_label_phase(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "phase_lines") {
          draw_scr_plines(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "criterion_line") {
          draw_scr_criterion(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "point") {
          draw_points(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "image") {
          draw_images(
            x,
            current_layer,
            current_facet
          )
        }

        if (current_layer$type == "rectangle") {
          draw_rect(
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

            current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
              tmp_x1,
              x$dims[["max.local.y"]]
            )

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2
            )

            tmp_point_top_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["topDraw"]]$dev,
              input = "dev"
            )

            tmp_point_bot_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["botDraw"]]$dev,
              input = "dev"
            )

            segments(tmp_point_top_dev$usr$x,
              tmp_point_top_dev$usr$y,
              tmp_point_bot_dev$usr$x,
              tmp_point_bot_dev$usr$y,
              col = "black"
            )

            current_layer$lines[[pname]][[current_index]][["topDraw"]] <- cnvrt_coords(
              tmp_x1,
              tmp_y2
            )$dev

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2
            )$dev

            plot_tops[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
              tmp_x1,
              tmp_y1
            )

            plot_bots[[pname]][[index_num[[pname]]]] <- cnvrt_coords(
              tmp_x2,
              tmp_y2
            )

            index_num[[pname]] <- index_num[[pname]] + 1
          }

          req_draw <- TRUE
        }
      }
    }

    if (!is.null(x[["legendpars"]])) {
      if (lookup && x$legendpars[["panel"]] == current_facet) {
        draw_legend(x)
      } else if (lookup && is.na(x$legendpars[["panel"]])) {
        draw_legend(x)
      }
    }
  }

  # Note: final overlays, once facets are drawn/coords cached
  if (req_draw) {
    n_phase_lines <- unique(names(plot_bots))

    for (pl in seq_len(length(n_phase_lines))) {
      n_facets <- length(plot_tops[[n_phase_lines[pl]]])

      for (plfacet in 2:n_facets) {
        # pts.pre <- plot_tops[[n_phase_lines[pl]]][[plfacet - 1]]
        pbs_pre <- plot_bots[[n_phase_lines[pl]]][[plfacet - 1]]

        pts <- plot_tops[[n_phase_lines[pl]]][[plfacet]]
        pbs <- plot_bots[[n_phase_lines[pl]]][[plfacet]]

        # tmp.point.top.pre.dev <- cnvrt_coords(pts.pre$dev, input = "dev")
        tmp_point_bot_pre_dev <- cnvrt_coords(pbs_pre$dev, input = "dev")

        tmp_point_top_dev <- cnvrt_coords(pts$dev, input = "dev")
        tmp_point_bot_dev <- cnvrt_coords(pbs$dev, input = "dev")

        segments(tmp_point_bot_pre_dev$usr$x, tmp_point_bot_pre_dev$usr$y,
          tmp_point_bot_pre_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
            tmp_point_top_dev$usr$y) / 2,
          col = "black"
        )

        segments(tmp_point_bot_pre_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
          tmp_point_top_dev$usr$y) / 2,
        tmp_point_top_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
          tmp_point_top_dev$usr$y) / 2,
        col = "black"
        )

        segments(tmp_point_top_dev$usr$x, (tmp_point_bot_pre_dev$usr$y +
          tmp_point_top_dev$usr$y) / 2,
        tmp_point_top_dev$usr$x, tmp_point_top_dev$usr$y,
        col = "black"
        )
      }
    }
  }

  if (!lookup && !is.null(x[["legendpars"]])) draw_legend(x)

  mtext(x$labs[["title"]],
    side  = 3,
    cex   = x$labs[["title_cex"]],
    col   = x$labs[["title_color"]],
    adj   = x$labs[["title_adj"]],
    font  = x$labs[["title_face"]],
    outer = x$labs[["outer"]]
  )

  mtext(x$labs[["ylab"]],
    side  = 2,
    cex   = x$labs[["ylab_cex"]],
    col   = x$labs[["ylab_color"]],
    adj   = x$labs[["ylab_adj"]],
    font  = x$labs[["ylab_face"]],
    outer = x$labs[["outer"]],
    line  = x$labs[["outer.y.line"]]
  )

  mtext(x$labs[["xlab"]],
    side  = 1,
    cex   = x$labs[["xlab_cex"]],
    col   = x$labs[["xlab_color"]],
    adj   = x$labs[["xlab_adj"]],
    font  = x$labs[["xlab_face"]],
    outer = x$labs[["outer"]],
    line  = x$labs[["outer.x.line"]]
  )
}
