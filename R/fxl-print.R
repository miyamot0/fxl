#' print.fxl
#'
#' Override the final call to print the fxl object. catches the obj and
#' prints out layers in the sequence laid out by the user
#'
#' @param core_frame fxl object
#'
#' @author Shawn Gilroy <sgilroy1@@lsu.edu>
#'
#' @return
#' @export print.fxl
#' @export
print.fxl <- function(core_frame, ...) {

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

  if ("facet" %in% names(core_frame$aes)) {
    facets <- unique(core_frame$data[[as.character(core_frame$aes["facet"])]])
    n_facets <- length(facets)
    n_facets_draw <- n_facets
    n_cols <- core_frame[["dims"]][["ncol"]]
    n_facets_draw <- as.integer(n_facets / n_cols)
    lookup <- TRUE
  }

  font_family <- ifelse(is.null(core_frame[["family"]]),
                        "serif",
                        core_frame[["family"]])

  par(mfrow  = c(n_facets_draw, n_cols), # Dynamic facet numbers/cols
      family = font_family,
      omi    = core_frame[["dims"]][["omi"]],
      mai    = core_frame[["dims"]][["mai"]],
      xaxs   = core_frame[["dims"]][["xaxs"]],
      yaxs   = core_frame[["dims"]][["yaxs"]],
      xpd    = NA)

  for (facetIndex in 1:n_facets) { # Print placeholders

    # Defaults, per data
    current_facet <- NA

    core_frame$dims[["min.local.x"]] <- min(
      core_frame$data[[as.character(core_frame$aes["x"])]],
      na.rm = TRUE)
    core_frame$dims[["max.local.x"]] <- max(
      core_frame$data[[as.character(core_frame$aes["x"])]],
      na.rm = TRUE)

    # Facet override
    if (lookup)  current_facet <- facets[facetIndex]

    # X overrides
    if (!is.null(core_frame$dims[["global.min.x"]]))
      core_frame$dims[["min.local.x"]] <- core_frame$dims[["global.min.x"]]

    if (!is.null(core_frame$dims[["global.max.x"]]))
      core_frame$dims[["max.local.x"]] <- core_frame$dims[["global.max.x"]]

    # X axes
    x_axis_draw  <- (facetIndex == n_facets)

    if (!is.null(core_frame$dims[["xdraws"]])) {
      x_axis_draw <-  current_facet %in% core_frame$dims[["xdraws"]]
    }

    # Note: Round UP, so as to scuttle the space near origin over
    x_axis_ticks <- seq(ceiling(core_frame$dims[["global.min.x"]]),
                        ceiling(core_frame$dims[["global.max.x"]]),
                        by = core_frame$dims[["xdelta"]])

    if (!is.null(core_frame$dims[["xticks"]]) && !is.list(
      core_frame$dims[["xticks"]])) {

      x_axis_ticks <- as.integer(core_frame$dims[["xticks"]])
    }

    if (!is.null(core_frame$dims[["xticks"]]) && is.list(
      core_frame$dims[["xticks"]])) {

      x_axis_ticks <- core_frame$dims[["xticks"]][[current_facet]]

      core_frame$dims[["min.local.x"]] <- min(
        as.numeric(core_frame$dims[["xticks"]][[current_facet]]))
      core_frame$dims[["max.local.x"]] <- max(
        as.numeric(core_frame$dims[["xticks"]][[current_facet]]))
    }

    # Y axes

    y_axis_draw  <- TRUE

    if (!is.null(core_frame$dims[["ydraws"]])) {
      y_axis_draw <-  current_facet %in% core_frame$dims[["ydraws"]]
    }

    y_axis_ticks <- seq(ceiling(core_frame$dims[["global.min.y"]]),
                        ceiling(core_frame$dims[["global.max.y"]]),
                        by = core_frame$dims[["ydelta"]])

    if (!is.null(core_frame$dims[["local.dims"]])) {
      core_frame$dims[["min.local.y"]] <- core_frame$dims[["local.dims"]][[current_facet]]$y0
      core_frame$dims[["max.local.y"]] <- core_frame$dims[["local.dims"]][[current_facet]]$y1

      y_axis_ticks <- seq(core_frame$dims[["min.local.y"]],
                          core_frame$dims[["max.local.y"]],
                          by = core_frame$dims[["ydelta"]])

      if ("yticks" %in% names(core_frame$dims[["local.dims"]][[ current_facet]]))
        y_axis_ticks <- core_frame$dims[["local.dims"]][[ current_facet]]$yticks

    } else {
      core_frame$dims[["min.local.y"]] <- ifelse(is.null(
        core_frame$dims[["global.min.y"]]),
        min(core_frame$data[[as.character(core_frame$aes["y"])]]),
        core_frame$dims[["global.min.y"]])

      core_frame$dims[["max.local.y"]] <- ifelse(is.null(
        core_frame$dims[["global.min.y"]]),
        max(core_frame$data[[as.character(core_frame$aes["y"])]]),
        core_frame$dims[["global.max.y"]])

      y_axis_ticks <- seq(core_frame$dims[["min.local.y"]],
                          core_frame$dims[["max.local.y"]],
                          by = core_frame$dims[["ydelta"]])
    }

    if (!is.null(core_frame$dims[["yticks"]]) && !is.list(
      core_frame$dims[["yticks"]])) {

      y_axis_ticks <- as.integer(core_frame$dims[["yticks"]])
    }

    plot(NULL,
         xlim = c(core_frame$dims[["min.local.x"]],
                  core_frame$dims[["max.local.x"]]),
         ylim = c(core_frame$dims[["min.local.y"]],
                  core_frame$dims[["max.local.y"]]),
         ylab = "",
         xlab = "",
         frame.plot = FALSE,
         las = 1,
         xaxt = "n",
         yaxt = "n")

    box(bty = "l")

    if (!is.null(core_frame$dims[["xticklabs"]]) &&
        !is.list(core_frame$dims[["xticklabs"]]) &&
        x_axis_draw) {

      x_axis_draw <- core_frame$dims[["xticklabs"]]
    }

    if (!is.null(core_frame$dims[["yticklabs"]]) &&
        !is.list(core_frame$dims[["yticklabs"]])) {

      y_axis_draw <- core_frame$dims[["yticklabs"]]
    }

    ## TODO: y axis labs here

    axis(1,
         labels = x_axis_draw,
         at     = x_axis_ticks)

    axis(2,
         labels = y_axis_draw,
         las    = 1,
         at     = y_axis_ticks)

    if (length(core_frame[["layers"]]) > 0) {
      for (i in seq_len(length(core_frame[["layers"]]))) {

        current_layer <- core_frame$layers[[i]]

        if (current_layer$type == "arrows")
          draw_arrows(core_frame,
                      current_layer,
                      current_facet)

        if (current_layer$type == "brackets")
          draw_brackets(core_frame,
                        current_layer,
                        current_facet)

        if (current_layer$type == "bar_support")
          draw_bar_support(core_frame,
                           current_layer,
                           current_facet)

        if (current_layer$type == "cum_sum_lines")
          draw_cumsum_lines(core_frame,
                            current_layer,
                            current_facet)

        if (current_layer$type == "cum_sum_points")
          draw_cumsum_points(core_frame,
                             current_layer,
                             current_facet)

        if (current_layer$type == "facet_label")
          draw_label_facet(core_frame,
                           current_layer,
                           current_facet)

        if (current_layer$type == "guide_line")
          draw_guide_line(core_frame,
                          current_layer,
                          current_facet)

        if (current_layer$type == "line")
          draw_lines(core_frame,
                     current_layer,
                     current_facet)

        if (current_layer$type == "phase_label")
          draw_label_phase(core_frame,
                           current_layer,
                           current_facet)

        if (current_layer$type == "phase_lines")
          draw_scr_plines(core_frame,
                          current_layer,
                          current_facet)

        if (current_layer$type == "point")
          draw_points(core_frame,
                      current_layer,
                      current_facet)

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
              core_frame$dims[["max.local.y"]])

            current_layer$lines[[pname]][[current_index]][["botDraw"]] <- cnvrt_coords(
              tmp_x2,
              -((core_frame$dims[["max.local.y"]] -
                   core_frame$dims[["min.local.y"]]) * 0.04))

            tmp_point_top_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["topDraw"]]$dev,
              input = "dev")

            tmp_point_bot_dev <- cnvrt_coords(
              current_layer$lines[[pname]][[current_index]][["botDraw"]]$dev,
              input = "dev")

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

            index_num[[pname]] <- index_num[[pname]] + 1
          }

          req_draw <- TRUE
        }
      }
    }

    if (!is.null(core_frame[["legendpars"]])) {
      if (lookup && core_frame$legendpars[["panel"]] ==  current_facet) {
        draw_legend(core_frame)
      } else if (lookup && is.na(core_frame$legendpars[["panel"]])) {
        draw_legend(core_frame)
      }
    }
  }

  # Note: final overlays, once facets are drawn/coords cached
  if (req_draw) {

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

  if (!lookup && !is.null(core_frame[["legendpars"]]))  draw_legend(core_frame)

  mtext(core_frame$labs[["title"]],
        side = 3,
        outer = core_frame$labs[["outer"]])
  mtext(core_frame$labs[["ylab"]],
        side = 2,
        outer = core_frame$labs[["outer"]],
        line = core_frame$labs[["outer.y.line"]])
  mtext(core_frame$labs[["xlab"]],
        side = 1,
        outer = core_frame$labs[["outer"]],
        line = core_frame$labs[["outer.x.line"]])
}
