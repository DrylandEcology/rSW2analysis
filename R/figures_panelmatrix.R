
#' @export
plot_matrix_of_panels <- function(
  n_panels,
  data_matrix, meta = NULL, subset = NULL,
  xlim_matrix = NULL, ylim_matrix = NULL, zlim_matrix = NULL,
  label_title_matrix = NULL, label_axis_matrix = NULL,
  use_labels = c("none", "panel_identifier", "legend_title"),
  type_matrix = NULL,
  ...,
  addfun_matrix = NULL,
  addenv_matrix = NULL,
  col_zero = "gray",
  col_rev = FALSE,
  legend_matrix = NULL,
  map_legend_pos = c("left", "right"),
  map_extent = NULL,
  fextend = 1, fexp_legend = 0.75, fexp_axis = 0.75,
  path, ftag = "",
  pborders = NULL,
  device = c("png", "pdf")
) {

  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  # Preparations
  dots <- list(...)
  use_labels <- match.arg(use_labels)
  label_title_str_matrix <- label_title_matrix
  is_plotmath_matrix <- add_legend <- array(FALSE, dim = n_panels)
  is_map <- is_smoothScatter <- array(NA, dim = n_panels)
  device <- match.arg(device)

  if (is.null(label_axis_matrix)) {
    label_axis_matrix <- array(list(), dim = n_panels)
  }

  if (is.null(label_title_matrix)) {
    label_title_matrix <- array(list(), dim = n_panels)
  }

  if (is.null(type_matrix)) {
    type_matrix <- array(list(), dim = n_panels)
  }

  if (is.null(legend_matrix)) {
    legend_matrix <- array(list(), dim = n_panels)
  }


  for (k1 in seq_len(n_panels[1])) for (k2 in seq_len(n_panels[2])) {
    # Subset data
    if (!is.null(subset)) {
      if (is.null(dim(data_matrix[k1, k2][[1]]))) {
        data_matrix[k1, k2][[1]][!subset] <- NA
      } else {
        data_matrix[k1, k2][[1]][!subset, ] <- NA
      }
    }

    if (!is.null(type_matrix[k1, k2][[1]])) {
      # Extract panel information
      tmp <- strsplit(type_matrix[k1, k2][[1]], split = ".", fixed = TRUE)[[1]]
      ntmp <- length(tmp)
      if (ntmp > 1) {
        is_map[k1, k2] <- tmp[1] == "map"
        is_smoothScatter[k1, k2] <- tmp[1] == "smoothScatter"
        type_matrix[k1, k2][[1]] <- tmp[2]
      } else {
        type_matrix[k1, k2][[1]] <- tmp[1]
      }

      # Fix limits for agreement_sign type
      if (type_matrix[k1, k2][[1]] == "agreement_sign") {
        zlim_matrix[k1, k2][[1]] <- get_zlim_for_agreement_sign(
          data_agree = data_matrix[k1, k2][[1]],
          zlim = zlim_matrix[k1, k2][[1]])
      }

      if (
        type_matrix[k1, k2][[1]] == "delta" &&
        !is.null(label_title_matrix[k1, k2][[1]]) &&
        nchar(label_title_matrix[k1, k2][[1]] > 0)
      ) {
        # Compose plotmath expression as string
        label_title_matrix[k1, k2][[1]] <- paste0(
          "Delta~",
          bquote(.(label_title_matrix[k1, k2]))
        )
        label_title_str_matrix[k1, k2][[1]] <- paste0(
          "D ",
          label_title_str_matrix[k1, k2][[1]]
        )
        is_plotmath_matrix[k1, k2] <- TRUE
      }
    }

    add_legend[k1, k2] <- if (!is.null(legend_matrix[k1, k2][[1]])) {
        legend_matrix[k1, k2][[1]]
      } else {
        isTRUE(is_map[k1, k2])
      }

    if (isTRUE(is_map[k1, k2]) && is.null(label_axis_matrix[k1, k2][[1]])) {
      label_axis_matrix[k1, k2] <- list(c("mapx", "mapy"))
    }
  }

  # Prepare additional arguments
  if (is.null(dots[["add_loess"]])) {
    dots[["add_loess"]] <- TRUE
  }

  if (is.null(dots[["add_1to1"]])) {
    dots[["add_1to1"]] <- FALSE
  }

  if (is.null(dots[["asp"]])) {
    dots[["asp"]] <- NA
  }

  # Identify panels that need axis labels
  N_prj <- prod(n_panels)
  tick_ids_matrix <- matrix(seq_len(N_prj),
    nrow = n_panels[1], ncol = n_panels[2], byrow = TRUE)
  ticks_matrix <- array(FALSE, dim = c(n_panels, 2))

  # x-axis labels needed if new x-axis label/limits for column
  id_ticks1 <- NULL
  for (k2 in seq_len(n_panels[2])) {
    tmp <- sapply(seq_len(n_panels[1]),
      function(k1) {
        tmp <- if (is.null(label_axis_matrix[k1, k2][[1]])) {
          k1n <- k1 + 1
          if (k1n <= n_panels[1] && !is.null(label_axis_matrix[k1n, k2][[1]])) {
            label_axis_matrix[k1n, k2][[1]][1]
          } else {
            ""
          }
        } else {
          label_axis_matrix[k1, k2][[1]][1]
        }
        if (!is.null(xlim_matrix[k1, k2][[1]])) {
          tmp <- paste0(tmp, "_",
            paste(prettyNum(xlim_matrix[k1, k2][[1]]), collapse = "_"))
        }
        tmp
    })
    rl1 <- which(diff(c(0, as.integer(factor(tmp, levels = unique(tmp))))) > 0)
    rl1 <- n_panels[1] - rl1 + 1
    ticks_matrix[rl1, k2, 1] <- TRUE
    id_ticks1 <- unique(sort(c(id_ticks1, tick_ids_matrix[rl1, k2])))
  }

  # y-axis labels needed if new y-axis label/limits for row
  id_ticks2 <- NULL
  for (k1 in seq_len(n_panels[1])) {
    tmp <- sapply(seq_len(n_panels[2]),
      function(k2) {
        tmp <- if (is.null(label_axis_matrix[k1, k2][[1]])) {
          k2n <- k2 + 1
          if (k2n <= n_panels[2] && !is.null(label_axis_matrix[k1, k2n][[1]])) {
            label_axis_matrix[k1, k2n][[1]][1]
          } else {
            ""
          }
        } else {
          label_axis_matrix[k1, k2][[1]][2]
        }
        if (!is.null(ylim_matrix[k1, k2][[1]])) {
          tmp <- paste0(tmp, "_",
            paste(prettyNum(ylim_matrix[k1, k2][[1]]), collapse = "_"))
        }
        tmp
    })
    rl2 <- which(diff(c(0, as.integer(as.factor(tmp)))) > 0)
    ticks_matrix[k1, rl2, 2] <- TRUE
    id_ticks2 <- unique(sort(c(id_ticks2, tick_ids_matrix[k1, rl2])))
  }


  # Prepare map legend position
  map_legend_pos <- tolower(match.arg(map_legend_pos))
  dbox <- c(x0 = 0.01, x1 = 0.04, y0 = 0.10, y1 = 0.42)

  if (map_legend_pos == "left") {
  } else if (map_legend_pos == "right") {
    dx <- dbox["x1"] - dbox["x0"]
    dbox["x0"] <- 0.8
    dbox["x1"] <- dbox["x0"] + dx
  }

  # Map extent
  if (!is.null(map_extent) && inherits(map_extent, "Extent")) {
    map_xlim <- c(map_extent@xmin, map_extent@xmax)
    map_ylim <- c(map_extent@ymin, map_extent@ymax)

  } else {
    map_xlim <- map_ylim <- NULL
  }


  # Figure sizes:

  # Figure scaling
  fexp <- 1
  fexp_legend <- fexp_legend * fexp
  fexp_axis <- fexp_axis * fexp

  # Heigth of panel and lower/interior/upper edge
  h.panel <- 4
  h.edge <- c(0.025, rep(0.025, n_panels[1L]))

  if (use_labels == "panel_identifier" && N_prj > 1) {
    h.edge[seq_len(n_panels[1L])] <- 0.25
  }

  haxs <- apply(ticks_matrix[, , 1, drop = FALSE], 1, any)
  hmapsonly <- apply(is_map, 1, all, na.rm = TRUE)
  if (any(haxs)) {
    h.edge[1 + which(haxs)] <- h.edge[1 + which(haxs)] +
      0.25 * ifelse(hmapsonly[haxs], 1, 1.5)
  }

  layout_heights <- c(NA, rep(c(h.panel, NA), n_panels[1L]))
  layout_heights[seq(1, 2 * n_panels[1L] + 1, by = 2)] <- h.edge

  # Width of panel and left/interior/right edge
  w.panel <- 4
  w.edge <- c(rep(0.025, n_panels[2L]), 0.025)

  waxs <- apply(ticks_matrix[, , 2, drop = FALSE], 2, any)
  wmapsonly <- apply(is_map, 2, all, na.rm = TRUE)
  if (any(waxs)) {
    w.edge[which(waxs)] <- w.edge[which(waxs)] +
      0.25 * ifelse(wmapsonly[waxs], 1, 1.5)
  }

  layout_widths <- c(NA, rep(c(w.panel, NA), n_panels[2L]))
  layout_widths[seq(1, 2 * n_panels[2L] + 1, by = 2)] <- w.edge


  # Figure layout
  lmat <- matrix(0L,
    nrow = 2 + 2 * n_panels[1] - 1L, ncol = 2 + 2 * n_panels[2] - 1,
    byrow = TRUE)

  stemp2 <- seq_len(n_panels[2L])
  for (k in seq_len(n_panels[1L])) {
    lmat[(k - 1) * 2 + 2, seq_len(ncol(lmat)) %% 2 == 0] <-
        (k - 1L) * n_panels[2L] + stemp2
  }

  # Figure device
  fname <- file.path(path, paste0("Fig_", ftag, ".", device))
  if (device == "png") {
    grDevices::png(
      filename = fname,
      units = "in",
      res = 150,
      height = fexp * sum(layout_heights),
      width = fexp * sum(layout_widths)
    )

  } else if (device == "pdf") {
    grDevices::pdf(
      file = fname,
      height = fexp * sum(layout_heights),
      width = fexp * sum(layout_widths)
    )
  }

  graphics::layout(lmat, heights = layout_heights, widths = layout_widths)

  par_prev <- graphics::par(
    mar = rep(0.1, 4), mgp = c(1, 0, 0), tcl = 0.3, cex = 1
  )

  on.exit({
    graphics::par(par_prev)
    grDevices::dev.off()
  })

  # Panel count / panel identifier
  i <- c(1, 1)

  # Panels
  for (k1 in seq_len(n_panels[1])) for (k2 in seq_len(n_panels[2])) {
    x <- data_matrix[k1, k2][[1]]

    if (all(is.na(x)) || is.null(x)) {
      graphics::plot.new()
      i[1] <- i[1] + 1
      next
    }

    if (isTRUE(is_map[k1, k2])) {
      # Get colors
      ctemp <- prepare_colors(
        type = type_matrix[k1, k2][[1]],
        zlim = zlim_matrix[k1, k2][[1]],
        col_zero = col_zero,
        col_rev = col_rev
      )

      # Plot map
      rdata <- draw_GISSM_map(
        x,
        meta,
        fextend,
        map_xlim,
        map_ylim,
        col_desc = ctemp,
        annx = i[1] %in% id_ticks1,
        anny = i[1] %in% id_ticks2,
        fexp_axis = fexp_axis,
        pborders = pborders
      )

    } else if (isTRUE(is_smoothScatter[k1, k2])) {
      draw_smoothScatter_panel(
        x = x[, 1], y = x[, 2],
        asp = dots[["asp"]],
        add_loess = dots[["add_loess"]],
        add_1to1 = dots[["add_1to1"]],
        xlim = xlim_matrix[k1, k2][[1]],
        ylim = ylim_matrix[k1, k2][[1]],
        annx = i[1] %in% id_ticks1,
        anny = i[1] %in% id_ticks2,
        xlab = label_axis_matrix[k1, k2][[1]][1],
        ylab = label_axis_matrix[k1, k2][[1]][2],
        is_plotmath = c(FALSE, FALSE),
        fexp_axis = fexp_axis
      )

    }

    # Additional panel elements
    if (!is.null(addfun_matrix) && !is.null(addfun_matrix[k1, k2])) {
      if (!is.null(addenv_matrix) && !is.null(addenv_matrix[k1, k2])) {
        eval(addfun_matrix[k1, k2][[1]], envir = addenv_matrix[k1, k2][[1]])
      } else {
        eval(addfun_matrix[k1, k2][[1]])
      }
    }

    # Add map legend
    if (isTRUE(is_map[k1, k2]) && add_legend[k1, k2]) {
      add_legend_to_GISSM_map(
        rdata,
        dbox,
        col_desc = ctemp,
        label = label_title_matrix[k1, k2][[1]],
        is_plotmath = is_plotmath_matrix[k1, k2],
        label_str = label_title_str_matrix[k1, k2][[1]],
        add_title = use_labels == "legend_title",
        fexp_legend = fexp_legend
      )
    }

    # Add panel identifier
    if (N_prj > 1) {
      add_panel_identifier(
        i[2],
        add_label = use_labels == "panel_identifier",
        label = label_title_matrix[k1, k2][[1]],
        is_plotmath = is_plotmath_matrix[k1, k2],
        fexp = fexp
      )
    }

    i <- i + 1
  }
}
