
#' Create and plot a continuous color legend
#'
#' @param zlim A numeric vector of length two. The smallest and largest z-value
#'   of \code{grid} for which a complete color gradient and legend should be
#'   printed.
#' @param zextreme A numeric vector length two. The minimum and maximum z-value
#'   of \code{grid}. If \code{zextreme[1] < zlim[1] || zextreme[2] > zlim[2]},
#'   then the values in the ranges between each \code{zextreme} and \code{zlim}
#'    will be highlighted as extreme areas in a different color.
#' @param col_desc A list with the color names. A returned object from a call to
#'   the function \code{\link{prepare_colors}}.
#' @param grid A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'   object for which the legend is created.
#' @param box A numeric vector of length four. The SW (lower left) and NE (upper
#'   right) corners of the color legend on the plotted \code{grid}.
#' @param whitebox A logical value. If \code{TRUE}, then the area of the color
#'   legend is printed white before the colors of the legend are added.
#' @param horiz A logical value. If \code{TRUE}, then the values of the color
#'   legend are printed below the legend box; if \code{FALSE}, then the values
#'   are printed on the right side of the box.
#' @param signed A numeric value. A scaling value multiplied with the z-values
#'   for determining legend values.
#' @param fun_inv_ens A function or \code{NULL}. If a function, then it will be
#'   applied to z-values for determining legend values.
#' @param at The points at which tick-marks are to be drawn. By default
#'   (when \code{NULL}) tickmark locations are computed,
#'   see \code{\link[graphics]{axis}}.
#' @param tick A logical value. Indicates whether or not tickmarks should be
#'   added to the legend color ramp.
#' @param labels The annotations to be written at the tick-marks,
#'   see \code{\link[graphics]{axis}}.
#' @param srt A numerical value. String rotation in degrees, see
#'   \code{\link[graphics]{text}}.
#' @param cex A numerical value. Numeric character expansion factor, see
#'   \code{\link[graphics]{text}}.
#'
#' @export
add_legend <- function(zlim, zextreme, col_desc, grid,
  box = c(-100, -97, -50, -10), whitebox = TRUE, horiz = FALSE, signed = 1,
  fun_inv_ens = NULL, at = NULL, tick = TRUE, labels = TRUE, srt = 90,
  cex = 1) {

  stopifnot(requireNamespace("raster"))
  if (is.null(fun_inv_ens)) fun_inv_ens <- function(x) x
  if (missing(zextreme) || is.null(zextreme)) zextreme <- zlim

  #--- Color legend
  # Create color legend
  zr <- raster::raster(xmn = box[1], xmx = box[2], ymn = box[3], ymx = box[4],
    crs = raster::projection(grid), resolution = raster::res(grid), vals = NULL)
  zr[] <- if (horiz) {
      rep(1:dim(zr)[2], times = dim(zr)[1])
    } else {
      rep(dim(zr)[1]:1, each = dim(zr)[2])
    }

  # Draw to plot
  if (whitebox) {
    raster::image(zr, col = "white", add = TRUE)
  }
  raster::image(zr, col = col_desc[["colors_label"]], add = TRUE)

  #--- Annotate
  # Calculate tickmarks on `zlim`-scale
  # Positions: generate default if `at` is NULL
  atz <- if (is.null(at)) pretty(zlim, n = 6L) else at

  # Beautify tickmarks
  temp2z <- atz <= zlim[1]
  if (any(temp2z)) {
    # adjust position(s) below the limit to one tickmark at the limit
    ids_temp2z <- which(temp2z)
    atz <- c(zlim[1], atz[!temp2z])
  }
  temp2z <- atz >= zlim[2]
  if (any(temp2z)) {
    # adjust position(s) above the limit to one tickmark at the limit
    ids_temp2z <- which(temp2z)
    atz <- c(atz[!temp2z], zlim[2])
  }

  datz <- diff(atz)
  temp <- 0.8 / (if (abs(srt) > 45) cex else 1)
  if (length(datz) > 0 && length(temp <- which(datz < temp * max(datz))) > 0) {
    # remove tickmarks if too close together, but not limits and not 0
    id_remove <- findInterval(x = 1 + temp, vec = seq_along(atz),
      all.inside = TRUE)
    temp <- which(0 == atz[id_remove])
    if (length(temp) > 0) {
      id_remove <- id_remove[-temp]
    }
    if (length(id_remove) > 0) {
      atz <- atz[-id_remove]
    }
  }
  if (length(datz) == 0) {
    atz <- zextreme
  }

  # Calculate tickmarks on `color`-scale
  ext <- raster::extent(zr)
  rtemp <- col_desc[["ncol_label_added"]] / length(col_desc[["colors_label"]])

  if (horiz) {
    xmin_orig <- ext@xmin
    if (col_desc[["added_below"]]) {
      xmin_orig <- xmin_orig + rtemp * (ext@xmax - ext@xmin)
    }

    xmax_orig <- ext@xmax
    if (col_desc[["added_above"]]) {
      xmax_orig <- xmax_orig - rtemp * (ext@xmax - ext@xmin)
    }

    temp <- (atz - min(zlim)) / diff(range(zlim))
    xs_orig <- xmin_orig + temp * (xmax_orig - xmin_orig)

  } else {
    ymin_orig <- ext@ymin
    if (col_desc[["added_below"]]) {
      ymin_orig <- ymin_orig + rtemp * (ext@ymax - ext@ymin)
    }

    ymax_orig <- ext@ymax
    if (col_desc[["added_above"]]) {
      ymax_orig <- ymax_orig - rtemp * (ext@ymax - ext@ymin)
    }

    temp <- (atz - min(zlim)) / diff(range(zlim))
    ys_orig <- ymin_orig + temp * (ymax_orig - ymin_orig)
  }

  # Draw ticks
  if (tick) {
    lwd_seg <- max(0.5, min(1, cex)) * graphics::par("lwd")
    if (horiz) {
      graphics::segments(x0 = xs_orig, x1 = xs_orig,
        y0 = ext@ymax - (ext@ymax - ext@ymin) / 3, y1 = ext@ymax, lwd = lwd_seg)

      if (col_desc[["added_below"]]) {
        graphics::segments(x0 = ext@xmin, x1 = ext@xmin, y0 = ext@ymin, y1 = ext@ymax,
          lwd = lwd_seg)
      }
      if (col_desc[["added_above"]]) {
        graphics::segments(x0 = ext@xmax, x1 = ext@xmax, y0 = ext@ymin,
          y1 = ext@ymax, lwd = lwd_seg)
      }

    } else {
      graphics::segments(x0 = ext@xmax - (ext@xmax - ext@xmin) / 3,
        x1 = ext@xmax, y0 = ys_orig, y1 = ys_orig, lwd = lwd_seg)

      if (col_desc[["added_below"]]) {
        graphics::segments(x0 = ext@xmin, x1 = ext@xmax, y0 = ext@ymin,
          y1 = ext@ymin, lwd = lwd_seg)
      }
      if (col_desc[["added_above"]]) {
        graphics::segments(x0 = ext@xmin, x1 = ext@xmax, y0 = ext@ymax,
          y1 = ext@ymax, lwd = lwd_seg)
      }
    }
  }

  # Decide whether to add tickmark labels
  if (is.logical(labels)) {
    add_automatic_labels <- labels
    add_arg_labels <- FALSE
  } else {
    add_automatic_labels <- FALSE
    labels <- grDevices::as.graphicsAnnot(labels)
    add_arg_labels <- length(labels) > 0 && length(labels) == length(atz)
  }
  add_labels <- add_automatic_labels || add_arg_labels

  # Tickmark labels
  if (add_labels) {
    if (add_automatic_labels) {
      # Create automatic tickmark labels
      ltxt <- prettyNum(signif(signed * fun_inv_ens(atz), 2))
      temp <- signif(signed * fun_inv_ens(zextreme), 2)
      ltext_extreme <- prettyNum(temp)
      if (any(temp < 0) && any(temp > 0)) {
        id <- temp > 0
        ltext_extreme[id] <- paste0("+", ltext_extreme[id])
      }
    } else {
      # generate argument-based tickmark labels
      ltxt <- labels
    }

    # Add tickmark labels to plot
    if (horiz) {
      if (abs(srt) > 45) {
        adj <- c(0.5, 1.3)
        ly <- ext@ymax
      } else {
        adj <- c(0.5, NA)
        ly <- ext@ymax +
          graphics::strheight(ltxt, units = "user", cex = cex * 1.05)
      }

      graphics::text(
        x = xs_orig, y = ly,
        labels = ltxt, srt = srt,
        adj = adj, cex = cex, xpd = TRUE
      )

      if (col_desc[["added_below"]] && !add_arg_labels) {
        graphics::text(
          x = ext@xmin, y = ext@ymin,
          labels = ltext_extreme[1],
          srt = 90, adj = c(0.1, -0.5), cex = cex, xpd = TRUE
        )
      }
      if (col_desc[["added_above"]] && !add_arg_labels) {
        graphics::text(
          x = ext@xmax, y = ext@ymin,
          labels = ltext_extreme[2],
          srt = 90, adj = c(0.1, 1.5), cex = cex, xpd = TRUE
        )
      }

    } else {
      if (abs(srt) > 45) {
        adj <- c(0.5, 1.3)
        lx <- ext@xmax
      } else {
        if (add_automatic_labels) {
          adj <- c(1, NA)
          temp1 <- if (ext@xmax > 0) -1 else +1
          temp2 <- if (cex < 0.5) 1.5 else 1.05
          lx <- ext@xmax + temp1 * max(graphics::strwidth(ltxt, units = "user",
            cex = cex * temp2))
        } else {
          adj <- c(-0.05, NA)
          lx <- ext@xmax
        }
      }

      graphics::text(
        x = lx, y = ys_orig,
        labels = ltxt, srt = srt, adj = adj,
        cex = cex, xpd = TRUE
      )

      if (col_desc[["added_below"]] && !add_arg_labels) {
        graphics::text(
          x = ext@xmax, y = ext@ymin,
          labels = ltext_extreme[1],
          srt = 0, adj = c(1, 1.3), cex = cex, xpd = TRUE
        )
      }
      if (col_desc[["added_above"]] && !add_arg_labels) {
        graphics::text(
          x = ext@xmax, y = ext@ymax,
          labels = ltext_extreme[2],
          srt = 0, adj = c(1, -0.5), cex = cex, xpd = TRUE
        )
      }
    }
  }

  invisible(TRUE)
}


#' Create a raster and plot it to the current device
#' @export
add_panel_map <- function(x, meta, subset, ...) {
  .Deprecated(new = "map_from_variable")

  map_from_variable(x, meta, subset, ...)
}


#' Create a raster from values and plot it to the current device
#'
#' @param x A vector of values to plot as a map, see details.
#' @param meta The metadata list of a \pkg{rSFSW2} simulation experiment.
#' @param subset A vector logical vector to subset \code{x} or missing.
#' @param ... Additional parameters most of which are passed to the plotting
#'   function(s) \code{\link[raster]{image}} or
#'   \code{\link[graphics]{rasterImage}};
#'   exceptions are explained in the details.
#' @param beautify Add country borders and the equator to the plot.
#'
#' @details \code{x} can be specified in two ways:
#'  codes: \itemize{
#'    \item If \code{x} is a numerical vector, then the values are interpreted
#'      as \var{z} values and plotted by the function
#'      \code{\link[raster]{image}} which calls \code{\link[graphics]{image}}.
#'    \item If \code{x} is a vector of hexadecimal color codes, then the
#'      values are plotted by the function \code{\link[graphics]{rasterImage}},
#'      similar to how \code{\link[raster]{plotRGB}} plots multi-color raster
#'      objects.
#'  }
#'
#' @details The following arguments in \code{...} are treated separately:
#' \itemize{
#'   \item If \code{ylim} is available but not \code{asp}, then an aspect
#'     ratio is calculated that corrects for longitude/latitude.
#'   \item If \code{useNA} is \code{TRUE} and \code{valNA} is available, then
#'     \code{NA}s are replaced by \code{valNA}.
#'   \item If \code{trim} is \code{TRUE}, then the resulting raster object
#'     is trimmed using \code{\link[raster]{trim}}.
#'   \item If \code{fextend} is a finite number, then the extent of the raster
#'     object (after trimming) is expanded by that factor.
#' }
#'
#' @return The raster object generated from \code{x} by a call to
#'   \code{\link{create_raster_from_variables}} is returned invisibly.
#'   The function is called for its side effect of plotting to the
#'   current device.
#'
#' @export
map_from_variable <- function(x, meta, subset, ..., beautify = TRUE) {
  #--- Prepare plotting arguments
  dots <- list(...)

  # Remove xlim and/or ylim if not finite
  id_dots_xlim <- match("xlim", names(dots), nomatch = 0)
  if (isTRUE(id_dots_xlim > 0)) {
    tmp <- dots[["xlim"]]
    if (is.null(tmp) || any(!is.finite(tmp))) {
      dots <- dots[-id_dots_xlim]
    }
  }

  id_dots_ylim <- match("ylim", names(dots), nomatch = 0)
  if (isTRUE(id_dots_ylim > 0)) {
    tmp <- dots[["ylim"]]
    if (is.null(tmp) || any(!is.finite(tmp))) {
      dots <- dots[-id_dots_ylim]
    }
  }

  # Adjust aspect ratio to correct for longitude/latitude
  dots_names <- names(dots)
  if ("ylim" %in% dots_names && !("asp" %in% dots_names)) {
    dots[["asp"]] <- 1 / cos((mean(dots[["ylim"]]) * pi) / 180)
  }

  # Subset x
  if (!missing(subset)) {
    x[!subset] <- NA
  }

  # Deal with NAs after subsetting
  id_dots_NAs <- match(c("useNA", "valNA"), names(dots), nomatch = 0)
  has_dots_NAs <- id_dots_NAs > 0

  if (all(has_dots_NAs) && isTRUE(dots[["useNA"]])) {
    ids <- if (missing(subset)) is.na(x) else is.na(x) & subset
    x[ids] <- dots[["valNA"]]
  }

  if (any(has_dots_NAs)) {
    dots <- dots[-id_dots_NAs]
  }


  #--- Create raster from data and plot it with suitable function
  rdata <- NULL

  if (is.numeric(x)) {
    # Create raster
    rdata <- create_raster_from_variables(meta, data = x)

    # Trim raster
    id_dots_trim <- match("trim", names(dots), nomatch = 0)
    if (isTRUE(id_dots_trim > 0)) {
      if (isTRUE(dots[["trim"]])) {
        rdata <- raster::trim(rdata)
      }

      dots <- dots[-id_dots_trim]
    }

    # Extend raster by a factor
    id_dots_extend <- match("fextend", names(dots), nomatch = 0)
    if (isTRUE(id_dots_extend > 0)) {
      if (isTRUE(is.finite(dots[["fextend"]]))) {
        rdata <- raster::extend(rdata, dots[["fextend"]] * raster::extent(rdata))
      }

      dots <- dots[-id_dots_extend]
    }

    # Make sure all gridcells are printed if not specified
    id_dots_maxpixels <- match("maxpixels", names(dots), nomatch = 0)
    if (isTRUE(id_dots_maxpixels == 0)) {
      dots[["maxpixels"]] <- raster::ncell(rdata)
    }

    # Remove unsupported dot-arguments
    id_dots_interpolate <- match("interpolate", names(dots), nomatch = 0)
    if (isTRUE(id_dots_interpolate > 0)) {
      dots <- dots[-id_dots_interpolate]
    }


    #--- Plot raster as map
    do.call(selectMethod("image", class(rdata)),
      args = c(list(x = rdata), dots))


  } else {
    could_be_color <- try(grDevices::col2rgb(x[!is.na(x)][1]), silent = TRUE)
    if (is.character(x) && !inherits(could_be_color, "try-error")) {
      x_isnotna <- !is.na(x)

      # Create raster
      tmp <- rep(NA, length(x))
      tmp[x_isnotna] <- seq_len(sum(x_isnotna))
      rdata <- create_raster_from_variables(meta, data = tmp)
      bb <- as.vector(t(sp::bbox(rdata)))

      # Extract add plot argument
      id_dots_add <- match("add", names(dots), nomatch = 0)
      add <- FALSE
      if (isTRUE(id_dots_add > 0)) {
        add <- dots[["add"]]
        dots <- dots[-id_dots_ylim]
      }

      # Make sure `rasterImage` arguments are set well
      id_dots_interpolate <- match("interpolate", names(dots), nomatch = 0)
      if (isTRUE(id_dots_interpolate == 0)) {
        dots[["interpolate"]] <- FALSE
      }

      id_dots_xlim <- match("xlim", names(dots), nomatch = 0)
      if (isTRUE(id_dots_xlim > 0)) {
        dots[["xleft"]] <- dots[["xlim"]][1]
        dots[["xright"]] <- dots[["xlim"]][2]
        dots <- dots[-id_dots_xlim]

      } else {
        dots[["xleft"]] <- bb[1]
        dots[["xright"]] <- bb[2]
      }

      id_dots_ylim <- match("ylim", names(dots), nomatch = 0)
      if (isTRUE(id_dots_ylim > 0)) {
        dots[["ybottom"]] <- dots[["ylim"]][1]
        dots[["ytop"]] <- dots[["ylim"]][2]
        dots <- dots[-id_dots_ylim]

      } else {
        dots[["ybottom"]] <- bb[3]
        dots[["ytop"]] <- bb[4]
      }

      # Remove unsupported dot-arguments
      to_keep <- unique(c(
        names(formals(graphics::rasterImage)),
        names(graphics::par(no.readonly = TRUE))
      ))
      dots <- dots[names(dots) %in% to_keep]


      # Trim raster to requested extent
      rdata <- raster::crop(rdata,
        raster::extent(dots[["xleft"]], dots[["xright"]],
          dots[["ybottom"]], dots[["ytop"]]))

      # Create matrix
      loc <- meta[["sim_space"]][["run_sites"]]
      loc <- sp::spTransform(loc, CRS = raster::crs(rdata))
      stopifnot(length(loc) == length(x))
      ids_cells <- raster::cellFromXY(rdata, sp::coordinates(loc))
      ids_isnotna <- !is.na(ids_cells)
      rdata[ids_cells[ids_isnotna]] <- x[ids_isnotna]
      z <- selectMethod("as.matrix", class(rdata))(rdata)

      #--- Start new plot or add to existing one?
      if (!add) {
        graphics::plot(
          NA,
          type = "n",
          xlim = c(dots[["xleft"]], dots[["xright"]]),
          ylim = c(dots[["ybottom"]], dots[["ytop"]]),
          xaxs = "i", yaxs = "i", axes = FALSE, ann = FALSE
        )
      }

      #--- Add raster as map to plot
      do.call(graphics::rasterImage, args = c(list(image = z), dots))


    } else {
      stop("Argument `x` must be a vector of numbers or color codes.")
    }
  }


  if (beautify) {
    if (requireNamespace("maps")) {
      maps::map(add = TRUE, resolution = 0, lwd = 0.5) # Country border
    }
    graphics::abline(h = 0, col = "darkgray", lty = 2, lwd = 1) # Equator
  }

  invisible(rdata)
}



#' @export
draw_GISSM_map <- function(x, meta, fextend, map_xlim, map_ylim,
  col_desc, annx = TRUE, anny = TRUE, fexp_axis = 1, pborders = NULL) {

  rdata <- map_from_variable(
    x = x,
    meta = meta, trim = TRUE, fextend = fextend,
    asp = 1,
    xlim = map_xlim, ylim = map_ylim,
    zlim = col_desc[["panel"]][["zlim"]],
    col = col_desc[["panel"]][["cols"]],
    axes = FALSE, ann = FALSE
  )


  # Add axes
  graphics::box()

  at <- graphics::axTicks(side = 1)
  graphics::axis(
    side = 1,
    at = at,
    labels = if (annx) format(at, scientific = TRUE) else FALSE,
    cex.axis = fexp_axis
  )

  at <- graphics::axTicks(side = 2)
  graphics::axis(
    side = 2,
    at = at,
    labels = if (anny) format(at, scientific = TRUE) else FALSE,
    cex.axis = fexp_axis
  )

  # Add country and state/province borders
  if (!is.null(pborders)) {
    sp::plot(pborders, lwd = 0.5, add = TRUE)
  }

  invisible(rdata)
}


#' @export
add_legend_to_GISSM_map <- function(rdata, dbox, col_desc, label,
  is_plotmath = FALSE, label_str = label, use_cats = FALSE, label_cats = "",
  add_title = FALSE, fexp_legend = 1) {

  xy_usr <- graphics::par("usr")
  d_usr <- c(xy_usr[2] - xy_usr[1], xy_usr[4] - xy_usr[3])
  box_legend <- c(
    x0 = xy_usr[1] + dbox[1] * d_usr[1],
    x1 = xy_usr[1] + dbox[2] * d_usr[1],
    y0 = xy_usr[3] + dbox[3] * d_usr[2],
    y1 = xy_usr[3] + dbox[4] * d_usr[2])

  dx2 <- box_legend[2] - box_legend[1] +
    graphics::strwidth("-8.88", cex = fexp_legend)
  dy <- graphics::strheight(label_str, cex = fexp_legend)
  box_title <- if (add_title) {
    c(
      dx = max(dx2, graphics::strwidth(label_str, cex = fexp_legend)),
      dy = dy
    )
  } else if (use_cats) {
    c(
      dx = max(graphics::strwidth(label_cats, cex = fexp_legend)),
      dy = dy
    )
  } else {
    c(dx = dx2, dy = dy)
  }

  ytmp <- c(
    if (add_title) 1 else 0.5,
    if (add_title) {
      if (is_plotmath) 3 else 2.5
    } else 0.75
  )

  rwhite <- raster::raster(
    xmn = box_legend[1],
    xmx = max(box_legend[2], box_legend[1] + 1.05 * box_title[1]),
    ymn = box_legend[3] - ytmp[1] * box_title[2],
    ymx = box_legend[4] + ytmp[2] * box_title[2],
    crs = raster::projection(rdata), resolution = raster::res(rdata),
    vals = 1)
  raster::image(rwhite, col = "white", add = TRUE)

  add_legend(
    zlim = col_desc[["legend"]][["zlim"]],
    zextreme = NULL,
    col_desc = col_desc[["legend"]][["desc"]],
    grid = rdata,
    srt = 0,
    cex = fexp_legend,
    box = box_legend,
    at = if (use_cats) -0.5 + seq_len(col_desc[["legend"]][["zlim"]][2]),
    labels = if (use_cats) label_cats else TRUE
  )

  if (add_title) {
    graphics::text(
      x = box_legend[1], y = box_legend[4],
      labels = if (is_plotmath) {
        # parse string containing plotmath expression
        parse(text = label)
      } else {
        label
      },
      adj = c(0, if (is_plotmath) -0.5 else -1),
      cex = fexp_legend
    )
  }
}
