
#' Add contour line to a \var{xy}-plot that envelopes \code{alpha * 100}-percent
#' of the \var{xy}-points
#'
#' @references Modified from code by Pascal Haenggi (2008)
#'   \url{https://stat.ethz.ch/pipermail/r-help/2008-June/166079.html}
#' @export
add_contour_to_pointcloud <- function(x, y, alpha = 0.95, col = "red",
  fill = FALSE, na.rm = FALSE, ...) {

  stopifnot(requireNamespace("MASS"))
  res <- FALSE

  if (na.rm) {
    iuse <- stats::complete.cases(cbind(x, y))
    x <- x[iuse]
    y <- y[iuse]
  }

  if (length(x) > 1 && stats::sd(x) > 0 && stats::sd(y) > 0) {
    xlim <- range(x)
    ylim <- range(y)

    # Extend limits by a factor of two
    xlim_kd <- rSW2utils::extend_range(xlim, 2)
    ylim_kd <- rSW2utils::extend_range(ylim, 2)

    # Determine suitable bandwidth/smoothing of kernel density estimate
    bdws <- c(MASS::bandwidth.nrd(x), MASS::bandwidth.nrd(y))
    if (all(bdws > 0)) {

      ltemp <- length(x) < 50 && min(bdws) < 1 && diff(range(bdws)) > 75
      n <- if (ltemp) 10 else 100

      # 2-dimensional kernel density estimate of xy-points
      kerneld <- MASS::kde2d(x, y, n = n, h = bdws, lims = c(xlim_kd, ylim_kd))

      # Extract density value for each xy-point
      pp <- vector(mode = "numeric", length = length(x))
      for (i in seq_along(x)) {
          z.x <- which.min(abs(kerneld$x - x[i]))
          z.y <- which.min(abs(kerneld$y - y[i]))
          pp[i] <- kerneld$z[z.x, z.y]
      }

      # Convert \code{alpha * 100}-percent value to corresponding density value
      confidencebound <- stats::quantile(pp, 1 - alpha, na.rm = TRUE)

      # Add contour(s) to plot
      if (fill) {
        cl <- grDevices::contourLines(kerneld, levels = confidencebound)
        cols <- rep_len(col, length(cl))
        for (cli in seq_along(cl)) {
          graphics::polygon(x = cl[[cli]]$x, y = cl[[cli]]$y, border = NA,
            col = grDevices::adjustcolor(cols[cli], alpha.f = 0.3), ...)
        }

      } else {
        graphics::contour(kerneld, levels = confidencebound, col = col,
          drawlabels = FALSE, xlim = xlim, ylim = ylim, add = TRUE, ...)
      }

      res <- TRUE
    }
  }

  invisible(res)
}


#' Add isoline(s) to a \var{xy}-plot
#'
#' The isolines can either represent the \code{probs * 100}-percent
#' quantile or represent fixed \code{levels} of the z-variable values
#'
#' @export
add_isoline_to_pointcloud <- function(x, y, z, probs = NULL, levels = NULL,
  col = "red", fill = FALSE, na.rm = FALSE, add_legend = FALSE,
  legend_xy = NULL, ...) {

  if (!is.null(probs)) {
    col <- rep_len(col, length(probs))
    levels <- stats::quantile(z, probs = probs, type = 8, na.rm = na.rm,
      names = FALSE)

  } else if (!is.null(levels)) {
    col <- rep_len(col, length(levels))

  } else stop("One of the arguments `probs` and `levels` must be specified.")

  for (k in seq_along(levels)) {
    ids <- z >= levels[k]
    if (any(ids)) {
      add_contour_to_pointcloud(x = x[ids], y = y[ids], alpha = 1,
        col = col[k], fill = fill, na.rm = na.rm, ...)
    }
  }

  if (add_legend && !is.null(legend_xy)) {
    graphics::legend(legend_xy, legend = signif(levels, 3), col = col, lwd = 2)
  }

  invisible(TRUE)
}



#' @export
add_loess <- function(x, y, xlim = NULL, res = 30L, col = "orange", lwd = 2) {
  if (is.null(xlim)) {
    xlim <- range(x, na.rm = TRUE)
  }

  if (all(is.finite(xlim)) && xlim[1] < xlim[2]) {
    xs <- seq(xlim[1], xlim[2], length.out = as.integer(res))
    lloess <- try(stats::loess(y ~ x), silent = TRUE)
    if (!inherits(lloess, "try-error")) {
      graphics::lines(
        xs,
        stats::predict(lloess, newdata = data.frame(x = xs)),
        col = col[1], lwd = lwd[1]
      )
    }
  }
}

#' Add a smoothed scatter plot to the current panel
#'
#' @export
draw_smoothScatter_panel <- function(x, y, asp = NA, xlim = NULL, ylim = xlim,
  loess_lim = xlim, add_loess = TRUE, add_1to1 = FALSE,
  xlab = "", ylab = "", is_plotmath = c(FALSE, FALSE),
  annx = TRUE, anny = TRUE, fexp_axis = 1) {

  if (is.null(xlim)) {
    xlim <- range(x, na.rm = TRUE)
  }

  if (is.null(ylim)) {
    ylim <- range(y, na.rm = TRUE)
  }

  tmp <- try(graphics::smoothScatter(x, y,
    nbin = 2 * 128, nrpoints = 0,
    asp = asp, xlim = xlim, ylim = ylim,
    main = "", xlab = "", ylab = "",
    axes = FALSE
  ))

  if (inherits(tmp, "try-error")) {
    graphics::plot(x, y,
      asp = asp,
      xlim = xlim, ylim = ylim,
      main = "", xlab = "", ylab = "",
      axes = FALSE
    )
  }

  if (add_loess) {
    try(add_loess(x, y, loess_lim))
  }

  if (add_1to1) {
    graphics::abline(a = 0, b = 1, col = "red", lwd = 2)
  }
  graphics::abline(h = 0, col = "darkgray", lty = 2, lwd = 2)
  graphics::abline(v = 0, col = "darkgray", lty = 2, lwd = 2)

  # Add axes
  graphics::axis(side = 1, labels = annx, cex.axis = fexp_axis)
  if (annx) {
    graphics::mtext(side = 1, line = fexp_axis, cex = fexp_axis,
      text = if (is_plotmath[1]) parse(text = xlab) else xlab)
  }

  graphics::axis(side = 2, labels = anny, cex.axis = fexp_axis)
  if (anny) {
    graphics::mtext(side = 2, line = fexp_axis, cex = fexp_axis,
      text = if (is_plotmath[2]) parse(text = ylab) else ylab
    )
  }
}
