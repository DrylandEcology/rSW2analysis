

#' Mix n colors using values of \code{x} as weights
#'
#' @param x A two-dimensional numerical object. Each row produces one
#'   mixed color. The number of columns must correspond to the number of
#'   \code{colors}.
#' @param colors A vector, list, or two-dimensional object of colors
#'   to be mixed. If a two-dimensional object, then the columns must
#'   represent the red, green, and blue values of RGB colors
#'   (see \code{\link[grDevices]{rgb}}).
#'   If a vector or list, then colors must be specified in on of the
#'   formats supported by \code{\link[grDevices]{col2rgb}}.
#'
#' @return A vector of the mixed colors. The format is \R's
#'   hexadecimal color codes \var{#rrggbb} (see \code{\link[grDevices]{rgb}}),
#'   which can be used as \code{col =} argument of graphics functions or in
#'   \code{\link[graphics]{par}}.
#'
#' @details Any \code{NA} value in a row of \code{x} will translate to a
#'  corresponding \code{NA} in the returned vector.
#'
#' @seealso \code{\link[colorspace]{mixcolor}} for mixing two colors.
#'
#' @examples
#' ## Mixing some colors
#' xw <- rbind(
#'   c(0.5, 0.25, 0.25, 0),
#'   c(0.25, 0, 0.75, 0),
#'   c(0, 0, 1, 0),
#'   c(1, 0, 0, 0),
#'   c(0.25, 0.25, 0.25, 0.25),
#'   c(0.1, 0.1, 0.1, 0.7)
#' )
#' cols <- c("red", "orange", "purple", "yellow")
#'
#' # Example where `colors` is a vector of color names
#' mix_colors(x = xw, colors = cols)
#'
#' # Example where `colors` is a list of color names
#' mix_colors(x = xw, colors = as.list(cols))
#'
#' if (requireNamespace("grDevices")) {
#'   # Example where `colors` is a list of hexadecimal color codes #rrggbb
#'   mix_colors(
#'     x = xw,
#'     colors = lapply(
#'       cols,
#'       function(col) {
#'         col <- t(grDevices::col2rgb(col))
#'         grDevices::rgb(col, maxColorValue = 255)
#'       }
#'     )
#'   )
#'
#'   # Example where `colors` is a matrix of RGB colors
#'   mix_colors(x = xw, colors = t(grDevices::col2rgb(cols)))
#' }
#'
#'
#' ## Mixing two colors produces almost the same results as the
#' ## function `mixcolor` (but not exactly due to rounding to integers when
#' ## converting to the 255-scale used by `col2rgb`)
#'
#' if (requireNamespace("colorspace")) {
#'   res1 <- colorspace::mixcolor(
#'     alpha = 0.25,
#'     colorspace::RGB(1, 0, 0),
#'     colorspace::RGB(0, 1, 0)
#'   )
#'   res1 <- colorspace::coords(res1)
#'
#'   res2 <- mix_colors(
#'     x = c(0.75, 0.25),
#'     colors = list(
#'       grDevices::rgb(1, 0, 0),
#'       grDevices::rgb(0, 1, 0)
#'     )
#'   )
#'   res2 <- grDevices::col2rgb(res2)
#'   res2 <- res2 / sum(res2)
#'
#'   isTRUE(all.equal(as.vector(res1), as.vector(res2), tolerance = 1 / 254))
#' }
#'
#' @export
mix_colors <- function(x, colors) {
  # Obtain colors in RGB coordinates
  isdf <- is.data.frame(colors)
  if (is.matrix(colors) || isdf) {
    stopifnot(ncol(colors) == 3)
    if (isdf) {
      colors <- unname(as.matrix(colors))
    }

    colors <- t(colors)

  } else if (is.vector(colors) || is.list(colors)) {
    colors <- sapply(colors, grDevices::col2rgb)

  } else {
    stop("Unknown format of argument `color`.")
  }

  Ncols <- ncol(colors)

  # Make sure weights are well defined
  if (!is.matrix(x)) {
    x <- matrix(x, ncol = Ncols)
  }

  stopifnot(ncol(x) == Ncols)

  # Propagate NAs
  xsum <- apply(x, 1, sum)
  ids_good <- !is.na(xsum)

  # Make sure that weights sum to 1 for each case
  if (any(xsum > 1, na.rm = TRUE)) {
    x <- sweep(x, MARGIN = 1, STATS = xsum, FUN = "/")
  }

  # Calculate linear combinations of colors
  mixed <- tcrossprod(x, colors)

  # Convert RGB codes to R colors
  res <- rep(NA, length = nrow(x))
  res[ids_good] <- grDevices::rgb(
    mixed[ids_good, , drop = FALSE],
    maxColorValue = if (max(colors) > 1) 255 else 1
  )

  res
}


#' Define a \var{colorramp} from minimum to maximum
#' @export
define_value_colorramp <- function(zlim, Nc = 255,
  cols = c("gray", "gold", "red")) {

  cramp <- grDevices::colorRampPalette(cols)(Nc)

  list(
    panel = list(cols = cramp, zlim = zlim),
    legend = list(cols = cramp, zlim = zlim,
      desc = list(
        colors_label = cramp,
        ncol_label_added = 0,
        added_below = FALSE,
        added_above = FALSE))
  )
}

#' Define a \var{colorramp} based one set of colors for the negative range and
#' another set of colors for the positive range
#' @export
define_delta_colorramp <- function(zlim, zlim_trim = NULL, Nc = 255,
  use_rev_cols = FALSE, add_extremes_to_legend = FALSE,
  col_zero = "gray",
  cols_blues = c("royalblue", "cornflowerblue", "lightblue"),
  cols_reds = c("lightsalmon", "mediumorchid1", "purple", "purple4")) {

  col_blue_extreme <- grDevices::adjustcolor(cols_blues[1],
    green.f = 0.5, red.f = 0.5, blue.f = 0.5)
  col_red_extreme <- grDevices::adjustcolor(cols_reds[length(cols_reds)],
    green.f = 0.5, red.f = 0.5, blue.f = 0.5)

  colramp <- if (use_rev_cols) {
      list(
        neg = grDevices::colorRampPalette(rev(cols_reds)),
        neg_extreme = col_red_extreme,
        pos = grDevices::colorRampPalette(rev(cols_blues)),
        pos_extreme = col_blue_extreme)
    } else {
      list(
        neg = grDevices::colorRampPalette(cols_blues),
        neg_extreme = col_blue_extreme,
        pos = grDevices::colorRampPalette(cols_reds),
        pos_extreme = col_red_extreme)
    }

  zlim_legend <- if (is.null(zlim_trim)) zlim else zlim_trim
  Nc_per_unit <- Nc / sum(abs(zlim_legend))

  if (is.finite(Nc_per_unit)) {
    ntemp <- abs(round(Nc_per_unit * zlim_legend))
    ctemp1 <- colramp[["neg"]](ntemp[1])
    ctemp2 <- colramp[["pos"]](ntemp[2])
    ctemp0 <- c(ctemp1, ctemp2)
    if (!is.null(col_zero)) {
      ctemp0[ntemp[1] + c(0, 1)] <- col_zero
    }

    if (add_extremes_to_legend) {
      temp <- ceiling(0.05 * sum(ntemp))
      ncol_label_added <- if (temp > 0) max(5L, temp) else 0L
      cramp_legend <- c(
        rep(colramp[["neg_extreme"]], ncol_label_added),
        ctemp0,
        rep(colramp[["pos_extreme"]], ncol_label_added))

    } else {
      ncol_label_added <- 0
      cramp_legend <- ctemp0
    }

    ntemp2 <- abs(round(Nc_per_unit * (zlim - zlim_legend)))
    cramp <- c(
      rep(colramp[["neg_extreme"]], ntemp2[1]),
      ctemp0,
      rep(colramp[["pos_extreme"]], ntemp2[2]))

  } else {
    ncol_label_added <- 0
    cramp <- cramp_legend <- NA
  }

  list(
    panel = list(cols = cramp, zlim = zlim),
    legend = list(
      cols = cramp_legend,
      zlim = zlim_legend,
      desc = list(
        colors_label = cramp_legend,
        ncol_label_added = ncol_label_added,
        added_below = add_extremes_to_legend,
        added_above = add_extremes_to_legend
      )
    )
  )
}


#' Prepare colors for plots
#' @export
prepare_colors <- function(type, zlim, col_rev = FALSE, col_zero = "gray") {

  colvir <- viridisLite::viridis(n = 255, direction = -1)
  colmag <- viridisLite::magma(n = 255, direction = -1)
  colblu <- viridisLite::inferno(122, begin = 0.4, end = 0.9)
  colred <- viridisLite::viridis(122, begin = 0.3, end = 0.8, direction = -1)

  switch(type,
    span = , #nolint
    agreement = define_value_colorramp(
      zlim = zlim,
      cols = if (col_rev) rev(c(col_zero, colmag)) else c(col_zero, colmag)
    ),
    agreement_sign = define_delta_colorramp(
      zlim = zlim,
      col_zero = col_zero,
      cols_blues = if (col_rev) rev(colred) else colblu,
      cols_reds = if (col_rev) rev(colblu) else colred
    ),
    vals = define_value_colorramp(
      zlim = zlim,
      cols = if (col_rev) rev(c(col_zero, colvir)) else c(col_zero, colvir)
    ),
    delta = define_delta_colorramp(
      zlim = zlim,
      col_zero = col_zero, use_rev_cols = !col_rev
    ),
    cats = , #nolint
    composite_cats = NA
  )
}


#' @export
add_panel_identifier <- function(i, add_label = FALSE, label = "",
  is_plotmath = FALSE, fexp = 1) {

  add_label <- add_label && !is.null(label) && nchar(as.character(label)) > 0

  txt <- if (add_label) {
    temp <- paste0("(", letters[i], "): ", label)
    # parse string containing plotmath expression
    if (is_plotmath) parse(text = temp) else temp
  } else {
    paste0("(", letters[i], ")")
  }

  graphics::mtext(
    side = 3,
    line = if (add_label) 0 else -1.5,
    adj = 0.05,
    font = 2,
    cex = fexp,
    text = txt
  )
}



#' Determine location for a legend based on lowest density of points
#'
#' @inheritParams grDevices::xy.coords
#' @param xlim A numeric vector of length two. The \var{x-axis} limits;
#'   derived from data if missing.
#' @param ylim A numeric vector of length two. The \var{y-axis} limits;
#'   derived from data if missing.
#'
#' @seealso \code{\link[graphics]{legend}}, \code{\link[grDevices]{xy.coords}}
#' @return A (character vector) keyword indicating suitable legend location.
#'
#' @examples
#' xy <- data.frame(
#'   x = c(rep(1:3, times = 1:3), rep(1:3, times = 3 + 1:3)),
#'   y = rep(1:2, times = c(6, 15))
#' )
#'
#' legend_location(xy)
#' legend_location(xy, xlim = c(0, 5))
#' pos <- legend_location(xy, xlim = c(0, 4), ylim = c(1, 4))
#'
#' if (requireNamespace("KernSmooth")) {
#'   graphics::smoothScatter(xy, xlim = c(0, 4), ylim = c(1, 4))
#' } else {
#'   graphics::plot(xy, xlim = c(0, 4), ylim = c(1, 4))
#' }
#' graphics::legend(pos, legend = pos, fill = "black")
#'
#' @export
legend_location <- function(x, y = NULL, xlim = NULL, ylim = NULL) {
  xy <- grDevices::xy.coords(x, y)

  xns <- c("left", "center", "right")
  yns <- c("bottom", "top")

  dxy <- expand.grid(yns, xns)
  dxy[, "pos"] <- apply(dxy, 1, paste, collapse = "")
  dxy[, "n"] <- 0

  xlim <- if (is.null(xlim)) c(min(xy[[1]]), max(xy[[1]])) else xlim
  ipsx <- findInterval(
    x = xy[[1]],
    vec = xlim[1] + 0:3 / 3 * (xlim[2] - xlim[1]),
    all.inside = TRUE
  )
  ipsx <- xns[ipsx]

  ylim <- if (is.null(ylim)) c(min(xy[[2]]), max(xy[[2]])) else ylim
  ipsy <- findInterval(
    x = xy[[2]],
    vec = ylim[1] + 0:2 / 2 * (ylim[2] - ylim[1]),
    all.inside = TRUE
  )
  ipsy <- yns[ipsy]

  ips <- paste0(ipsy, ipsx)
  fips <- table(ips)

  ids <- match(dxy[, "pos"], names(fips), nomatch = 0)
  dxy[ids > 0, "n"] <- fips[ids]

  sub("center", "", dxy[which.min(dxy[, "n"]), "pos"])
}
