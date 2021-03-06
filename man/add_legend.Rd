% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_maps.R
\name{add_legend}
\alias{add_legend}
\title{Create and plot a continuous color legend}
\usage{
add_legend(
  zlim,
  zextreme,
  col_desc,
  grid,
  box = c(-100, -97, -50, -10),
  whitebox = TRUE,
  horiz = FALSE,
  signed = 1,
  fun_inv_ens = NULL,
  at = NULL,
  tick = TRUE,
  labels = TRUE,
  srt = 90,
  cex = 1
)
}
\arguments{
\item{zlim}{A numeric vector of length two. The smallest and largest z-value
of \code{grid} for which a complete color gradient and legend should be
printed.}

\item{zextreme}{A numeric vector length two. The minimum and maximum z-value
of \code{grid}. If \code{zextreme[1] < zlim[1] || zextreme[2] > zlim[2]},
then the values in the ranges between each \code{zextreme} and \code{zlim}
 will be highlighted as extreme areas in a different color.}

\item{col_desc}{A list with the color names. A returned object from a call to
the function \code{\link{prepare_colors}}.}

\item{grid}{A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
object for which the legend is created.}

\item{box}{A numeric vector of length four. The SW (lower left) and NE (upper
right) corners of the color legend on the plotted \code{grid}.}

\item{whitebox}{A logical value. If \code{TRUE}, then the area of the color
legend is printed white before the colors of the legend are added.}

\item{horiz}{A logical value. If \code{TRUE}, then the values of the color
legend are printed below the legend box; if \code{FALSE}, then the values
are printed on the right side of the box.}

\item{signed}{A numeric value. A scaling value multiplied with the z-values
for determining legend values.}

\item{fun_inv_ens}{A function or \code{NULL}. If a function, then it will be
applied to z-values for determining legend values.}

\item{at}{The points at which tick-marks are to be drawn. By default
(when \code{NULL}) tickmark locations are computed,
see \code{\link[graphics]{axis}}.}

\item{tick}{A logical value. Indicates whether or not tickmarks should be
added to the legend color ramp.}

\item{labels}{The annotations to be written at the tick-marks,
see \code{\link[graphics]{axis}}.}

\item{srt}{A numerical value. String rotation in degrees, see
\code{\link[graphics]{text}}.}

\item{cex}{A numerical value. Numeric character expansion factor, see
\code{\link[graphics]{text}}.}
}
\description{
Create and plot a continuous color legend
}
