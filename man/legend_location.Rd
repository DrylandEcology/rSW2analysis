% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_functions_miscellaneous.R
\name{legend_location}
\alias{legend_location}
\title{Determine location for a legend based on lowest density of points}
\usage{
legend_location(x, y = NULL, xlim = NULL, ylim = NULL)
}
\arguments{
\item{x}{the x and y coordinates of a set of points.
    Alternatively, a single argument \code{x} can be provided.}

\item{y}{the x and y coordinates of a set of points.
    Alternatively, a single argument \code{x} can be provided.}

\item{xlim}{A numeric vector of length two. The \var{x-axis} limits;
derived from data if missing.}

\item{ylim}{A numeric vector of length two. The \var{y-axis} limits;
derived from data if missing.}
}
\value{
A (character vector) keyword indicating suitable legend location.
}
\description{
Determine location for a legend based on lowest density of points
}
\examples{
xy <- data.frame(
  x = c(rep(1:3, times = 1:3), rep(1:3, times = 3 + 1:3)),
  y = rep(1:2, times = c(6, 15))
)

legend_location(xy)
legend_location(xy, xlim = c(0, 5))
pos <- legend_location(xy, xlim = c(0, 4), ylim = c(1, 4))

if (requireNamespace("KernSmooth")) {
  graphics::smoothScatter(xy, xlim = c(0, 4), ylim = c(1, 4))
} else {
  graphics::plot(xy, xlim = c(0, 4), ylim = c(1, 4))
}
graphics::legend(pos, legend = pos, fill = "black")

}
\seealso{
\code{\link[graphics]{legend}}, \code{\link[grDevices]{xy.coords}}
}
