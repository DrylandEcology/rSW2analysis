% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/figures_scatter.R
\name{draw_smoothScatter_panel}
\alias{draw_smoothScatter_panel}
\title{Add a smoothed scatter plot to the current panel}
\usage{
draw_smoothScatter_panel(
  x,
  y,
  asp = NA,
  xlim = NULL,
  ylim = xlim,
  loess_lim = xlim,
  add_loess = TRUE,
  add_1to1 = FALSE,
  xlab = "",
  ylab = "",
  is_plotmath = c(FALSE, FALSE),
  annx = TRUE,
  anny = TRUE,
  fexp_axis = 1
)
}
\description{
Add a smoothed scatter plot to the current panel
}
