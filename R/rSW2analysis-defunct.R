#' Defunct functions in package \pkg{rSW2analysis}
#'
#' Executing a defunct function will fail and tell you which function
#' replaces them.
#'
#' @name rSW2analysis-defunct
NULL


#' @rdname rSW2analysis-defunct
#' @export
calculate_cell_area <- function(...) {
  .Defunct(
    new = "calculate_cell_area",
    package = "rSW2funs",
    msg = paste(
      "`rSW2analysis::calculate_cell_area()`",
      "is defunct;",
      "please use",
      "`rSW2st::calculate_cell_area()`",
      "instead."
    )
  )
}
