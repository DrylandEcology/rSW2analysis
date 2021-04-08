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
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::calculate_cell_area()`",
      "is defunct;",
      "please use",
      "`rSW2st::calculate_cell_area()`",
      "instead."
    )
  )
}


#' @rdname rSW2analysis-defunct
#' @export
calculate_nominal_resolution <- function(...) {
  .Defunct(
    new = "calculate_nominal_resolution",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::calculate_nominal_resolution()`",
      "is defunct;",
      "please use",
      "`rSW2st::calculate_nominal_resolution()`",
      "instead."
    )
  )
}



#' @rdname rSW2analysis-defunct
#' @export
create_raster_from_variables <- function(...) {
  .Defunct(
    new = "create_raster_from_variables",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::create_raster_from_variables()`",
      "is defunct;",
      "please use",
      "`rSW2st::create_raster_from_variables()`",
      "instead."
    )
  )
}
