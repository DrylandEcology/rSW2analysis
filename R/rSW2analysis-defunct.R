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



#' @rdname rSW2analysis-defunct
#' @export
get_isoline_polygon <- function(...) {
  .Defunct(
    new = "isoline_from_raster",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::get_isoline_polygon()`",
      "is defunct;",
      "please use",
      "`rSW2st::isoline_from_raster()`",
      "instead."
    )
  )
}




#' @rdname rSW2analysis-defunct
#' @export
variogram_range <- function(...) {
  .Defunct(
    new = "variogram_range",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::variogram_range()`",
      "is defunct;",
      "please use",
      "`rSW2st::variogram_range()`",
      "instead."
    )
  )
}





#' @rdname rSW2analysis-defunct
#' @export
create_empty_netCDF_file <- function(...) {
  .Defunct(
    new = "create_netCDF",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::create_empty_netCDF_file()`",
      "is defunct;",
      "please use",
      "`rSW2st::create_netCDF()`",
      "instead."
    )
  )
}

#' @rdname rSW2analysis-defunct
#' @export
populate_netcdf_from_array <- function(...) {
  .Defunct(
    new = "populate_netCDF",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::populate_netcdf_from_array()`",
      "is defunct;",
      "please use",
      "`rSW2st::populate_netCDF()` or directly `rSW2st::create_netCDF()`",
      "instead."
    )
  )
}


#' @rdname rSW2analysis-defunct
#' @export
read_netCDF_to_array <- function(...) {
  .Defunct(
    new = "read_netCDF",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::read_netCDF_to_array()`",
      "is defunct;",
      "please use",
      "`rSW2st::read_netCDF()` or `rSW2st::read_netCDF_as_array()`",
      "instead."
    )
  )
}



#' @rdname rSW2analysis-defunct
#' @export
read_netCDF_to_raster <- function(...) {
  .Defunct(
    new = "read_netCDF",
    package = "rSW2st",
    msg = paste(
      "`rSW2analysis::read_netCDF_to_raster()`",
      "is defunct;",
      "please use",
      "`rSW2st::read_netCDF()` or `rSW2st::read_netCDF_as_raster()`",
      "instead."
    )
  )
}
