
#' Calculate range of variogram in kilometers
#'
#' @param x A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'   in \var{un}-projected coordinates (i.e., longitude/latitude)
#'
#' @export
variogram_range <- function(x, project_to_utm = TRUE,
  sub_samplepoints_N = NULL) {

  stopifnot(requireNamespace("sp"), requireNamespace("raster"),
    requireNamespace("automap"), requireNamespace("gstat"))

  # Create spatial points from raster
  points <- raster::rasterToPoints(x, spatial = TRUE)

  if (!is.null(sub_samplepoints_N)) {
    set.seed(2017)
    temp <- sample(seq_len(nrow(points)), sub_samplepoints_N,
      replace = FALSE)
    points <- points[temp, ]
  }

  if (project_to_utm) {
    # if region, then project to UTM coordinates with distance units of km
    # --> variogram will calculate Euclidean distances in map units
    xytemp <- sp::coordinates(points)
    utm_zone <- rSW2st::get_UTM_Zone(xytemp[, 1], xytemp[, 2])
    utm_NS <- if (mean(xytemp[, 2]) > 0) " +north" else " +south"
    points_prj <- sp::spTransform(points,
      CRSobj = sp::CRS(paste0("+proj=utm +zone=", utm_zone, utm_NS,
        " +datum=WGS84 +units=km +no_defs")))

  } else {
    # if global, then don't project, i.e. use long/lat with WGS84 datum
    # and distance units of meters
    # --> variogram will calculate great circle distances in kilometers(!)
    points_prj <- sp::spTransform(points,
      CRSobj = sp::CRS("+proj=longlat +datum=WGS84 +units=m +no_defs"))
  }

  names(points_prj) <- "target"

  # determine variogram; see gstat::variogram
  fittedVar <- automap::autofitVariogram(target ~ 1,
    input_data = points_prj,
    miscFitOptions = list(merge.small.bins = TRUE))

  # variogram range in km:
  # estimate is only valid if some variation in data (i.e., sill > 0)
  psill_sum <- sum(fittedVar[["var_model"]][, "psill"])

  if (abs(psill_sum) > sqrt(.Machine$double.eps)) {
    fittedVar[["var_model"]][2, "range"]
  } else NaN
}
