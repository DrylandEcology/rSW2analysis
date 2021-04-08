
#' @export
find_siteIDs <- function(meta, xy, proj4string = NULL) {
  if (is.null(proj4string)) {
    proj4string <- raster::crs("+init=epsg:4326")
  }

  loc <- sp::SpatialPoints(coords = xy, proj4string = proj4string)
  loc <- sp::spTransform(loc, CRS = meta[["sim_space"]][["sim_crs"]])

  r <- raster::raster(meta[["sim_space"]][["sim_raster"]])
  r <- raster::init(r, fun = function(x) rep(0, x))
  ids <- raster::cellFromXY(r, sp::coordinates(loc))
  r[ids] <- 1

  ids <- raster::extract(r, meta[["sim_space"]][["run_sites"]])
  as.logical(ids)
}
