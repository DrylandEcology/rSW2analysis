
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


#' Creates an isoline-polygon from data where values are larger than alpha
#'
#' Function converts data to a raster object and then draws a
#' polygon around those gridcells with a value larger than alpha
#'
#' @param x A numeric vector
#' @inheritParams create_raster_from_variables
#' @param subset A logical vector of length equal to \code{x}
#' @param alpha A numeric value
#'
#' @return A \code{\link[sp]{SpatialPolygons}} object.
#'
#' @export
get_isoline_polygon <- function(x, site_locations, grid, subset, alpha) {
  if (!missing(subset)) {
    x[!subset] <- NA
  }

  rtmp <- create_raster_from_variables(
    data = x,
    site_locations = site_locations,
    grid = grid
  )

  rtmp <- raster::calc(
    rtmp,
    fun = function(x) ifelse(x >= alpha, 1L, NA)
  )

  raster::rasterToPolygons(rtmp, dissolve = TRUE)
}
