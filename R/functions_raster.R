
#' Converts values associated with geographic locations into a \var{Raster*}
#'
#' @param data A vector or two-dimensional object.
#' @param site_locations An object that described geographic locations of
#'   \code{data} that can be sent to \code{\link[rSW2st]{as_points}}
#' @param site_crs The \code{crs} of \code{site_locations}.
#' @param grid A \var{Raster*} object used as template.
#' @param grid_crs The \code{crs} of \code{grid}.
#' @param filename A character string. Passed to \code{\link[raster]{brick}}.
#'
#' @return A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'   (if \code{data} is a vector) or
#'   \code{\link[raster:RasterBrick-class]{raster::RasterBrick}}
#'   (if \code{data} is two-dimensional).
#'
#' @export
create_raster_from_variables <- function(
  data,
  site_locations,
  grid,
  site_crs = sf::st_crs(site_locations),
  filename = ""
) {

  # prepare locations
  loc <- rSW2st::as_points(site_locations, to_class = "sf", crs = site_crs)

  if (sf::st_crs(loc) != sf::st_crs(grid)) {
    loc <- sf::st_transform(loc, crs = sf::st_crs(grid))
  }

  coords <- sf::st_coordinates(loc)

  # prepare data
  nl <- NCOL(data)
  cnames <- colnames(data)

  if (!is.numeric(data)) {
    if (nl > 1) {
      for (k in seq_len(nl)) {
        tmp <- try(if (is.factor(data[, k])) {
          as.integer(data[, k])
        } else {
          as.double(data[, k])
        })
        stopifnot(!inherits(tmp, "try-error"))
        data[, k] <- tmp
      }

    } else {
      data <- try(if (is.factor(data)) {
        as.integer(data)
      } else {
        as.double(data)
      })
      stopifnot(!inherits(data, "try-error"))
    }
  }

  if (nl == 1) {
    data <- matrix(data, ncol = 1)
  }

  # create raster, init with NAs, and add data
  ids <- NULL
  rl <- list()
  if (nl > 1) {
    filenameks <- sapply(seq_len(nl), raster::rasterTmpFile)
  }

  for (k in seq_len(nl)) {
    rk <- raster::init(grid, fun = function(x) rep(NA, x))
    if (k == 1) {
      ids <- raster::cellFromXY(rk, xy = coords)
    }

    rk[ids] <- data[, k]

    if (nl > 1) {
      rk <- raster::writeRaster(rk, filename = filenameks[k])
    }

    rl <- c(rl, rk)
  }

  if (nl > 1) {
    names(rl) <- cnames
    # first convert list to stack before passing to brick because
    # raster v2.9.6 the list-method of brick ignores all ... arguments
    r <- raster::brick(raster::stack(rl), filename = filename)
    unlink(filenameks)

  } else {
    r <- rl[[1]]
  }


  # set datatype
  raster::dataType(r) <- get_raster_datatype(data)

  r
}



#' Convert \code{\link{typeof}} to \code{\link[raster]{dataType}} types
#'
#' @references Relevant code adapted from \code{`raster:::dataType<-`}
#' @export
get_raster_datatype <- function(data) {
  switch(EXPR = substr(toupper(typeof(data)), 1, 5),
    LOGIC = "LOG1S",
    BYTE = "INT1U",
    SMALL = "INT2S",
    INTEG = "INT4S",
    NUMER = , FLOAT = , SINGL = , REAL = "FLT4S", #nolint
    DOUBL = "FLT8S"
    )
}



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
    SFSW2_prj_meta = SFSW2_prj_meta,
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
