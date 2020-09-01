
#' Converts values into a Raster*
#'
#' @param SFSW2_prj_meta An environment
#' @param data A vector or two-dimensional object.
#' @param locations A numeric matrix or data.frame with coordinates. Each row is
#'   a point.
#' @param crs A CRS object.
#' @param filename A character string. Passed to \code{\link[raster]{brick}}.
#'
#' @return A \code{\link[raster:RasterLayer-class]{raster::RasterLayer}}
#'   (if \code{data} is a vector) or
#'   \code{\link[raster:RasterBrick-class]{raster::RasterBrick}}
#'   (if \code{data} is two-dimensional).
#'
#' @export
create_raster_from_variables <- function(SFSW2_prj_meta = NULL, locations,
                                         crs, data, filename = "") {
  
  
  ###############################################################################
  #### Prepare location & spatial data -------------------------------------------
  ###############################################################################
  if(is.null(SFSW2_prj_meta) && missing(locations)){
    stop('Missing both SFSW2_prj_meta and locations. Need one or the other to proceed')
  }

  # From SOILWAT2 SFSW2_prj_meta data ----------------
  if(!is.null(SFSW2_prj_meta)) {
    loc <- SFSW2_prj_meta[["sim_space"]][["run_sites"]]

    # if not the same CRS spTransform
    if (!raster::compareCRS(SFSW2_prj_meta[["sim_space"]][["crs_sites"]],
      SFSW2_prj_meta[["sim_space"]][["sim_crs"]])) {
        loc <- sp::spTransform(loc,
          CRS = SFSW2_prj_meta[["sim_space"]][["sim_crs"]])
    }
  }

  # From user supplied location and CRS ----------------
  if(!missing(locations)) {
    loc <- SpatialPoints(locations)
    proj4string(loc) <- CRS(crs)
  }

  ############################################################################
  #### Prepare and check data  -------------------------------------------
  ############################################################################
  nl <- NCOL(data)
  cnames <- colnames(data)

  if (!is.numeric(data)) {
    if (nl > 1) {
      for (k in seq_len(nl)) {
        temp <- try(if (is.factor(data[, k])) {
            as.integer(data[, k])
          } else {
            as.double(data[, k])
          })
        stopifnot(!inherits(temp, "try-error"))
        data[, k] <- temp
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

  ###############################################################################
  #### Create raster, init with NAs, add data  ----------------------------------
  ###############################################################################

  ids <- NULL
  rl <- list()
  if (nl > 1) {
    filenameks <- sapply(seq_len(nl), raster::rasterTmpFile)
  }

  for (k in seq_len(nl)) {
    if(!is.null(SFSW2_prj_meta)) {
      rk <- raster::raster(SFSW2_prj_meta[["sim_space"]][["sim_raster"]])
    } 
    if(!missing(locations)) {
      rk <- raster::raster(loc)
      extent(rk) <- extent(loc)
    }
    rk <- raster::init(rk, fun = function(x) rep(NA, x))
    if (k == 1) {
      ids <- raster::cellFromXY(rk, sp::coordinates(loc))
    }
    # add values -------------------
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
    NUMER = , FLOAT = , SINGL = , REAL = "FLT4S",
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
#' Function converts data to a raster object and then extracts draws a
#' polygon around those gridcells with a value larger than alpha
#'
#' @param x A numeric vector
#' @param SFSW2_prj_meta An environment
#' @param subset A logical vector of length equal to \code{x}
#' @param alpha A numeric value
#'
#' @return A \code{\link[sp]{SpatialPolygons}} object.
#'
#' @export
get_isoline_polygon <- function(x, SFSW2_prj_meta, subset, alpha) {
  if (!missing(subset)) {
    x[!subset] <- NA
  }

  rtmp <- create_raster_from_variables(
    SFSW2_prj_meta = SFSW2_prj_meta,
    data = x)

  rtmp <- raster::calc(rtmp,
    fun = function(x) ifelse(x >= alpha, 1L, NA))

  raster::rasterToPolygons(rtmp, dissolve = TRUE)
}
