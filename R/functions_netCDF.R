#' Convert raster where variables are organized in the third dimension to a
#' \var{netCDF} file
#'
#' @param x A Raster* object.
#' @param var_attributes A list of named character strings each of length
#'   equal to the number of variables, i.e., the third dimension of \code{x}.
#'   If not missing, then the variable attributes will be added to the
#'   \var{netCDF} file. If no variable names are provided, then the names of
#'   \code{x} are used; otherwise, these are added as \code{original_name}.
#' @param global_attributes A list of named character strings. If not missing,
#'   then the global attributes will be added to the \var{netCDF} file.
#' @param file A character string. The file path of the \var{netCDF} file to be
#'   created.
#' @param overwrite A logical value. If \code{TRUE}, file will be overwritten
#'   if it exists.
#'
#'  @return This function is used for the side-effect of creating a file.
#'
#' @export
create_netCDF_from_raster_with_variables <- function(x, time_bounds,
  var_attributes, global_attributes, file, force_v4 = TRUE,
  overwrite = FALSE) {

  stopifnot(requireNamespace("ncdf4"))
  if (force_v4) {
    # avoid "_FillValue" error in older versions of `raster` package
    stopifnot(utils::packageVersion("raster") >= "2.9.1")
  }

  nl <- raster::nlayers(x)
  var_names <- var_longnames <- names(x)
  var_units <- rep("", nl)

  ncdf4_datatype <- raster:::.getNetCDFDType(raster::dataType(x))

  NAflag <- raster::NAvalue(x)
  if (is.infinite(NAflag)) {
    # values: \code{\link[raster]{dataType}} and \code{`raster:::dataType<-`}
    NAflag <- switch(ncdf4_datatype,
      char = NULL, byte = NULL, short = -128L, integer = -2147483647L,
      float = -3.4e+38, double = -1.7e+308)
  }

  # Note: raster files are organized starting from NE corner
  xvals <- raster::xFromCol(x, seq_len(raster::ncol(x)))
  yvals <- raster::yFromRow(x, seq_len(raster::nrow(x)))
  grid_halfres <- raster::res(x) / 2

  has_time_central <- !missing(time_bounds)
  if (has_time_central) {
    stopifnot(length(time_bounds) == 2L)
    time_central <- mean(time_bounds)
  }

  if (!missing(var_attributes)) {
    if ("name" %in% names(var_attributes)) {
      var_names <- var_attributes[["name"]]
      var_attributes[["name"]] <- NULL
    }

    if ("long_name" %in% names(var_attributes)) {
      var_longnames <- var_attributes[["long_name"]]
      var_attributes[["long_name"]] <- NULL
    }

    if (is.null(var_longnames) && !is.null(var_names)) {
      var_longnames <- var_names
    }

    if ("units" %in% names(var_attributes)) {
      var_units <- var_attributes[["units"]]
      var_attributes[["units"]] <- NULL
    }
  }

  if (!missing(var_attributes)) {
    ns_att_vars <- names(var_attributes)
  }


  #--- setup of netCDF file
  var_chunksizes <- c(raster::ncol(x), raster::nrow(x))
  var_start <- c(1, 1)

  if (has_time_central) {
    var_chunksizes <- c(var_chunksizes, 1L)
    var_start <- c(var_start, 1)
  }

  # define dimensions
  bnddim <- ncdf4::ncdim_def(name = "bnds", units = "", vals = seq_len(2L),
    create_dimvar = FALSE)
  xdim <- ncdf4::ncdim_def(name = "lon", longname = "Longitude",
    units = "degrees_east", vals = xvals)
  ydim <- ncdf4::ncdim_def(name = "lat", longname = "Latitude",
    units = "degrees_north", vals = yvals)
  if (has_time_central) {
    tdim <- ncdf4::ncdim_def(name = "time", units = "Gregorian_year since 1900",
      vals = time_central)
  }

  # define variables
  var_dims <- list(xdim, ydim)
  if (has_time_central) {
    var_dims <- c(var_dims, list(tdim))
  }

  var_defs <- lapply(seq_len(nl), function(k)
    ncdf4::ncvar_def(name = var_names[k], units = var_units[k],
      dim = var_dims, chunksizes = var_chunksizes, missval = NAflag,
      longname = var_longnames[k], prec = ncdf4_datatype))

  crsdef <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(),
    missval = NULL, prec = "integer")

  # define dimension bounds
  lonbnddef <- ncdf4::ncvar_def(name = "lon_bnds", units = "",
    dim = list(bnddim, xdim), missval = NULL,
    chunksizes = c(2L, var_chunksizes[1]))
  latbnddef <- ncdf4::ncvar_def(name = "lat_bnds", units = "",
    dim = list(bnddim, ydim), missval = NULL,
    chunksizes = c(2L, var_chunksizes[2]))

  if (has_time_central) {
    tbnddef <- ncdf4::ncvar_def(name = "time_bnds", units = "",
      dim = list(bnddim, tdim), missval = NULL, chunksizes = c(2L, 1L))
  }

  nc_dimvars <- list(lonbnddef, latbnddef)
  if (has_time_central) {
    nc_dimvars <- c(nc_dimvars, list(tbnddef))
  }

  #--- create netCDF file
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  nc <- ncdf4::nc_create(filename = file,
    vars = c(nc_dimvars, list(crsdef), var_defs), force_v4 = force_v4)
  on.exit(ncdf4::nc_close(nc))


  #--- write values of dimension bounds:
  try(ncdf4::ncvar_put(nc, varid = "lon_bnds",
    vals = rbind(xvals - grid_halfres[1], xvals + grid_halfres[1]),
    start = c(1, 1), count = c(2L, var_chunksizes[1])))

  try(ncdf4::ncvar_put(nc, varid = "lat_bnds",
    vals = rbind(yvals + grid_halfres[2], yvals - grid_halfres[2]),
    start = c(1, 1), count = c(2L, var_chunksizes[2])))

  if (has_time_central) {
    try(ncdf4::ncvar_put(nc, varid = "time_bnds",
      vals = time_bounds,
      start = c(1, 1), count = c(2L, 1L)))
  }


  #--- add attributes
  # add dimension attributes
  ncdf4::ncatt_put(nc, "lon", "axis", "X")
  ncdf4::ncatt_put(nc, "lon", "bounds", "lon_bnds")
  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  ncdf4::ncatt_put(nc, "lat", "bounds", "lat_bnds")

  if (has_time_central) {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bnds")
    ncdf4::ncatt_put(nc, "time", "calendar", "gregorian")
  }

  # add global attributes
  ncdf4::ncatt_put(nc, varid = 0, attname = "Conventions", attval = "CF-1.4")
  ncdf4::ncatt_put(nc, varid = 0, attname = "created_by",
    attval = paste0(R.version[["version.string"]], ", R packages ",
      if (requireNamespace("rSFSW2")) {
        paste0("rSFSW2 v", utils::packageVersion("rSFSW2"), " and ")
      },
      "ncdf4 v", utils::packageVersion("ncdf4"),
      ", and ", system2("nc-config", "--version", stdout = TRUE, stderr = TRUE))
    )
  ncdf4::ncatt_put(nc, varid = 0, attname = "creation_date",
    attval = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  if (!missing(global_attributes)) {
    ns_att_glob <- names(global_attributes)

    for (natt in ns_att_glob) {
      ncdf4::ncatt_put(nc, varid = 0, attname = natt,
        attval = global_attributes[[natt]])
    }
  }

  if (!has_time_central) {
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_label",
      attval = "None")
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_title",
      attval = "No temporal dimensions ... fixed field")
  }

  # add coordinate system attributes
  prj <- raster::crs(x)
  if (!is.na(prj)) {
    ncdf4::ncatt_put(nc, varid = "crs", attname = "proj4",
      attval = as.character(prj))
  }


  #--- add variables values
  for (k in seq_len(nl)) {
    # write values to variable
    vals <- raster::getValues(if (nl > 1) raster::raster(x, layer = k) else x)

    try(ncdf4::ncvar_put(nc, varid = var_names[k],
      vals = matrix(vals, ncol = var_chunksizes[2]),
      start = var_start, count = var_chunksizes))

    # add variable attributes
    for (natt in ns_att_vars) {
      ncdf4::ncatt_put(nc, varid = var_names[k], attname = natt,
        attval = var_attributes[[natt]][k])
    }

    # Flush this step to the file so we dont lose it
    # if there is a crash or other problem
    ncdf4::nc_sync(nc)
  }

  invisible(TRUE)
}

#' @describeIn create_netCDF_from_raster_with_variables Convert array where
#'   variables are organized in the third dimension to a \var{netCDF} file
#' @export
#' 

# new argument - is it a grid? TRUE or FALSE....
# should be able to take 3 atypes grid, locs on grid, locs not on grid ..
# global attribute - is it point data?
# time bands as a separate argument ...
# 

create_empty_netCDF_file <- function(data, has_T_timeAxis = FALSE,
  has_Z_verticalAxis = FALSE, time_bounds, vert_bounds, var_attributes, 
  time_attributes, vertical_attributes, global_attributes, 
  isGridded = TRUE, grid = NULL, locations, crs, file, 
  force_v4 = TRUE, overwrite = FALSE) {

  # ---------------------------------------------------------------------
  # Set up and checks ---------------------------------------------------
  # ---------------------------------------------------------------------
  stopifnot(requireNamespace("ncdf4"))
  if (force_v4) {
    # avoid "_FillValue" error in older versions of `raster` package
    stopifnot(utils::packageVersion("raster") >= "2.9.1")
  }
  
  if(is.null(grid) && missing(locations)) {
    stop('Error: Neither a grid or locations data present. Must supply one to function.')
  }
  
  stopifnot(!missing(locations) && !missing(crs)) #If you are giving locations and not a grid, need to define the CRS.

  if (file.exists(file)) {
    if (overwrite) {
      unlink(file)
    } else {
      stop("File ", shQuote(basename(file)), " exists and 'overwrite' is FALSE")
    }
  }

  nl <- NCOL(data)
  if (nl == 1 && is.null(dim(data))) {
    data <- matrix(data, ncol = 1, dimnames = list(NULL, names(data)))
  }

  ncdf4_datatype <- raster:::.getNetCDFDType(get_raster_datatype(data))
  
  NAflag <- switch(ncdf4_datatype,
                   char = NULL, byte = NULL, short = -128L, integer = -2147483647L,
                   float = -3.4e+38, double = -1.7e+308)
  
  # location and spatial info  -------------------------------------------------------------
  if(!missing(locations)) {
    if (inherits(locations, "Spatial")) {
      loc <- locations #sp::coordinates(locations)
      if(isGridded) gridded(loc) = TRUE
    } else {
      loc <- SpatialPoints(locations)
      proj4string(loc) <- CRS(crs)
      if(isGridded) gridded(loc) = TRUE 
    }
  }
  
  # Note: xvals should be organized from west to east, yvals from north to south
  if(!is.null(grid)){
    xvals <- raster::xFromCol(grid, seq_len(raster::ncol(grid)))
    yvals <- raster::yFromRow(grid, seq_len(raster::nrow(grid)))
    grid_halfres <- raster::res(grid) / 2
  } else {
    xvals <- sort(unique(loc@coords[,1]))
    yvals <- sort(unique(loc@coords[,2]), decreasing = TRUE) 
    grid_halfres <- loc@grid@cellsize / 2
  }

  # Time dimension setup & info ----------------------------------------------------------------
  has_time_central <- !missing(time_bounds)
  
  if(has_T_timeAxis & has_time_central) {
    
    has_time_central <- FALSE
    t_chunksize <- nl
    
    stopifnot(length(time_attributes[["vals"]]) == nl)
  }
  
  if (has_time_central & !has_T_timeAxis) {
    stopifnot(length(time_bounds) == 2L)
    time_central <- mean(time_bounds)
    t_chunksize <- 1L
  }

  # Depth info  ---------------------------------------------------------------------------
  if(has_Z_verticalAxis) {
    
  } 
  
  # Variable info  ------------------------------------------------------------------------
  if (!missing(var_attributes)) {
    if ("name" %in% names(var_attributes)) {
      var_names <- var_attributes[["name"]]
      var_attributes[["name"]] <- NULL
    }

    if ("long_name" %in% names(var_attributes)) {
      var_longnames <- var_attributes[["long_name"]]
      var_attributes[["long_name"]] <- NULL
    }

    if (is.null(var_longnames) && !is.null(var_names)) {
      var_longnames <- var_names
    }

    if ("units" %in% names(var_attributes)) {
      var_units <- var_attributes[["units"]]
      var_attributes[["units"]] <- NULL
    }
  }

  if (has_T_timeAxis == FALSE  & has_Z_verticalAxis == FALSE) { # if both of these are false, netcdf will be organized by vars.
    var_names <- var_longnames <- names(data)
    var_units <- rep("", nl)
    
    if (is.null(var_longnames) && !is.null(var_names)) {
      var_longnames <- var_names
    }
  }
  
  if (!missing(var_attributes)) {
    ns_att_vars <- names(var_attributes)
  }

  # ---------------------------------------------------------------------------------------
  # -- Setup info for netCDF file ---------------------------------------------------------
  # ---------------------------------------------------------------------------------------
  
  if(is.null(grid)) {
    var_chunksizes <- c(length(xvals), length(yvals))
  } else {
    var_chunksizes <- c(raster::ncol(grid), raster::nrow(grid))
  }
  
  var_start <- c(1, 1)

  if (has_T_timeAxis) {
    var_chunksizes <- c(var_chunksizes, t_chunksize) 
    var_start <- c(var_start, 1)
  }
  
  if (has_Z_verticalAxis) {
    var_chunksizes <- c(var_chunksizes, z_chunksize) 
    var_start <- c(var_start, 1)
  }

  # define dimensions ---------------------------------------------------------------------
  
  #  bound dimension for lat and long
  bnddim <- ncdf4::ncdim_def(name = "bnds", units = "", vals = seq_len(2L),
    create_dimvar = FALSE)
  
  # lat and long dimension
  xdim <- ncdf4::ncdim_def(name = "lon", longname = "Longitude",
    units = "degrees_east", vals = xvals)
  ydim <- ncdf4::ncdim_def(name = "lat", longname = "Latitude",
    units = "degrees_north", vals = yvals)
  
  # time dimension 
  if(has_T_timeAxis) {
    if (has_time_central) {
      tdim <- ncdf4::ncdim_def(name = time_attributes[['name']], 
                               units = time_attributes[['units']],
                               calendar = time_attributes[['calendar']],
                               vals = time_central)
    } else {
      tdim <-  ncdf4::ncdim_def(name = time_attributes[['name']], 
                                units = time_attributes[['units']],
                                calendar = time_attributes[['calendar']],
                                vals = time_attributes[['vals']])
    }
  }
  
  # vertical dimension
  if(has_Z_verticalAxis) {
    vdim <-  ncdf4::ncdim_def(name = vertical_attributes[['name']], 
                              units = vertical_attributes[['units']],
                              positive = vertical_attributes[['positive']],
                              vals = vertical_attributes[['vals']])
  }

  # define dimensionality of netcdf variables -------------------------------------------------
  var_dims <- list(xdim, ydim)
  
  if (has_T_timeAxis) {
    var_dims <- c(var_dims, list(tdim))
  }
  
  if (has_Z_verticalAxis) {
    var_dims <- c(var_dims, list(zdim))
  }
  
  if(has_T_timeAxis || has_Z_verticalAxis) nn <- 1 else nn <- nl

  var_defs <- lapply(seq_len(nn), function(k)
    ncdf4::ncvar_def(name = var_names[k], units = var_units[k],
      dim = var_dims, chunksizes = var_chunksizes, missval = NAflag,
      longname = var_longnames[k], prec = ncdf4_datatype))
  
  # CRS defintion ------------------------------------------------------------------------
  crsdef <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(),
    missval = NULL, prec = "integer")

  # define dimension bands ---------------------------------------------------------------
  lonbnddef <- ncdf4::ncvar_def(name = "lon_bnds", units = "",
    dim = list(bnddim, xdim), missval = NULL,
    chunksizes = c(2L, var_chunksizes[1]))
  latbnddef <- ncdf4::ncvar_def(name = "lat_bnds", units = "",
    dim = list(bnddim, ydim), missval = NULL,
    chunksizes = c(2L, var_chunksizes[2]))
  
  if (has_T_timeAxis) {
    tbnddef <- ncdf4::ncvar_def(name = "time_bnds", units = "",
                                dim = list(bnddim, tdim), missval = NULL, 
                                chunksizes = c(2L, 1L))
  }
  if (has_Z_verticalAxis) {
    vertbnddef <- ncdf4::ncvar_def(name = "vert_bnds", units = "",
                                   dim = list(bnddim, vdim), missval = NULL, 
                                   chunksizes = c(2L, 1L))
  }

  nc_dimvars <- list(lonbnddef, latbnddef)
  
  if (has_T_timeAxis) {
    nc_dimvars <- c(nc_dimvars, list(tbnddef))
  }
  if (has_Z_verticalAxis) {
    nc_dimvars <- c(nc_dimvars, list(vertbnddef))
  }

  # ---------------------------------------------------------------------------------------
  #--- create empty netCDF file -----------------------------------------------------------
  # ---------------------------------------------------------------------------------------
  
  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)
  
  nc <- ncdf4::nc_create(filename = file,
    vars = c(nc_dimvars, list(crsdef), var_defs), force_v4 = force_v4)
  on.exit(ncdf4::nc_close(nc))

 #--- write values of dimension bands -----------------------------------------------------
  try(ncdf4::ncvar_put(nc, varid = "lon_bnds",
    vals = rbind(xvals - grid_halfres[1], xvals + grid_halfres[1]),
    start = c(1, 1), count = c(2L, var_chunksizes[1])))

  try(ncdf4::ncvar_put(nc, varid = "lat_bnds",
    vals = rbind(yvals + grid_halfres[2], yvals - grid_halfres[2]),
    start = c(1, 1), count = c(2L, var_chunksizes[2])))

  if (has_T_timeAxis) {
    if(has_time_central){
      try(ncdf4::ncvar_put(nc, varid = "time_bnds",
                           vals = time_bounds,
                           start = c(1, 1), count = c(2L, 1L)))
    } else {
      try(ncdf4::ncvar_put(nc, varid = "time_bnds",
                           vals = time_bounds, 
                           start = c(1, 1), count = c(2L, var_chunksizes[3])))
    }
  }

  if(has_Z_verticalAxis) {
    try(ncdf4::ncvar_put(nc, varid = "depth_bnds",
                         vals = vertical_attributes[['vals']],
                         start = c(1, 1), count = c(2L, var_chunksizes[4]))) # top and bottom of each soil layer
  }

  #--- add attributes -----------------------------------------------------------------
  
  # add dimension attributes
  ncdf4::ncatt_put(nc, "lon", "axis", "X")
  ncdf4::ncatt_put(nc, "lon", "bounds", "lon_bnds")
  ncdf4::ncatt_put(nc, "lat", "axis", "Y")
  ncdf4::ncatt_put(nc, "lat", "bounds", "lat_bnds")

  if (has_T_timeAxis) {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bnds")
  }

  # add global attributes
  ncdf4::ncatt_put(nc, varid = 0, attname = "Conventions", attval = "CF-1.4")
  ncdf4::ncatt_put(nc, varid = 0, attname = "created_by",
    attval = paste0(R.version[["version.string"]], ", R packages ",
      if (requireNamespace("rSFSW2")) {
        paste0("rSFSW2 v", utils::packageVersion("rSFSW2"), " and ")
      },
      "ncdf4 v", utils::packageVersion("ncdf4"),
      ", and ", system2("nc-config", "--version", stdout = TRUE, stderr = TRUE))
  )
  ncdf4::ncatt_put(nc, varid = 0, attname = "creation_date",
    attval = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  if (!missing(global_attributes)) {
    ns_att_glob <- names(global_attributes)

    for (natt in ns_att_glob) {
      ncdf4::ncatt_put(nc, varid = 0, attname = natt,
        attval = global_attributes[[natt]])
    }
  }

  if (!has_T_timeAxis) {
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_label",
      attval = "None")
    ncdf4::ncatt_put(nc, varid = 0, attname = "time_title",
      attval = "No temporal dimensions ... fixed field")
  }

  # add coordinate system attributes ---------------------------------------------------
  if(is.null(grid)){
    prj <- raster::crs(grid)
  } else {
    prj <- CRS(crs)
  }
  
  if (!is.na(prj)) {
    ncdf4::ncatt_put(nc, varid = "crs", attname = "proj4",
      attval = as.character(prj))
  }
  
  invisible(TRUE)
}


populate_netcdf_from_array <- function(file, grid, var_attributes) {

  #--- prepare to put values to full grid format
  val_template <- rep(NA, raster::ncell(grid))
  val_ids <- raster::cellFromXY(grid, locations)

  #--- add variables values
  for (k in seq_len(nl)) {
    # put values into full grid format
    temp <- val_template
    temp[val_ids] <- x[, k]
    vals <- matrix(temp, ncol = var_chunksizes[2])

    # write values to variable
    try(ncdf4::ncvar_put(nc, varid = var_names[k], vals = vals,
      start = var_start, count = var_chunksizes))

    # add variable attributes
    for (natt in ns_att_vars) {
      ncdf4::ncatt_put(nc, varid = var_names[k], attname = natt,
        attval = var_attributes[[natt]][k])
    }

    # Flush this step to the file so we dont lose it
    # if there is a crash or other problem
    ncdf4::nc_sync(nc)
  }

  invisible(TRUE)
}




#' Enhance \code{\link[raster]{raster}} to handle added information written
#' to a \var{netCDF} file by function
#' \code{\link{create_netCDF_from_raster_with_variables}}
#'
#' @return A Raster* object.
#' @export
read_netCDF_to_raster <- function(x, ...) {
  r <- raster::raster(x, ...)

  # Check whether projection was read correctly
  r_crs <- raster::crs(r)
  r_has_crs <- inherits(r_crs, "CRS") && !is.na(r_crs) &&
    rgdal::checkCRSArgs(raster::crs(r, asText = TRUE))[[1]]

  if (!r_has_crs) {
    nc <- RNetCDF::open.nc(x)
    nc_crs <- RNetCDF::att.get.nc(nc, variable = "crs", attribute = "proj4")
    RNetCDF::close.nc(nc)

    nc_crs <- raster::crs(nc_crs)
    if (inherits(nc_crs, "CRS") && !is.na(nc_crs) &&
        rgdal::checkCRSArgs(raster::crs(nc_crs, asText = TRUE))[[1]]) {
      raster::crs(r) <- nc_crs
    } else {
      warning("'read_netCDF_to_raster': could not locate a valid projection.")
    }
  }

  r
}



#' Calculate "nominal resolution" of grid
#' @references CMIP6 Global Attributes, DRS, Filenames, Directory Structure, and CV’s
#'   10 September 2018 (v6.2.7)
#'   Appendix 2: Algorithms for Defining the "nominal_resolution" Attribute
#'   https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit#bookmark=id.ibeh7ad2gpdi
#' @export
calculate_nominal_resolution <- function(grid, sites, cell_areas_km2) {
  stopifnot(requireNamespace("geosphere"))
  # For a land surface model calculated on its own grid, include all land grid cells

  # For each grid cell, calculate the distance (in km) between each pair of
  # cell vertices and select the maximum distance ("dmax").
  # For latxlon grid cells, for example, dmax would be the diagonal distance.
  res <- raster::res(grid)

  if (raster::isLonLat(grid)) {
    xy <- sp::coordinates(sites)
    xy_lowerleft <- xy - res / 2
    xy_upperright <- xy + res / 2

    id_use <-
      xy_lowerleft[, 1] >= -180 & xy_lowerleft[, 1] <= 180 &
      xy_lowerleft[, 2] >= -90 & xy_lowerleft[, 2] <= 90 &
      xy_upperright[, 1] >= -180 & xy_upperright[, 1] <= 180 &
      xy_upperright[, 2] >= -90 & xy_upperright[, 2] <= 90

    dmax_km <- 1e-3 *
      geosphere::distGeo(xy_lowerleft[id_use, ], xy_upperright[id_use, ])

    # Calculate the mean over all cells of dmax, weighting each by the grid-cell's
    # area (A)
    mean_resolution_km <- stats::weighted.mean(dmax_km, cell_areas_km2[id_use])

  } else {

    mean_resolution_km <- dmax_km <- 1e-3 * sqrt(sum(res ^ 2))
  }



  # Nominal resolution
  ifelse(mean_resolution_km < 0.72, "0.5 km",
    ifelse(mean_resolution_km < 1.6, "1 km",
      ifelse(mean_resolution_km < 3.6, "2.5 km",
        ifelse(mean_resolution_km < 7.2, "5 km",
          ifelse(mean_resolution_km < 16, "10 km",
            ifelse(mean_resolution_km < 36, "25 km",
              ifelse(mean_resolution_km < 72, "50 km",
                ifelse(mean_resolution_km < 160, "100 km",
                  ifelse(mean_resolution_km < 360, "250 km",
                    ifelse(mean_resolution_km < 720, "500 km",
                      ifelse(mean_resolution_km < 1600, "1000 km",
                        ifelse(mean_resolution_km < 3600, "2500 km",
                          ifelse(mean_resolution_km < 7200, "5000 km",
                            "10000 km")))))))))))))
}
