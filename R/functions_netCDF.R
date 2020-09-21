#'  Create a structured, empty netcdf file
#'
#'  This function creates a strucutured netCDF follwing CF 1.8 standards with metadata
#'  from an array, but without data.
#'
#'  The user provides an array and specifies spatial information, dimensionality,
#'  and metadata in order to create an empty netcdf.
#'
#'  The netCDF always includes at least two dimensions: Latitude and Longitude.
#'  Information about these dimensions are taken from the \var{locations},
#'  \var{isGridded}, \var{crs_attributes}, and \var{grid} arguments. One of \var{locations}
#'  or \var{grid} needs to be provided, but not both.
#'  
#'  Additional dimensions are added by setting \var{has_T_timeAxis} and/or
#'  \var{has_Z_verticalAxis} to \code{TRUE}. If both flags are set to
#'  \code{FALSE} then the netCDF is two dimensions with one or more variables.
#'  The number of variables is set in the \var{var_attributes} argument. If either
#'  \var{has_T_timeAxis} \emph{or} \var{has_Z_verticalAxis} is set to \code{TRUE} then a
#'  third dimension is added to the netCDF. If \emph{both} \var{has_T_timeAxis}
#'  and \var{has_Z_verticalAxis} are set to TRUE, then the netCDF will have four
#'  dimensions, with time as the third, and depth as the fourth. Information about
#'  these dimensions is set in the respective \var{bounds} and \var{attributes} arguments.
#'  \var{has_T_timeAxis} and/or \var{has_Z_verticalAxis} is set to \code{TRUE}
#'  the netCDF can \emph{only} have one variable.
#'  
#'  The \var{data} object is used to set up the size and dataType in the netCDF. The metadata
#'  needs to match the data. This object contains the data that you intend to
#'  populate the netCDF wtih. The array should be set up so that each row is a site's info,
#'  and each column is a value for variables, time or depth. If the netcdf is 4d
#'  (time and depth == \code{TRUE}, then need to have a 3-d data array,
#'  where each additional dimension contains values for each depth.
#'
#'
#' @param data A numeric array.
#' @param has_T_timeAxis A logical value. Indicates that the netCDF created will
#'     have a third, time dimension.
#' @param has_Z_verticalAxis A logical value. Indicates that the netCDF created will
#'     have a veritical (e.g. soil profile depths) dimension. If \var{has_T_timeAxis}
#'     is set to \code{FALSE} then the Z axis will be the third dimension.
#'     If \var{has_T_timeAxis} is set to \code{TRUE} then the Z axis will be the
#'     fourth dimension.
#' @param time_bounds A numeric vector that continuously lists the lower and upper
#'    bounds of each time dimension value. In the absence of a time dimension,
#'    this argument can be used to define the calculation period of the variable(s).
#' @param vert_bounds A numeric vector that continuously lists the lower and upper
#'    bounds of each vertical dimension value.
#' @param var_attributes A list of named character strings defining the variables
#'   of the netCDF.
#' @param time_attributes A list of named character strings defining the time
#'   dimension of the netCDF.
#' @param vertical_attributes A list of named character strings defining the vertical
#'   dimension of the netCDF.
#' @param global_attributes A list of named character strings defining the
#'   global attributes of the netCDF.
#' @param crs_attributes A list of named character string defining the CRS
#'   of the netCDF.
#' @param isGridded A logical value. Represents whether the location data is on
#'   a regular grid or not.
#' @param grid filename (character). File containing the grid information
#'   (i.e. resolution, extent, locations) of the data. Supported file types are
#'   the 'native' raster package format and those that can be read via rgdal.
#' @param locations A SpatialPoints object or a matrix or data.frame with two
#'   columns containing long and lat values. Data must be organized by (1) long,
#'   (2) lat.
#' @param file A character string. The file path of the netCDF file to be
#'   created.
#' @param force_v4 A logical value. Force version 4 of netCDF.
#' @param overwrite A logical value. If \code{TRUE}, file will be overwritten
#'   if it already exists.
#'
#'
#' @return This function is used for the side-effect of creating a file.
#'
#' @examples
#' #############################################################################
#' # example - create an empty netcdf with a three dimensions (lat, long, time)
#' #############################################################################
#'
#' # create dummy data ---------------------------------------------------------
#'
#' someData <- rnorm(100, 7, 30)
#' data <- array(someData, c(10, 10))
#'
#' locations <- data.frame(X_WGS84 = c(rep(-124.5938, 5), rep(-124.5312, 5)),
#'                         Y_WGS84 = rep(c(47.90625, 47.96875, 48.03125, 48.09375,
#'                                    48.15625), 2))
#'
#' annual_time_bounds <- c(43737, 44102, 44102, 44467, 44467, 44832, 44832, 45197,
#'                         45197, 45563, 45563, 45928, 45928, 46293, 46293, 46658,
#'                         46658, 47024, 47024, 47389) # beginning and end of year
#'
#' outFileName <- 'dummynetcdf.nc'
#'
#' # define attributes ---------------------------------------------------------
#'
#' # time attribute
#' time_attributes <- list(
#'    name = 'time',
#'    units = 'days since 1900-01-01',
#'    calendar = 'standard',
#'    unlim = 'TRUE',
#'    vals = c(43554, 43920, 44285, 44650, 45015, 45381, 45746, 46111, 46476, 46842)# mid point of year
#'    )
#'
#' # variable attributes
#' var_attributes <- list(
#'    name = 'JulyTemp',
#'    long_name = 'Annual Mean July Temperature',
#'    units = 'Celsius',
#'    description = 'example data!'
#'    )
#'
#' # CRS attributes
#' crs_attributes <- list(
#'   proj = '+init=epsg:4326',
#    grid_mapping_name = "latitude_longitude",
#'   longitude_of_prime_meridian = 0.0,
#'   semi_major_axis = 6378137.0,
#'   inverse_flattening = 298.257223563,
#'   crs_wkt = 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],
#'                          AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0, AUTHORITY["EPSG","8901"]],
#'                          UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'
#' )
#'
#' # global attributes
#' global_attributes <- list(
#'    title = "",
#'    institution = 'Southwest Biological Science Center, U.S. Geological Survey',
#'    description = 'how this data was made',
#'    source = paste(
#'      "SOILWAT2 (v4.2.0);",
#'      "rSOILWAT2 (v2.3.2);",
#'      "rSFSW2 (v3.1.2)"
#'    ),
#'    source_id = "SOILWAT2",
#'    realm = "land",
#'    parent_mip_era = "CMIP5",
#'    parent_experiment_id = "RCP45",
#'    parent_source = "CanESM2",
#'    parent_variant_label = "r1i1p1f1",
#'    product = "model-output",
#'    projection = 'Geographic',
#'    grid = 'WGS84',
#'    grid_label = "gn",
#'    nominal_resolution = "10 km", # \code{calculate_nominal_resolution}
#'    further_info_url = "https://github.com/DrylandEcology/",
#'    contact = "you@email.com"
#'    )
#'
#' # run function -------------------------------------------------------------
#' create_empty_netCDF_file(
#'      data = data,
#'      has_T_timeAxis = TRUE,
#'      has_Z_verticalAxis = FALSE,
#'      time_bounds = annual_time_bounds,
#'      vert_bounds = NULL,
#'      var_attributes = var_attributes,
#'      time_attributes  = time_attributes,
#'      vertical_attributes = NULL,
#'      global_attributes = global_attributes,
#'      crs_attributes = crs_attributes,
#'      isGridded = TRUE,
#'      locations = locations,
#'      file = outFileName,
#'      force_v4 = TRUE,
#'      overwrite = TRUE
#'    )
#'
#' @seealso \code{populate_netcdf_from_array}
#' @seealso \url{http://cfconventions.org/cf-conventions/cf-conventions.html} # for defining attributes
#' 
#' @export

create_empty_netCDF_file <- function(data, has_T_timeAxis = FALSE,
  has_Z_verticalAxis = FALSE, time_bounds, vert_bounds, var_attributes,
  time_attributes, vertical_attributes, global_attributes,
  crs_attributes, isGridded = TRUE, grid = NULL, locations,
  file, force_v4 = TRUE, overwrite = FALSE) {

  # ---------------------------------------------------------------------
  # Checks --------------------------------------------------------------
  # ---------------------------------------------------------------------
  stopifnot(requireNamespace("ncdf4"))
  if (force_v4) {
    # avoid "_FillValue" error in older versions of `raster` package
    stopifnot(utils::packageVersion("raster") >= "2.9.1")
  }

  if(is.null(grid) && missing(locations)) {
    stop('Error: Neither a grid or locations data present. Must supply one at least
         one of these arguments to the function.')
  }

  # find CRS definition
  crs <- crs_attributes[['proj']]
  if(is.null(crs) & isS4(locations)) crs <-  locations@proj4string
  
  if(!missing(locations) && is.null(crs)){
    stop('Error: If you are giving locations and not a grid, need to define the CRS
         in the crs_attribute[["proj"]] argument')
  } 

  if (file.exists(file)) {
    if (overwrite) {
      unlink(file)
    } else {
      stop("File ", shQuote(basename(file)), " exists and 'overwrite' is FALSE")
    }
  }

  nl <- NCOL(data)

  if(has_T_timeAxis == TRUE) {
    tn <- length(time_attributes$vals)

    if(is.null(time_bounds)) {
      stop('Need to define time bounds data for time dimension')
    }

    if(tn * 2 != length(time_bounds) ){
      stop('Need to define bounds (min and max) for each time value in time attributes')
    }

    if(tn > 1 && tn !=nl) {
      stop('number of values in time dimension should either be of length 1
      (if all variable in the dataset represent the same measurement time) or equal
      to the number of columns in the dataset (dataset is a time series of one variable)')
    }
  }

  if(has_Z_verticalAxis == TRUE) {
    zn <- length(vertical_attributes$vals)

    if(is.null(vert_bounds)){
      stop('Need to define depth bounds data for depth dimension')
      }

    if(zn * 2 != length(vert_bounds)) {
      stop('Number of depth layers need to be equal to defined values')
    }
  }

  # ---------------------------------------------------------------------
  # Setup  --------------------------------------------------------------
  # ---------------------------------------------------------------------
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
      if(isGridded) sp::gridded(loc) = TRUE
    } else {
      loc <- sp::SpatialPoints(locations)
      sp::proj4string(loc) <- sp::CRS(crs)
      if(isGridded) sp::gridded(loc) = TRUE
    }
  }

  if(!isGridded) nloc <- dim(loc@coords)[1]

  # Note: xvals should be organized from west to east, yvals from north to south
  if(!is.null(grid)) {
    xvals <- raster::xFromCol(grid, seq_len(raster::ncol(grid)))
    yvals <- raster::yFromRow(grid, seq_len(raster::nrow(grid)))
    grid_halfres <- raster::res(grid) / 2
  } else {
    xvals <- sort(unique(loc@coords[,1]))
    yvals <- sort(unique(loc@coords[,2]), decreasing = TRUE)
    if(isGridded) grid_halfres <- loc@grid@cellsize / 2
  }

  # crs attributes setup & info ----------------------------------------------------------------
  if (!missing(crs_attributes)) {

    if ("crs_wkt" %in% names(crs_attributes)) {
      crs_wkt <- crs_attributes[["crs_wkt"]]
      crs_attributes[["crs_wkt"]] <- NULL
    } 
    
    ns_att_crs <- names(crs_attributes)

  }

  # Time dimension setup & info ----------------------------------------------------------------
  if(has_T_timeAxis & !is.null(time_bounds)) {
    t_chunksize <- length(time_attributes[["vals"]])
  }

  if(has_T_timeAxis) {
    if (!missing(time_attributes) || !is.null(time_attributes)) {
      if ("name" %in% names(time_attributes)) {
        time_names <- time_attributes[["name"]]
        time_attributes[["name"]] <- NULL
      } else {
        stop('Need name attribute in time attribute list')
      }
      
      if ("units" %in% names(time_attributes)) {
        time_units <- time_attributes[["units"]]
        time_attributes[["units"]] <- NULL
      } else {
        stop('Need units attribute in time attribute list')
      }
      
      if ("calendar" %in% names(time_attributes)) {
        time_cal <- time_attributes[["calendar"]]
        time_attributes[["calendar"]] <- NULL
      } else {
        stop('Need calendar attribute in time attribute list')
      }
      
      if ("vals" %in% names(time_attributes)) {
        time_vals <- time_attributes[["vals"]]
        time_attributes[["vals"]] <- NULL
      } else {
        stop('Need vals attribute in time attribute list')
      }
      
      ns_att_time <- names(time_attributes)
    }
  } else {
    time_vals <- 0
  }

  # Depth info  ---------------------------------------------------------------------------
  if(has_Z_verticalAxis && !is.null(vert_bounds)) {
    if(has_T_timeAxis) { # if time and depth are both TRUE , the third dimension
      z_chunksize <- dim(data)[3]
      stopifnot(z_chunksize ==  zn)
    } else { # no time, then we can assume that the depth dimension is the 2d
      z_chunksize <- nl
      stopifnot(z_chunksize ==  zn)
    }
  }

  if(has_Z_verticalAxis) {
    if (!missing(vertical_attributes) || !is.null(vertical_attributes) ) {
      if ("name" %in% names(vertical_attributes)) {
        vert_names <- vertical_attributes[["name"]]
        vertical_attributes[["name"]] <- NULL
      } else {
        stop('Need name attribute in vertical attribute list')
      }
      
      if ("units" %in% names(vertical_attributes)) {
        vert_units <- vertical_attributes[["units"]]
        vertical_attributes[["units"]] <- NULL
      } else {
        stop('Need units attribute in vertical attribute list')
      }
      
      if ("vals" %in% names(vertical_attributes)) {
        vert_vals <- vertical_attributes[["vals"]]
        vertical_attributes[["vals"]] <- NULL
      } else {
        stop('Need vals attribute in vertical attribute list')
      }
      
      ns_att_vert <- names(vertical_attributes)
      
    }
  } else {
    vert_vals <- 0
  }
  
  # Variable info  ------------------------------------------------------------------------
  if (!missing(var_attributes)) {
    if ("name" %in% names(var_attributes)) {
      var_names <- var_attributes[["name"]]
      var_attributes[["name"]] <- NULL
    } else {
      stop('Need name attribute in variable attribute list')
    }

    if ("long_name" %in% names(var_attributes)) {
      var_longnames <- var_attributes[["long_name"]]
      var_attributes[["long_name"]] <- NULL
    } else {
      var_longnames <- NULL
    }

    if (is.null(var_longnames) && !is.null(var_names)) {
      var_longnames <- var_names
    }

    if ("units" %in% names(var_attributes)) {
      var_units <- var_attributes[["units"]]
      var_attributes[["units"]] <- NULL
    } else {
      stop('Need unit attribute in variable attribute list')
    }

    ns_att_vars <- names(var_attributes)
  }

  # ---------------------------------------------------------------------------------------
  # -- Setup info for netCDF file ---------------------------------------------------------
  # ---------------------------------------------------------------------------------------

  # Starts and chunksizes -----------------------------------------------------------------
  if(isGridded) {
    if(is.null(grid)) {
      var_chunksizes <- c(length(xvals), length(yvals))
      } else {
        var_chunksizes <- c(raster::ncol(grid), raster::nrow(grid))
    }
    var_start <- c(1, 1)
    } else { # locations aren't gridded
    var_chunksizes <- c(nloc)
    var_start <- c(1)
  }

  if (has_T_timeAxis) {
    var_chunksizes <- c(var_chunksizes, t_chunksize)
    var_start <- c(var_start, 1)
  }

  if (has_Z_verticalAxis) {
    var_chunksizes <- c(var_chunksizes, z_chunksize)
    var_start <- c(var_start, 1)
  }

  # define dimensions ---------------------------------------------------------------------

  #  bounds dimension
  bnddim <- ncdf4::ncdim_def(name = "bnds", units = "", vals = seq_len(2L),
                                           create_dimvar = FALSE)

  # lat and long dimension
  if(isGridded){
    xdim <- ncdf4::ncdim_def(name = "lon", longname = "Longitude",
                             units = "degrees_east", vals = xvals)
    ydim <- ncdf4::ncdim_def(name = "lat", longname = "Latitude",
                             units = "degrees_north", vals = yvals)
  } else {
    idim <- ncdf4::ncdim_def(name = 'site', longname = 'SOILWAT2 simulation sites',
                             units = 'site_id', vals = 1:nrow(data))
  }

  # time dimension
  if(has_T_timeAxis) {
      tdim <-  ncdf4::ncdim_def(name = time_names,
                                units = time_units,
                                calendar = time_cal,
                                vals = time_vals)
  }

  # vertical dimension
  if(has_Z_verticalAxis) {
    zdim <-  ncdf4::ncdim_def(name = vert_names,
                              units = vert_units,
                              vals = vert_vals)
  }

  # define dimensionality of netcdf variables -------------------------------------------------
  var_dims <- if(isGridded) list(xdim, ydim) else list(idim)

  if (has_T_timeAxis) {
    var_dims <- c(var_dims, list(tdim))
  }

  if (has_Z_verticalAxis) {
    var_dims <- c(var_dims, list(zdim))
  }

  if(length(time_vals) > 1 || length(vert_vals) > 1) nn <- 1 else nn <- nl

  var_defs <- lapply(seq_len(nn), function(k)
    ncdf4::ncvar_def(name = var_names[k], units = var_units[k],
      dim = var_dims, chunksizes = var_chunksizes, missval = NAflag,
      longname = var_longnames[k], prec = ncdf4_datatype))

  # add lat and long as variables if not gridded
  if(!isGridded){
   latvar <-  ncdf4::ncvar_def(name = 'lat', units = 'degrees_north',
                               dim = list(idim), chunksizes = var_chunksizes[1], missval = NAflag,
                               longname = 'site latitiude', prec = ncdf4_datatype)

   longvar <- ncdf4::ncvar_def(name = 'lon', units = 'degrees_east',
                               dim = list(idim), chunksizes = var_chunksizes[1], missval = NAflag,
                               longname = 'site longitude', prec = ncdf4_datatype)

   var_defs <- c(var_defs, list(latvar, longvar))
  }

  # CRS defintion ------------------------------------------------------------------------
  crsdef <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(),
    missval = NULL, prec = "integer")

  # define dimension bands ---------------------------------------------------------------
  if(isGridded) {
    lonbnddef <- ncdf4::ncvar_def(name = "lon_bnds", units = "",
                                  dim = list(bnddim, xdim), missval = NULL,
                                  chunksizes = c(2L, var_chunksizes[1]))

    latbnddef <- ncdf4::ncvar_def(name = "lat_bnds", units = "",
                                  dim = list(bnddim, ydim), missval = NULL,
                                  chunksizes = c(2L, var_chunksizes[2]))
  }

  if (has_T_timeAxis) {
      tbnddef <- ncdf4::ncvar_def(name = "time_bnds", units = "",
                                dim = list(bnddim, tdim), missval = NULL,
                                chunksizes = c(2L, 1L))
  }

  if (has_Z_verticalAxis) {
      vertbnddef <- ncdf4::ncvar_def(name = "depth_bnds", units = "",
                                    dim = list(bnddim, zdim), missval = NULL,
                                    chunksizes = c(2L, 1L))
  }

  nc_dimvars <- if(isGridded) list(lonbnddef, latbnddef) else list()

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

 #--- write values of dimension bounds -----------------------------------------------------
  if(isGridded) {
    try(ncdf4::ncvar_put(nc, varid = "lon_bnds",
                        vals = rbind(xvals - grid_halfres[1], xvals + grid_halfres[1]),
                        start = c(1, 1), count = c(2L, var_chunksizes[1])))

    try(ncdf4::ncvar_put(nc, varid = "lat_bnds",
                         vals = rbind(yvals + grid_halfres[2], yvals - grid_halfres[2]),
                         start = c(1, 1), count = c(2L, var_chunksizes[2])))
  } else {
    try(ncdf4::ncvar_put(nc, varid = "lon",
                         vals = locations$X_WGS84,
                         start = c(1), count = c(var_chunksizes[1])))

    try(ncdf4::ncvar_put(nc, varid = "lat",
                         vals = locations$Y_WGS84,
                         start = c(1), count = c( var_chunksizes[1])))
  }

  if (has_T_timeAxis) {
        try(ncdf4::ncvar_put(nc, varid = "time_bnds",
                             vals = time_bounds,
                             start = c(1,1), count = c(2, t_chunksize))) # beginning and end of each TP
    }

  if(has_Z_verticalAxis) {
      try(ncdf4::ncvar_put(nc, varid = "depth_bnds",
                           vals = vert_bounds,
                           start = c(1, 1), count = c(2L, z_chunksize))) # top and bottom of each soil layer
  }

  #--- add attributes -----------------------------------------------------------------

  # add dimension attributes --------------------------------
 if (isGridded) {
   ncdf4::ncatt_put(nc, "lon", "axis", "X")
   ncdf4::ncatt_put(nc, "lon", "bounds", "lon_bnds")
   ncdf4::ncatt_put(nc, "lat", "axis", "Y")
   ncdf4::ncatt_put(nc, "lat", "bounds", "lat_bnds")
   }

  if (has_T_timeAxis) {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bnds")

    for (natt in ns_att_time) {
      ncdf4::ncatt_put(nc, varid = 'time', attname = natt,
                       attval = time_attributes[[natt]])
    }
  }

  if (has_Z_verticalAxis) {
    ncdf4::ncatt_put(nc, "depth", "axis", "Z")
    ncdf4::ncatt_put(nc, "depth", "bounds", "depth_bnds")

    for (natt in ns_att_vert) {
      ncdf4::ncatt_put(nc, varid = 'depth', attname = natt,
                       attval = vertical_attributes[[natt]])
    }
  }

  # add variable attributes  ---------------------------------------------------
  for (k in seq_len(nn)) {
    for (natt in ns_att_vars) {
      ncdf4::ncatt_put(nc, varid = var_names[k], attname = natt,
                       attval = var_attributes[[natt]][k])
    }
  }

  # add coordinate system attributes --------------------------------------------
  if(!is.null(grid)){
    prj <- raster::crs(grid)
  } else {
    prj <- sp::CRS(crs)
  }

  if (!is.na(prj)) {
    ncdf4::ncatt_put(nc, "crs", attname = "crs_wkt", crs_wkt)

     for (natt in ns_att_crs) {
       ncdf4::ncatt_put(nc, varid = 'crs', attname = natt,
                        attval = crs_attributes[[natt]])
    }
  }

  # add global attributes --------------------------------------------------------s
  ncdf4::ncatt_put(nc, varid = 0, attname = "Conventions", attval = "CF-1.8")

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

  # The end --------------------------------------------------------------

  message(paste("The file has", nc$nvars, "variables and",  nc$ndim, "dimensions"))

  invisible(TRUE)
}

#' Populate an empty netcdf with values stored in an array
#'
#' This function add values to a structured netCDF created with the
#' \code{create_empty_netCDF_file} function.
#' The data array provided as an argument to this function should be the same as
#' that provided to the create_empty_netCDF_file function.
#' One of \var{locations} or \var{grid} needs to be provided, but not both.
#'
#' @param file A character string. File path to a pre-created, empty netCDF.
#' @param data A numeric array.
#' @param var_name A character string equal to the length of the number of variables.
#'   The length of var_name should either be 1 if there is either a time and/or
#'   depth dimension \emph{or} equal to the number of columns in the array if
#'   there is no time or depth dimension.
#' @param has_T_timeAxis A logical value. Indicates that the netCDF file
#'     has a third, time dimension.
#' @param has_Z_verticalAxis A logical value. Indicates that the netCDF file
#'     has a veritical (e.g. soil profile depths) dimension. If \var{has_T_timeAxis}
#'     is set to \code{FALSE} then the Z axis will be the third dimension.
#'     If \var{has_T_timeAxis} is set to \code{TRUE} then the Z axis will be the
#'     fourth dimension.
#' @param isGridded A logical value. Represents whether the location data is on
#'   a regular grid or not.
#' @param grid filename (character). File containing the grid information
#'   (i.e. resolution, extent) of the data. Supported file types are the 'native'
#'   raster package format and those that can be read via rgdal.
#' @param locations A SpatialPoints object or a matrix or data.frame with two
#'   columns containing long and lat values. Data must be organized by long, lat.
#' @param force_v4 A logical value. Force version 4 of netCDF.
#'
#' @return This function is used for the side-effect of filling a file.
#'
#' @examples
#' #############################################################################
#' # example - create an empty netcdf with a three dimensions (lat, long, time)
#' #############################################################################
#'
#' # create dummy data ---------------------------------------------------------
#'
#' someData <- rnorm(100, 7, 30)
#' data <- array(someData, c(10, 10))
#'
#' locations <- data.frame(X_WGS84 = c(rep(-124.5938, 5), rep(-124.5312, 5)),
#'                         Y_WGS84 = rep(c(47.90625, 47.96875, 48.03125, 48.09375,
#'                                    48.15625), 2))
#'
#' annual_time_bounds <- c(43737, 44102, 44102, 44467, 44467, 44832, 44832, 45197,
#'                         45197, 45563, 45563, 45928, 45928, 46293, 46293, 46658,
#'                         46658, 47024, 47024, 47389) # beginning and end of year
#'
#' outFileName <- 'dummynetcdf.nc'
#'
#' # define attributes ---------------------------------------------------------
#'
#' # time attribute
#' time_attributes <- list(
#'    name = 'time',
#'    units = 'days since 1900-01-01',
#'    calendar = 'standard',
#'    unlim = 'TRUE',
#'    vals = c(43554, 43920, 44285, 44650, 45015, 45381, 45746, 46111, 46476, 46842)# mid point of year
#'    )
#'
#' # variable attributes
#' var_attributes <- list(
#'    name = 'JulyTemp',
#'    long_name = 'Annual Mean July Temperature',
#'    units = 'Celsius',
#'    description = 'example data!'
#'    )
#'
#' # CRS attributes
#' crs_attributes <- list(
#'  proj = '+init=epsg:4326',
#'  grid_mapping_name = 'latitude_longitude',
#'  longitude_of_prime_meridian = 0.0,
#'  semi_major_axis = 6378137.0,
#'  inverse_flattening = 298.257223563,
#'  crs_wkt = 'GEOGCS["WGS 84",DATUM["WGS_1984",SPHEROID["WGS 84",6378137,298.257223563,AUTHORITY["EPSG","7030"]],
#'                         AUTHORITY["EPSG","6326"]],PRIMEM["Greenwich",0, AUTHORITY["EPSG","8901"]],
#'                         UNIT["degree",0.01745329251994328,AUTHORITY["EPSG","9122"]],AUTHORITY["EPSG","4326"]]'
#' )
#'
#' # global attributes
#' global_attributes <- list(
#'    title = "",
#'    institution = 'Southwest Biological Science Center, U.S. Geological Survey',
#'    description = 'how this data was made',
#'    source = paste(
#'      "SOILWAT2 (v4.2.0);",
#'      "rSOILWAT2 (v2.3.2);",
#'      "rSFSW2 (v3.1.2)"
#'    ),
#'    source_id = "SOILWAT2",
#'    realm = "land",
#'    parent_mip_era = "CMIP5",
#'    parent_experiment_id = "RCP45",
#'    parent_source = "CanESM2",
#'    parent_variant_label = "r1i1p1f1",
#'    product = "model-output",
#'    projection = 'Geographic',
#'    grid = 'WGS84',
#'    grid_label = "gn",
#'    nominal_resolution = "10 km", # calculate_nominal_resolution
#'    further_info_url = "https://github.com/DrylandEcology/",
#'    contact = "you@email.com"
#'    )
#'
#' # run create_empty_netCDF_file function --------------------------------------
#' create_empty_netCDF_file(
#'      data = data,
#'      has_T_timeAxis = TRUE,
#'      has_Z_verticalAxis = FALSE,
#'      time_bounds = annual_time_bounds,
#'      vert_bounds = NULL,
#'      var_attributes = var_attributes,
#'      time_attributes  = time_attributes,
#'      vertical_attributes = NULL,
#'      global_attributes = global_attributes,
#'      crs_attributes = crs_attributes,
#'      isGridded = TRUE,
#'      locations = locations,
#'      file = outFileName,
#'      force_v4 = TRUE,
#'      overwrite = TRUE
#'    )
#'
#' # run function populate_netcdf_from_array
#' populate_netcdf_from_array(
#'      file = outFileName,
#'      data = data,
#'      var_names = var_attributes$name,
#'      has_T_timeAxis = TRUE,
#'      has_Z_verticalAxis = FALSE,
#'      isGridded = TRUE,
#'      locations = locations,
#'      force_v4 = TRUE
#'    )
#'
#' @seealso \code{create_empty_netCDF_file}
#' @seealso \url{http://cfconventions.org/cf-conventions/cf-conventions.html} # for defining attributes
#' @seealso Demonstration of netCDF functionality in rSW2analysis vignette
#'
#' @export

populate_netcdf_from_array <- function(file, data, var_names = NULL,
                                       has_T_timeAxis, has_Z_verticalAxis,
                                       isGridded = TRUE, grid = NULL, locations,
                                       force_v4 = TRUE) {


  # ---------------------------------------------------------------------
  # Set up and checks ---------------------------------------------------
  # ---------------------------------------------------------------------

  stopifnot(requireNamespace("ncdf4"))
  if (force_v4) {
    # avoid "_FillValue" error in older versions of `raster` package
    stopifnot(utils::packageVersion("raster") >= "2.9.1")
  }

  stopifnot(file.exists(file) || !missing(locations)) # file and locations need to exist

  # open file, writeable --------------
  nc <- ncdf4::nc_open(file, write = TRUE)
  on.exit(ncdf4::nc_close(nc))
  
  # check  netcdf against data and inputs ---------------------------
  nc_dims <-  attributes(nc$dim)$names #dims of netcdf
  nn <- NCOL(data) # number of cols of data
  data_dims <- length(dim(data)) # ndim of data
  nvars <- length(var_names) #  vars names set by user

  if(has_T_timeAxis) stopifnot('time' %in% nc_dims)
  if(has_Z_verticalAxis) stopifnot('depth' %in% nc_dims)
  if(has_T_timeAxis & has_Z_verticalAxis) stopifnot(data_dims == 3) # if have both T and Z, data should be 3 dims
  if(!has_T_timeAxis & !has_Z_verticalAxis) stopifnot(nvars == nn) # if org by vars names should be equal to nl

  #print(paste("The file has", nc$nvars, "variables and",  nc$ndim,"dimensions"))
  #print(paste("The dimensions are", paste(nc_dims, collapse = ', ')))

  # ---------------------------------------------------------------------
  #  Locations and spatial info  ----------------------------------------
  # ---------------------------------------------------------------------
  
  crs <- ncdf4::ncatt_get(nc, varid = 'crs')[['proj']]
  
  if(!inherits(locations, "Spatial") & is.null(crs)){
    stop('Need CRS for locations data') # need crs if locations isn't spatially defined
  }
  
  if(isGridded) {
    if (inherits(locations, "Spatial")) {
      loc <- locations #sp::coordinates(locations)
    } else {
      loc <- sp::SpatialPoints(locations)
      sp::proj4string(loc) <- sp::CRS(crs)
    } 
    # Create grid from location values ---------------------------------
    sp::gridded(loc) = TRUE
    
    if(is.null(grid)) {
      grid_template <- raster::raster(loc)
      raster::extent(grid_template) <- raster::extent(loc)
    } else {
      grid_template <- rep(NA, raster::ncell(grid))
    }
    
    val_grid_ids <- raster::cellFromXY(grid_template, locations)
  }

  # ---------------------------------------------------------------------
  # Add data ------------------------------------------------------------
  # ---------------------------------------------------------------------

  for(k in seq(nvars)) {

    nc_names <- attributes(nc$var)$names
    stopifnot(var_names[k] %in% nc_names)
    var_nc_index <- grep(var_names[k], nc_names)[1]

    nc_var <- ncdf4::ncvar_get(nc, attributes(nc$var)$names[var_nc_index])
    nc_var_dims <- dim(nc_var) # lon, lat, then #vars time or depth of gridded 

    message(paste('The dimensionality of variable',var_names[k], 'is',
                paste(nc_var_dims, collapse = ', ')))
    
    # Set up chunksizes  ---------------------------------------------------
    if(has_Z_verticalAxis) {
      if(has_T_timeAxis) { # if time and depth are both TRUE , the third dimension
        z_chunksize <- dim(data)[3]
      } 
    }
    
    # ---------------------------------------------------------------------
    # NOT GRIDDED ---------------------------------------------------------
    # ---------------------------------------------------------------------
    if(isGridded == FALSE) {
      
      var_chunksizes <- nc_var_dims[1] # site
      
      #  -----------------------------------------------------------------------
      # add variable values ! --------------------------------------------------
      # ------------------------------------------------------------------------
      if(nvars > 1) {
        
        vals <- data[ ,k]
        
        try(ncdf4::ncvar_put(nc, varid = var_names[k],
                             vals = vals,
                             start = 1,
                             count = var_chunksizes))
        
      } else {
        if(has_T_timeAxis && has_Z_verticalAxis) {
          
          for(z in seq(z_chunksize)) { # by Z axis
            message('Adding depth layer ', z)
            for (t in seq_len(nn)) { # col by col - always time in this case
              
              vals <- data[, t, z]
              
              try(ncdf4::ncvar_put(nc, varid = var_names[k],
                                   vals = vals,
                                   start = c(1, t, z), #x-y-t-z
                                   count = c(var_chunksizes, 1, 1)))
            }
          }
        } else { 
          # write values, col by col -- n values can rep dif. vals, time, or depth
          for (n in seq_len(nn)) {
            
            vals <-  data[, n] # by time chunk or var chunk

            var_start <-  c(1, n)
            
            try(ncdf4::ncvar_put(nc, varid = var_names[k],
                                 vals = vals,
                                 start = var_start,
                                 count = c(var_chunksizes, 1)))
          }
        }
      }
    }
    
    #  GRIDDED --------------------------------------------------------------
    if(isGridded == TRUE) {
      
      # more checks based on dim of var ----------------------------------------
      
      if(has_T_timeAxis & !has_Z_verticalAxis) stopifnot(nc_var_dims[3] == nn)
      if(!has_T_timeAxis & has_Z_verticalAxis) stopifnot(nc_var_dims[3] == nn)
      
      var_chunksizes <- c(nc_var_dims[1], nc_var_dims[2]) # lon, lat
      
      #  -----------------------------------------------------------------------
      # add variable values ! --------------------------------------------------
      # ------------------------------------------------------------------------
      if(nvars > 1) {
        temp <- grid_template
        temp[val_grid_ids] <- data[, k]
        vals <- matrix(temp,  ncol = var_chunksizes[2])
        
        var_start <-  c(1, 1)
        
        try(ncdf4::ncvar_put(nc, varid = var_names[k],
                             vals = vals,
                             start = var_start,
                             count = var_chunksizes))
        
      } else {
        if(has_T_timeAxis && has_Z_verticalAxis) {
          
          temp <- grid_template
          
          for(z in seq(z_chunksize)) { # by Z axis
            message('Adding depth layer ', z)
            for (t in seq_len(nn)) { # col by col - always time in this case
              
              temp[val_grid_ids] <- data[, t, z]
              vals <- matrix(temp, ncol = as.numeric(var_chunksizes[2]))
              
              try(ncdf4::ncvar_put(nc, varid = var_names[k],
                                   vals = vals,
                                   start = c(1, 1, t, z), #x-y-t-z
                                   count = c(var_chunksizes, 1, 1)))
            }
          }
        } else {
          # write values, col by col -- n values can rep dif. time, or depth
          for (n in seq_len(nn)) {
            
            temp <- grid_template
            temp[val_grid_ids] <- data[, n]
            vals <- matrix(temp,  ncol = var_chunksizes[2])

            var_start <-  c(1, 1, n)

            try(ncdf4::ncvar_put(nc, varid = var_names[k],
                                 vals = vals,
                                 start = var_start,
                                 count = c(var_chunksizes, 1)))
          }
        }
      }
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

#' Read data from a netcdf into an array or matrix
#'
#' @param x a netCDF fiile
#' @return an array
#' @export
#' 
read_netCDF_to_array <- function(x, locations) {
  
  # open file, writeable --------------
  nc <- ncdf4::nc_open(x, write = TRUE)
  on.exit(ncdf4::nc_close(nc))
  
  # figure out dimensionality
  nc_dims <-  attributes(nc$dim)$names #dims of netcdf
  
  if('site' %in% nc_dims) isGridded = FALSE
  if('lat' %in% nc_dims) isGridded = TRUE
  
  has_T_timeAxis <-  ifelse('time' %in% nc_dims, TRUE, FALSE)
  has_Z_verticalAxis <-  ifelse('depth' %in% nc_dims, TRUE, FALSE)
  
  # Define variables
  nc_var_names <- attributes(nc$var)$names
  nc_var_names <- grep('bnds|crs|lat|lon', nc_var_names, invert = TRUE, value = TRUE)
  
  if(isGridded == TRUE) {
    
    # location stuff  ---------------------------------------------------------
    loc <- sp::SpatialPoints(locations)
    # Create grid from location values ----------------------------------------
    sp::gridded(loc) = TRUE
    grid_template <- raster::raster(loc)
    raster::extent(grid_template) <- raster::extent(loc)
    
    val_ids <- raster::cellFromXY(grid_template, locations)
    
    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)
    
    locDims <- c(nc_var_dims[1] * nc_var_dims[2])
    dimtz <- nc_var_dims[3]
    dimz <- if(has_T_timeAxis && has_Z_verticalAxis) nc_var_dims[4]
    
    
  } else {
    
    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)
    
    val_ids <- seq_len(nc_var_dims[1])
    
    locDims <- nc_var_dims[1]
    dimtz <- nc_var_dims[2]
    dimz <- if(has_T_timeAxis && has_Z_verticalAxis) nc_var_dims[3]
    
  }
    
  # ---------------------------------------------------------------------------
  # Put variables into array
  # ---------------------------------------------------------------------------
  
  # multi variable
  if(length(nc_var_names) > 1) { 
    
    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)
    newArray <- array( , dim = c(locDims, 
                                 length(nc_var_names)))
    
    for(k in 1:length(nc_var_names)) { 
      nc_var <- ncdf4::ncvar_get(nc, nc_var_names[k])
      v <-  c(nc_var)
      newArray[, k] <- v[val_ids]
    }
    
  } else {
    # values
    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)
    
    if(has_T_timeAxis && has_Z_verticalAxis) {
      # if there is a time AND depth axis
      
      newArray <- array(, dim = c(locDims, 
                                  dimtz, dimz))
      
      for(z in seq(dimz)){
        
        for(i in seq(dimtz)) {
          
          if(isGridded == TRUE) v <- c(nc_var[, , i, z]) else v <- c(nc_var[, i, z])
          newArray[, i, z] <- v[val_ids]
          
        }
      }
      
    } else {
      # if there is a time OR depth axis
      newArray <- array(, dim = c(locDims, dimtz))
      
      for(i in seq(dimtz)) {
        
        if(isGridded == TRUE) v <- c(nc_var[, , i]) else v <- c(nc_var[, i])
        newArray[, i] <- v[val_ids]
        
      }
    }
  }

  return(newArray)
}

#' Calculate "nominal resolution" of grid
#'
#' @param grid A raster object
#' @param sites A raster object. A numeric array, matrix, or data.frame where each
#'  row is a site and the columns contain values for Longitude and Latitude.
#' @param cell_areas_km2 A string of numeric values. Equal in length to the length
#'  of sites. Area each cell represents in km2.
#'
#' @references CMIP6 Global Attributes, DRS, Filenames, Directory Structure, and CV’s
#'   10 September 2018 (v6.2.7)
#'   Appendix 2: Algorithms for Defining the "nominal_resolution" Attribute
#'   \url{https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit#bookmark=id.ibeh7ad2gpdi}
#'
#' @examples
#' r <- raster::raster(
#'   xmn = -120, xmx = -90,
#'   ymn = 30, ymx = 50,
#'   crs ="+init=epsg:4326",
#'   resolution = c(0.5, 0.5)
#' )
#' r[] <- seq_len(prod(dim(r)))
#' xy <- raster::sampleRandom(r, size = 50, sp = TRUE)
#' grid_cell_area <- calculate_cell_area(sites = xy, grid = r)
#'
#' calculate_nominal_resolution(
#'   grid = r,
#'   sites = xy,
#'   cell_areas_km2 = grid_cell_area[, "km2"]
#' )
#'
#' @export
  calculate_nominal_resolution <- function(grid, sites, cell_areas_km2) {
  stopifnot(requireNamespace("geosphere"))
  # For a land surface model calculated on its own grid,
  # include all land grid cells

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

    # Calculate the mean over all cells of dmax, weighting each by the
    # grid-cell's area (A)
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
