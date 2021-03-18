#'  Create a structured, empty netCDF file
#'
#'  This function creates a structured netCDF following CF 1.8 standards with
#'  metadata from an array, but without data.
#'
#'  The user provides an array and specifies spatial information,
#'  dimensionality, and metadata in order to create an empty netCDF.
#'
#'  The netCDF always includes at least two dimensions: Latitude and Longitude.
#'  Information about these dimensions are taken from the \var{locations},
#'  \var{isGridded}, \var{crs_attributes}, and \var{grid} arguments. One of
#'  \var{locations} or \var{grid} needs to be provided, but not both.
#'
#'  Additional dimensions are added by setting \var{has_T_timeAxis} and/or
#'  \var{has_Z_verticalAxis} to \code{TRUE}. If both flags are set to
#'  \code{FALSE} then the netCDF is two dimensions with one or more variables.
#'  The number of variables is set in the \var{var_attributes} argument. If
#'  either \var{has_T_timeAxis} \emph{or} \var{has_Z_verticalAxis} is set to
#'  \code{TRUE} then a third dimension is added to the netCDF. If \emph{both}
#'  \var{has_T_timeAxis} and \var{has_Z_verticalAxis} are set to TRUE, then the
#'  netCDF will have four dimensions, with time as the third, and vertical as
#'  the fourth. Information about these dimensions is set in the respective
#'  \var{bounds} and \var{attributes} arguments. \var{has_T_timeAxis} and/or
#'  \var{has_Z_verticalAxis} is set to \code{TRUE} the netCDF can \emph{only}
#'  have one variable.
#'
#'  The \var{data} object is used to set up the size and dataType in the netCDF.
#'  The metadata needs to match the data. This object contains the data that you
#'  intend to populate the netCDF with. The array should be set up so that each
#'  row is a site's info, and each column is a value for variables, time or
#'  vertical. If the netCDF is 4d (time and vertical == \code{TRUE}, then need
#'  to have a 3-d data array, where each additional dimension contains values
#'  for each vertical horizon.
#'
#'  For variable names, Use CMIP6 standard variable names where available.
#'  The variable names are contained in the
#'  [CMIP6-cmor-tables]
#'  (https://github.com/PCMDI/cmip6-cmor-tables/tree/master/Tables).
#'
#' @param data A numeric array.
#' @param has_T_timeAxis A logical value. Indicates that the netCDF created will
#'     have a third, time dimension.
#' @param has_Z_verticalAxis A logical value. Indicates that the netCDF created
#'     will have a vertical (e.g. soil profile depths) dimension.
#'     If \var{has_T_timeAxis} is set to \code{FALSE} then the Z axis will be
#'     the third dimension. If \var{has_T_timeAxis} is set to \code{TRUE} then
#'     the Z axis will be the fourth dimension.
#' @param time_bounds A numeric vector that continuously lists the lower and
#'    upper bounds of each time dimension value. In the absence of a time
#'    dimension, this argument can be used to define the calculation period of
#'    the variable(s).
#' @param vert_bounds A numeric vector that continuously lists the lower and
#'     upper bounds of each vertical dimension value.
#' @param var_attributes A list of named character strings defining the
#'    variables of the netCDF.
#' @param time_attributes A list of named character strings defining the time
#'   dimension of the netCDF.
#' @param vertical_attributes A list of named character strings defining the
#'   vertical dimension of the netCDF.
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
#' @return This function is used for the side-effect of creating a file.
#'
#' @examples
#' #############################################################################
#' # example - create an empty netCDF with a three dimensions (lat, long, time)
#' #############################################################################
#'
#' # create dummy data ---------------------------------------------------------
#'
#' someData <- rnorm(100, 7, 30)
#' data <- array(someData, c(10, 10))
#'
#' locations <- data.frame(X_WGS84 = c(rep(-124.5938, 5), rep(-124.5312, 5)),
#'                         Y_WGS84 = rep(c(47.90625, 47.96875, 48.03125,
#'                                       48.09375, 48.15625), 2))
#'
#' annual_time_bounds <- c(43737, 44102, 44102, 44467, 44467, 44832, 44832,
#'                         45197, 45197, 45563, 45563, 45928, 45928, 46293,
#'                         46293, 46658, 46658, 47024, 47024, 47389)
#'
#' outFileName <- "dummynetcdf.nc"
#'
#' # define attributes ---------------------------------------------------------
#'
#' # time attribute
#' time_attributes <- list(
#'    units = "days since 1900-01-01",
#'    calendar = "standard",
#'    unlim = "TRUE",
#'    vals = c(43554, 43920, 44285, 44650, 45015, 45381, 45746, 46111, 46476,
#'             46842)# mid point of year
#'    )
#'
#' # variable attributes
#' var_attributes <- list(
#'    name = "JulyTemp",
#'    long_name = "Annual Mean July Temperature",
#'    units = "Celsius",
#'    description = "example data!",
#'    grid_mapping = "crs: lat lon"
#'    )
#'
#' # CRS attributes
#' crs_attributes <- list(
#'   crs_wkt = sp::wkt(sp::CRS(SRS_string = "EPSG:4326")),
#    grid_mapping_name = "latitude_longitude",
#'   longitude_of_prime_meridian = 0.0,
#'   semi_major_axis = 6378137.0,
#'   inverse_flattening = 298.257223563
#' )
#'
#' # global attributes
#' global_attributes <- list(
#'    title = "",
#'    institution = "Southwest Biological Science Center,
#'                   U.S. Geological Survey",
#'    description = "how this data was made",
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
#'    parent_variant_label = "r1i1p1",
#'    product = "model-output",
#'    projection = "Geographic",
#'    grid = "WGS84",
#'    grid_label = "gn",
#'    nominal_resolution = "10 km",
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
#' unlink(outFileName)
#'
#' @seealso \code{populate_netcdf_from_array}
#' @seealso \url{http://cfconventions.org/cf-conventions/cf-conventions.html}
#'
#' @export

create_empty_netCDF_file <- function(data, has_T_timeAxis = FALSE,
  has_Z_verticalAxis = FALSE, time_bounds, vert_bounds, var_attributes,
  time_attributes, vertical_attributes, global_attributes,
  crs_attributes,
  xy_attributes = list(
    name = c("lon", "lat"),
    standard_name = c("longitude", "latitude"),
    long_name = c("Longitude", "Latitude"),
    units = c("degrees_east", "degrees_north")
  ),
  isGridded = TRUE, grid = NULL, locations,
  file, force_v4 = TRUE, overwrite = FALSE, verbose = FALSE) {

  # ---------------------------------------------------------------------
  # Checks --------------------------------------------------------------
  # ---------------------------------------------------------------------
  stopifnot(requireNamespace("ncdf4"))
  if (force_v4) {
    # avoid "_FillValue" error in older versions of `raster` package
    stopifnot(utils::packageVersion("raster") >= "2.9.1")
  }

  if (is.null(grid) && missing(locations)) {
    stop("Error: Neither a grid or locations data present. Must supply one at
         least one of these arguments to the function.")
  }

  # check that CRS is present
  crs <- crs_attributes[["crs_wkt"]]
  if (!missing(locations) && is.null(crs)) {
    stop("Error: If you are giving locations and not a grid, need to define the
         CRS in the crs_attribute[['crs_wkt']] argument")
  }
  # check that CRS is valid
  tmp <- try(sf::st_crs(crs), silent = TRUE)
  if (!inherits(tmp, "crs") || tmp == sf::NA_crs_) {
    stop("`crs_attributes[[\"crs_wkt\"]]` does not represent a valid CRS.")
  }
  # check that CRS definition matches CRS of locations.
  if (inherits(locations, "Spatial")) {
    crsL <- sp::wkt(raster::crs(locations))
  }

  if (inherits(locations, "sf")) {
    crsL <- sf::st_crs(locations)
  }

  if (exists("crsL")) {
    if (sf::st_crs(crsL) != sf::st_crs(crs)) {
      stop(paste0("Error: The CRS given in crs_attributes[[crs_wkt]] needs to
                  match the CRS of the locations objects. Currently,
                  crs_attributes[[crs_wkt]] is ", crs, "and the CRS of the
                  locations arguments is", crsL))
    }
  }

  if (file.exists(file)) {
    if (overwrite) {
      unlink(file)
    } else {
      stop("File ", shQuote(basename(file)), " exists and 'overwrite' is FALSE")
    }
  }

  nl <- NCOL(data)

  if (has_T_timeAxis == TRUE) {
    tn <- length(time_attributes$vals)

    if (is.null(time_bounds)) {
      stop("Need to define time bounds data for time dimension")
    }

    if (tn * 2 != length(time_bounds)) {
      stop("Need to define bounds (min and max) for each time value in time
           attributes")
    }

    if (tn > 1 && tn != nl) {
      stop("number of values in time dimension should either be of length 1
      (if all variable in the dataset represent the same measurement time) or
      equal to the number of columns in the dataset (dataset is a time series
           of one variable)")
    }
  }

  if (has_Z_verticalAxis == TRUE) {
    zn <- length(vertical_attributes$vals)

    if (is.null(vert_bounds)) {
      stop("Need to define vertical bounds data for vertical dimension")
      }

    if (zn * 2 != length(vert_bounds)) {
      stop("Number of vertical layers need to be equal to defined values")
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
                   char = NULL, byte = NULL, short = -128L,
                   integer =  -2147483647L,
                   float = -3.4e+38, double = -1.7e+308)

  # location and spatial info  ------------------------------------------------

  # Note: xvals should be organized from west to east, yvals from south to north
  if (!is.null(grid)) {
    # coordinates of cell centers
    xvals <- raster::xFromCol(grid, seq_len(raster::ncol(grid)))
    yvals <- raster::yFromRow(grid, seq_len(raster::nrow(grid)))
    grid_halfres <- raster::res(grid) / 2

  } else {
    if (isGridded) {
      if (is(locations, "SpatialGrid")) {
        loc <- locations
      } else {
        loc <- rSW2st::as_points(locations, to_class = "sp", crs = crs)
        sp::gridded(loc) <- TRUE # Converts to SpatialGrid or SpatialPixel
        loc <- as(loc, Class = "SpatialGrid") # Make sure this is SpatialGrid
      }

      tmp1 <- sp::gridparameters(loc)
      grid_halfres <- tmp1[, "cellsize"] / 2
      tmp_coord <- sp::coordinates(loc) # coordinates of cell centers
      stopifnot(nrow(tmp_coord) == prod(tmp1[, "cells.dim"]))
      xvals <- sort(unique(tmp_coord[, 1]))
      yvals <- sort(unique(tmp_coord[, 2]))

    } else {
      loc <- rSW2st::as_points(locations, to_class = "sf", crs = crs)
      nloc <- nrow(loc)
      tmp_coord <- sf::st_coordinates(loc) # coordinates of point locations
      xvals <- tmp_coord[, 1]
      yvals <- tmp_coord[, 2]
    }
  }

  # crs attributes setup & info ------------------------------------------------
  if (!missing(crs_attributes)) {

    if ("crs_wkt" %in% names(crs_attributes)) {
      crs_wkt <- crs_attributes[["crs_wkt"]]
      crs_attributes[["crs_wkt"]] <- NULL
    } else {
      stop("Need 'crs_wkt' in crs_attributes")
    }

    ns_att_crs <- names(crs_attributes)

  }

  # Time dimension setup & info -----------------------------------------------
  if (has_T_timeAxis && !is.null(time_bounds)) {
    t_chunksize <- length(time_attributes[["vals"]])
  }

  if (has_T_timeAxis) {
    if (!missing(time_attributes) || !is.null(time_attributes)) {

      if ("units" %in% names(time_attributes)) {
        time_units <- time_attributes[["units"]]
        time_attributes[["units"]] <- NULL
      } else {
        stop("Need units attribute in time attribute list")
      }

      if ("calendar" %in% names(time_attributes)) {
        time_cal <- time_attributes[["calendar"]]
        time_attributes[["calendar"]] <- NULL
      } else {
        stop("Need calendar attribute in time attribute list")
      }

      if ("vals" %in% names(time_attributes)) {
        time_vals <- time_attributes[["vals"]]
        time_attributes[["vals"]] <- NULL
      } else {
        stop("Need vals attribute in time attribute list")
      }

      ns_att_time <- names(time_attributes)
    }
  } else {
    time_vals <- 0
  }

  # Vertical info  -------------------------------------------------------------
  if (has_Z_verticalAxis && !is.null(vert_bounds)) {
    if (has_T_timeAxis) {#if time and vertical are both TRUE, the 3rd dimension
      z_chunksize <- dim(data)[3]
      stopifnot(z_chunksize ==  zn)
    } else { # no time, then we can assume that the vertical dimension is the 2d
      z_chunksize <- nl
      stopifnot(z_chunksize ==  zn)
    }
  }

  if (has_Z_verticalAxis) {
    if (!missing(vertical_attributes) || !is.null(vertical_attributes)) {

      if ("units" %in% names(vertical_attributes)) {
        vert_units <- vertical_attributes[["units"]]
        vertical_attributes[["units"]] <- NULL
      } else {
        stop("Need units attribute in vertical attribute list")
      }

      if ("vals" %in% names(vertical_attributes)) {
        vert_vals <- vertical_attributes[["vals"]]
        vertical_attributes[["vals"]] <- NULL
      } else {
        stop("Need vals attribute in vertical attribute list")
      }

      ns_att_vert <- names(vertical_attributes)

    }
  } else {
    vert_vals <- 0
  }

  # Variable info  -------------------------------------------------------------
  if (!missing(var_attributes)) {
    if ("name" %in% names(var_attributes)) {
      var_names <- var_attributes[["name"]]
      var_attributes[["name"]] <- NULL
    } else {
      stop("Need name attribute in variable attribute list")
    }

    if (!"long_name" %in% names(var_attributes)) {
      var_attributes[["long_name"]] <- var_names
    }

    if ("units" %in% names(var_attributes)) {
      var_units <- var_attributes[["units"]]
      var_attributes[["units"]] <- NULL
    } else {
      stop("Need unit attribute in variable attribute list")
    }

    if (isGridded) {
        if (!"grid_mapping" %in% names(var_attributes)) {
          stop("Need grid_mapping attribute in variable attribute list")
        }
    }

    ns_att_vars <- names(var_attributes)

  }

  # ----------------------------------------------------------------------------
  # -- Setup info for netCDF file ----------------------------------------------
  # ----------------------------------------------------------------------------

  # Starts and chunksizes ------------------------------------------------------
  if (isGridded) {
    var_chunksizes <- c(length(xvals), length(yvals))
    var_start <- c(1, 1)

  } else {
    # locations aren't gridded
    var_chunksizes <- nloc
    var_start <- 1
  }

  if (has_Z_verticalAxis) {
    var_chunksizes <- c(var_chunksizes, z_chunksize)
    var_start <- c(var_start, 1)
  }

  if (has_T_timeAxis) {
    var_chunksizes <- c(var_chunksizes, t_chunksize)
    var_start <- c(var_start, 1)
  }

  # define dimensions ----------------------------------------------------------

  #  bounds dimension
  bnddim <- ncdf4::ncdim_def(name = "bnds", units = "", vals = seq_len(2L),
                                           create_dimvar = FALSE)

  # x and y dimension
  if (isGridded) {
    xdim <- ncdf4::ncdim_def(
      name = xy_attributes[["name"]][1],
      longname = xy_attributes[["long_name"]][1],
      units = xy_attributes[["units"]][1],
      vals = xvals
    )
    ydim <- ncdf4::ncdim_def(
      name = xy_attributes[["name"]][2],
      longname = xy_attributes[["long_name"]][2],
      units = xy_attributes[["units"]][2],
      vals = yvals
    )

  } else {
    idim <- ncdf4::ncdim_def(name = "site", longname = "SOILWAT2 simulation
                             sites", units = "1",
                             vals = seq_len(nloc))
  }

  # vertical dimension
  if (has_Z_verticalAxis) {
    zdim <-  ncdf4::ncdim_def(name = "vertical",
                              units = vert_units,
                              vals = vert_vals)
  }

  # time dimension
  if (has_T_timeAxis) {
      tdim <-  ncdf4::ncdim_def(name = "time",
                                units = time_units,
                                calendar = time_cal,
                                vals = time_vals)
  }

  # define dimensionality of netCDF variables ----------------------------------
  var_dims <- if (isGridded) list(xdim, ydim) else list(idim)

  if (has_Z_verticalAxis) {
    var_dims <- c(var_dims, list(zdim))
  }

  if (has_T_timeAxis) {
    var_dims <- c(var_dims, list(tdim))
  }

  if (length(time_vals) > 1 || length(vert_vals) > 1) nn <- 1 else nn <- nl

  var_defs <- lapply(seq_len(nn), function(k)
    ncdf4::ncvar_def(name = var_names[k], units = var_units[k],
      dim = var_dims, chunksizes = var_chunksizes, missval = NAflag,
      prec = ncdf4_datatype))

  # add x and y as variables if not gridded
  if (!isGridded) {
    xvar <- ncdf4::ncvar_def(
      name = xy_attributes[["name"]][1],
      longname = xy_attributes[["long_name"]][1],
      units = xy_attributes[["units"]][1],
      dim = list(idim),
      chunksizes = var_chunksizes[1],
      missval = NAflag,
      prec = "double"
    )
    yvar <- ncdf4::ncvar_def(
      name = xy_attributes[["name"]][2],
      longname = xy_attributes[["long_name"]][2],
      units = xy_attributes[["units"]][2],
      dim = list(idim),
      chunksizes = var_chunksizes[1],
      missval = NAflag,
      prec = "double"
    )

    var_defs <- c(var_defs, list(yvar, xvar))
  }

  # CRS defintion --------------------------------------------------------------
  crsdef <- ncdf4::ncvar_def(name = "crs", units = "", dim = list(),
    missval = NULL, prec = "integer")

  # define dimension bounds ----------------------------------------------------
  if (isGridded) {
    bnds_name <- paste0(xy_attributes[["name"]][1:2], "_bnds")

    xbnddef <- ncdf4::ncvar_def(
      name = bnds_name[1],
      units = "",
      dim = list(bnddim, xdim),
      missval = NULL,
      chunksizes = c(2L, var_chunksizes[1]),
      prec = "double"
    )

    ybnddef <- ncdf4::ncvar_def(
      name = bnds_name[2],
      units = "",
      dim = list(bnddim, ydim),
      missval = NULL,
      chunksizes = c(2L, var_chunksizes[2]),
      prec = "double"
    )
  }

  if (has_T_timeAxis) {
      tbnddef <- ncdf4::ncvar_def(name = "time_bnds", units = "",
                                dim = list(bnddim, tdim), missval = NULL,
                                chunksizes = c(2L, 1L), prec = "double")
  }

  if (has_Z_verticalAxis) {
      vertbnddef <- ncdf4::ncvar_def(name = "vertical_bnds", units = "",
                                    dim = list(bnddim, zdim), missval = NULL,
                                    chunksizes = c(2L, 1L), prec = "double")
  }

  nc_dimvars <- if (isGridded) list(xbnddef, ybnddef) else list()

  if (has_Z_verticalAxis) {
    nc_dimvars <- c(nc_dimvars, list(vertbnddef))
  }

  if (has_T_timeAxis) {
    nc_dimvars <- c(nc_dimvars, list(tbnddef))
  }

  # ----------------------------------------------------------------------------
  #--- create empty netCDF file ------------------------------------------------
  # ----------------------------------------------------------------------------

  dir.create(dirname(file), recursive = TRUE, showWarnings = FALSE)

  nc <- ncdf4::nc_create(filename = file,
    vars = c(nc_dimvars, list(crsdef), var_defs), force_v4 = force_v4)

  on.exit(ncdf4::nc_close(nc))

 #--- write values of dimension bounds -----------------------------------------
  if (isGridded) {
    try(ncdf4::ncvar_put(nc, varid = bnds_name[1],
                        vals = rbind(xvals - grid_halfres[1],
                                     xvals + grid_halfres[1]),
                        start = c(1, 1), count = c(2L, var_chunksizes[1])))

    try(ncdf4::ncvar_put(nc, varid = bnds_name[2],
                         vals = rbind(yvals + grid_halfres[2],
                                      yvals - grid_halfres[2]),
                         start = c(1, 1), count = c(2L, var_chunksizes[2])))
  } else {

    try(ncdf4::ncvar_put(nc, varid = xy_attributes[["name"]][1],
                         vals = xvals,
                         start = 1, count = var_chunksizes[1]))

    try(ncdf4::ncvar_put(nc, varid = xy_attributes[["name"]][2],
                         vals = yvals,
                         start = 1, count = var_chunksizes[1]))
  }

  if (has_Z_verticalAxis) {# top and bottom of each soil layer
    try(ncdf4::ncvar_put(nc, varid = "vertical_bnds",
                         vals = vert_bounds,
                         start = c(1, 1), count = c(2L, z_chunksize)))
  }

  if (has_T_timeAxis) { # beginning and end of each TP
        try(ncdf4::ncvar_put(nc, varid = "time_bnds",
                             vals = time_bounds,
                             start = c(1, 1), count = c(2, t_chunksize)))
  }

  #--- add attributes ----------------------------------------------------------

  # add standard_name attribute of x/y variables
  if ("standard_name" %in% names(xy_attributes)) {
    for (k in seq_len(2)) {
      ncdf4::ncatt_put(
        nc,
        varid = xy_attributes[["name"]][k],
        attname = "standard_name",
        attval = xy_attributes[["standard_name"]][k]
      )
    }
  }

  # add dimension attributes --------------------------------
  if (isGridded) {
   ncdf4::ncatt_put(nc, xy_attributes[["name"]][1], "axis", "X")
   ncdf4::ncatt_put(nc, xy_attributes[["name"]][1], "bounds", bnds_name[1])
   ncdf4::ncatt_put(nc, xy_attributes[["name"]][2], "axis", "Y")
   ncdf4::ncatt_put(nc, xy_attributes[["name"]][2], "bounds", bnds_name[2])
   }

  if (has_Z_verticalAxis) {
    ncdf4::ncatt_put(nc, "vertical", "axis", "Z")
    ncdf4::ncatt_put(nc, "vertical", "bounds", "vertical_bnds")

    for (natt in ns_att_vert) {
      ncdf4::ncatt_put(nc, varid = "vertical", attname = natt,
                         attval = vertical_attributes[[natt]])
    }
  }

  if (has_T_timeAxis) {
    ncdf4::ncatt_put(nc, "time", "axis", "T")
    ncdf4::ncatt_put(nc, "time", "bounds", "time_bnds")

    for (natt in ns_att_time) {
      ncdf4::ncatt_put(nc, varid = "time", attname = natt,
                       attval = time_attributes[[natt]])
    }
  }

  # add variable attributes  ---------------------------------------------------
  for (k in seq_len(nn)) {
    for (natt in ns_att_vars) {
      ncdf4::ncatt_put(nc, varid = var_names[k], attname = natt,
                       attval = var_attributes[[natt]][k])
    }
  }

  # add coordinate system attributes -------------------------------------------
  for (natt in ns_att_crs) {
     ncdf4::ncatt_put(nc, varid = "crs", attname = natt,
                      attval = crs_attributes[[natt]])
   }

  ncdf4::ncatt_put(nc, "crs", attname = "crs_wkt", crs_wkt)

  # add global attributes ------------------------------------------------------
  ncdf4::ncatt_put(nc, varid = 0, attname = "Conventions", attval = "CF-1.8")

  ncdf4::ncatt_put(nc, varid = 0, attname = "created_by",
    attval = paste0(R.version[["version.string"]], ", R packages ",
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

  if (verbose) {
    message(paste("The file has", nc$nvars, "variables and",  nc$ndim,
                  "dimensions"))
  }

  invisible(TRUE)
}

#' Populate an empty netCDF with values stored in an array
#'
#' This function add values to a structured netCDF created with the
#' \code{create_empty_netCDF_file} function.
#' The data array provided as an argument to this function should be the same as
#' that provided to the create_empty_netCDF_file function.
#' One of \var{locations} or \var{grid} needs to be provided, but not both.
#'
#' @param file A character string. File path to a pre-created, empty netCDF.
#' @param data A numeric array.
#' @param var_name A character string equal to the length of the number of
#'   variables. The length of var_name should either be 1 if there is either a
#'   time and/or vertical dimension \emph{or} equal to the number of columns
#'   in the array if there is no time or vertical dimension.
#' @param has_T_timeAxis A logical value. Indicates that the netCDF file
#'     has a third, time dimension.
#' @param has_Z_verticalAxis A logical value. Indicates that the netCDF file
#'     has a vertical (e.g. soil profile depths) dimension. If
#'      \var{has_T_timeAxis} is set to \code{FALSE} then the Z axis will be the
#'      third dimension. If \var{has_T_timeAxis} is set to \code{TRUE} then the
#'      Z axis will be the fourth dimension.
#' @param isGridded A logical value. Represents whether the location data is on
#'   a regular grid or not.
#' @param grid filename (character). File containing the grid information
#'   (i.e. resolution, extent) of the data. Supported file types are the native
#'   raster package format and those that can be read via rgdal.
#' @param locations A SpatialPoints object or a matrix or data.frame with two
#'   columns containing long and lat values. Data must be organized by long, lat
#' @param force_v4 A logical value. Force version 4 of netCDF.
#'
#' @return This function is used for the side-effect of filling a file.
#'
#' @examples
#' #############################################################################
#' # example - create an empty netCDF with a three dimensions (lat, long, time)
#' #############################################################################
#'
#' # create dummy data ---------------------------------------------------------
#'
#' someData <- rnorm(100, 7, 30)
#' data <- array(someData, c(10, 10))
#'
#' locations <- data.frame(X_WGS84 = c(rep(-124.5938, 5), rep(-124.5312, 5)),
#'                         Y_WGS84 = rep(c(47.90625, 47.96875, 48.03125,
#'                         48.09375, 48.15625), 2))
#'
#' annual_time_bounds <- c(43737, 44102, 44102, 44467, 44467, 44832, 44832,
#'                         45197, 45197, 45563, 45563, 45928, 45928, 46293,
#'                         46293, 46658, 46658, 47024, 47024, 47389)
#'
#' outFileName <- "dummynetcdf.nc"
#'
#' # define attributes ---------------------------------------------------------
#'
#' # time attribute
#' time_attributes <- list(
#'    units = "days since 1900-01-01",
#'    calendar = "standard",
#'    unlim = "TRUE",
#'    vals = c(43554, 43920, 44285, 44650, 45015, 45381, 45746, 46111, 46476,
#'             46842)# mid point of year
#'    )
#'
#' # variable attributes
#' var_attributes <- list(
#'    name = "JulyTemp",
#'    long_name = "Annual Mean July Temperature",
#'    units = "Celsius",
#'    description = "example data!",
#'    grid_mapping = "crs: lat lon"
#'    )
#'
#' # CRS attributes
#' crs_attributes <- list(
#'  crs_wkt = sp::wkt(sp::CRS(SRS_string = "EPSG:4326")),
#'  grid_mapping_name = "latitude_longitude",
#'  longitude_of_prime_meridian = 0.0,
#'  semi_major_axis = 6378137.0,
#'  inverse_flattening = 298.257223563
#'  )
#'
#' # global attributes
#' global_attributes <- list(
#'    title = "",
#'    institution = "Southwest Biological Science Center,
#'                    U.S. Geological Survey",
#'    description = "how this data was made",
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
#'    parent_variant_label = "r1i1p1",
#'    product = "model-output",
#'    projection = "Geographic",
#'    grid = "WGS84",
#'    grid_label = "gn",
#'    nominal_resolution = "10 km", # calculate_nominal_resolution
#'    further_info_url = "https://github.com/DrylandEcology/",
#'    contact = "you@email.com"
#'    )
#'
#' # run create_empty_netCDF_file function -------------------------------------
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
#'  unlink(outFileName)
#'
#' @seealso \code{create_empty_netCDF_file}
#' @seealso \url{http://cfconventions.org/cf-conventions/cf-conventions.html}
#' @seealso Demonstration of netCDF functionality in rSW2analysis vignette
#'
#' @export

populate_netcdf_from_array <- function(file, data, var_names = NULL,
                                       has_T_timeAxis, has_Z_verticalAxis,
                                       isGridded = TRUE, grid = NULL, locations,
                                       force_v4 = TRUE, verbose = FALSE) {


  # ---------------------------------------------------------------------
  # Set up and checks ---------------------------------------------------
  # ---------------------------------------------------------------------

  stopifnot(requireNamespace("ncdf4"))
  # file and locations need to exist
  stopifnot(file.exists(file) || !missing(locations))

  # open file, writeable --------------
  nc <- ncdf4::nc_open(file, write = TRUE)
  on.exit(ncdf4::nc_close(nc))

  # check  netCDF against data and inputs ---------------------------
  nc_dims <-  attributes(nc$dim)$names #dims of netCDF
  nn <- NCOL(data) # number of cols of data
  data_dims <- length(dim(data)) # ndim of data
  nvars <- length(var_names) #  vars names set by user

  if (has_Z_verticalAxis) stopifnot("vertical" %in% nc_dims)
  if (has_T_timeAxis) stopifnot("time" %in% nc_dims)
  # if have both T and Z, data should be 3 dims
  if (has_T_timeAxis & has_Z_verticalAxis) stopifnot(data_dims == 3)
  # if org by vars names should be equal to nl
  if (!has_T_timeAxis & !has_Z_verticalAxis) stopifnot(nvars == nn)

  if (verbose) {
    print(paste("The file has", nc$nvars, "variables and",
                nc$ndim, "dimensions"))
    print(paste("The dimensions are", paste(nc_dims, collapse = ",")))
  }

  # ---------------------------------------------------------------------
  #  Locations and spatial info  ----------------------------------------
  # ---------------------------------------------------------------------

  crs <- ncdf4::ncatt_get(nc, varid = "crs")[["crs_wkt"]]

  # check that CRS definition matches CRS of locations.
  if (inherits(locations, "Spatial")) {
    crsL <- sp::wkt(raster::crs(locations))
  }

  if (inherits(locations, "sf")) {
    crsL <- sf::st_crs(locations)
  }

  if (exists("crsL")) {
    if (sf::st_crs(crsL) != sf::st_crs(crs)) {
      stop(paste0("Error: The CRS of the netCDF needs to
                  match the CRS of the locations objects. Currently,
                  the crs_wkt of the netCDF is ", crs, "and the CRS of the
                  locations arguments is", crsL))
    }
  }

  # if gridded, get grid ids for inserting values into netCDF
  if (isGridded) {
    loc <- rSW2st::as_points(locations, to_class = "sp", crs = crs)

    if (is.null(grid)) {       # make grid
      sp::gridded(loc) <- TRUE
      grid_template <- raster::raster(loc)
      raster::extent(grid_template) <- raster::extent(loc)
    } else {
      grid_template <- raster::init(grid, fun = function(x) rep(NA, x))
    }

    val_grid_ids <- raster::cellFromXY(grid_template, loc)
  }

  # ---------------------------------------------------------------------
  # Add data ------------------------------------------------------------
  # ---------------------------------------------------------------------
  if (NCOL(data) == 1 && is.null(dim(data))) {
    data <- matrix(data, ncol = 1, dimnames = list(NULL, names(data)))
  }

  for (k in seq(nvars)) {

    nc_names <- attributes(nc$var)$names
    stopifnot(var_names[k] %in% nc_names)
    var_nc_index <- grep(var_names[k], nc_names)[1]

    nc_var <- ncdf4::ncvar_get(nc, attributes(nc$var)$names[var_nc_index])
    nc_var_dims <- dim(nc_var) # lon, lat, then vars time or vertical of gridded

    if (verbose) {
      message(paste("The dimensionality of variable", var_names[k], "is",
                    paste(nc_var_dims, collapse = ",")))
    }

    # Set up chunksizes  ---------------------------------------------------
    if (has_Z_verticalAxis) {
      if (has_T_timeAxis) {
        z_chunksize <- dim(data)[3]
      }
    }

    # ---------------------------------------------------------------------
    # NOT GRIDDED ---------------------------------------------------------
    # ---------------------------------------------------------------------
    if (!isGridded) {

      var_chunksizes <- nc_var_dims[1] # site

      #  -----------------------------------------------------------------------
      # add variable values ! --------------------------------------------------
      # ------------------------------------------------------------------------
      if (nvars > 1) {

        vals <- data[, k]

        try(ncdf4::ncvar_put(nc, varid = var_names[k],
                             vals = vals,
                             start = 1,
                             count = var_chunksizes))

      } else {
        if (has_T_timeAxis && has_Z_verticalAxis) {

          for (z in seq(z_chunksize)) { # by Z axis
            if (verbose) message("Adding vertical layer ", z)
            for (t in seq_len(nn)) { # col by col - always time in this case

              vals <- data[, t, z]

              try(ncdf4::ncvar_put(nc, varid = var_names[k],
                                   vals = vals,
                                   start = c(1, z, t), #x-y-z-t
                                   count = c(var_chunksizes, 1, 1)))
            }
          }
        } else {
          # write values, col by col - n values can rep dif. vals, time, or vert
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
    if (isGridded) {

      # more checks based on dim of var ----------------------------------------

      if (isTRUE(xor(has_T_timeAxis, has_Z_verticalAxis)))
        stopifnot(nc_var_dims[3] == nn)

      var_chunksizes <- c(nc_var_dims[1], nc_var_dims[2]) # lon, lat

      #  -----------------------------------------------------------------------
      # add variable values ! --------------------------------------------------
      # ------------------------------------------------------------------------
      if (nvars > 1) {
        temp <- grid_template
        temp[val_grid_ids] <- data[, k]
        vals <- matrix(temp,  ncol = var_chunksizes[2])

        var_start <-  c(1, 1)

        try(ncdf4::ncvar_put(nc, varid = var_names[k],
                             vals = vals,
                             start = var_start,
                             count = var_chunksizes))

      } else {
        if (has_T_timeAxis && has_Z_verticalAxis) {

          temp <- grid_template

          for (z in seq(z_chunksize)) { # by Z axis
           if (verbose) message("Adding vertical layer ", z)
            for (t in seq_len(nn)) { # col by col - always time in this case

              temp[val_grid_ids] <- data[, t, z]
              vals <- matrix(temp, ncol = as.numeric(var_chunksizes[2]))

              try(ncdf4::ncvar_put(nc, varid = var_names[k],
                                   vals = vals,
                                   start = c(1, 1, z, t), #x-y-z-t
                                   count = c(var_chunksizes, 1, 1)))
            }
          }
        } else {
          # write values, col by col -- n values can rep dif. time, or verticals
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

  }

  invisible(TRUE)
}

#' Enhance \code{\link[raster]{raster}} to handle added information written
#' to a \var{netCDF} file by function
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
    nc_crs <- RNetCDF::att.get.nc(nc, variable = "crs", attribute = "crs_wkt")
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

#' Read data from a netCDF into an array or matrix
#'
#' @param x a netCDF file
#' @return an array
#' @export
#'
read_netCDF_to_array <- function(x, locations) {

  # open file, writeable --------------
  nc <- ncdf4::nc_open(x, write = TRUE)
  on.exit(ncdf4::nc_close(nc))

  # figure out dimensionality
  nc_dims <-  attributes(nc$dim)$names #dims of netCDF

  if ("site" %in% nc_dims) isGridded <- FALSE
  if ("lat" %in% nc_dims) isGridded <- TRUE

  has_T_timeAxis <-  "time" %in% nc_dims
  has_Z_verticalAxis <-  "vertical" %in% nc_dims

  # Define variables
  nc_var_names <- attributes(nc$var)$names
  nc_var_names <- grep("bnds|crs|lat|lon", nc_var_names, invert = TRUE,
                       value = TRUE)

  if (isGridded) {

    crs <- ncdf4::ncatt_get(nc, varid = "crs")[["crs_wkt"]]
    loc <- rSW2st::as_points(locations, to_class = "sp", crs = crs)

    # Create grid from location values ----------------------------------------
    sp::gridded(loc) <- TRUE
    grid_template <- raster::raster(loc)
    raster::extent(grid_template) <- raster::extent(loc)

    val_ids <- raster::cellFromXY(grid_template, loc)

    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)

    locDims <- nc_var_dims[1] * nc_var_dims[2]
    dimtz <- nc_var_dims[3]
    dimt <- if (has_T_timeAxis && has_Z_verticalAxis) nc_var_dims[4]

  } else {

    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)

    val_ids <- seq_len(nc_var_dims[1])

    locDims <- nc_var_dims[1]
    dimtz <- nc_var_dims[2]
    dimt <- if (has_T_timeAxis && has_Z_verticalAxis) nc_var_dims[3]

  }

  # ---------------------------------------------------------------------------
  # Put variables into array
  # ---------------------------------------------------------------------------

  # multi variable
  if (length(nc_var_names) > 1) {

    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)
    newArray <- array(, dim = c(locDims,
                                 length(nc_var_names)))

    for (k in seq_along(nc_var_names)) {
      nc_var <- ncdf4::ncvar_get(nc, nc_var_names[k])
      v <-  c(nc_var)
      newArray[, k] <- v[val_ids]
    }

  } else {
    # values
    nc_var <- ncdf4::ncvar_get(nc, nc_var_names[1])
    nc_var_dims <- dim(nc_var)

    if (has_T_timeAxis && has_Z_verticalAxis) {
      # if there is a time AND vertical axis

      newArray <- array(, dim = c(locDims,
                                  dimt, dimtz))

      for (t in seq(dimt)) {

        for (i in seq(dimtz)) {

          if (isGridded) v <- c(nc_var[, , i, t]) else v <- c(nc_var[, i, t])
          newArray[, t, i] <- v[val_ids]

        }
      }

    } else {
      # if there is a time OR vertical axis
      newArray <- array(, dim = c(locDims, dimtz))

      for (i in seq(dimtz)) {

        if (isGridded) v <- c(nc_var[, , i]) else v <- c(nc_var[, i])
        newArray[, i] <- v[val_ids]

      }
    }
  }

  newArray
}

#' Calculate "nominal resolution" of grid
#'
#' @param grid A raster object
#' @param sites A raster object. A numeric array, matrix, or data.frame where
#'  each row is a site and the columns contain values for long and lat.
#' @param cell_areas_km2 A string of numeric values. Equal in length to the
#' length of sites. Area each cell represents in km2.
#'
#' @references CMIP6 Global Attributes, DRS, Filenames, Directory Structure,
#'   and CV’s 10 September 2018 (v6.2.7)
#'   Appendix 2: Algorithms for Defining the "nominal_resolution" Attribute
#nolint start
#'   \url{https://docs.google.com/document/d/1h0r8RZr_f3-8egBMMh7aqLwy3snpD6_MrDz1q8n5XUk/edit#bookmark=id.ibeh7ad2gpdi}
#nolint end
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
