% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions_netCDF.R
\name{populate_netcdf_from_array}
\alias{populate_netcdf_from_array}
\title{Populate an empty netCDF with values stored in an array}
\usage{
populate_netcdf_from_array(
  file,
  data,
  var_names = NULL,
  has_T_timeAxis,
  has_Z_verticalAxis,
  isGridded = TRUE,
  grid = NULL,
  locations,
  force_v4 = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{file}{A character string. File path to a pre-created, empty netCDF.}

\item{data}{A numeric array.}

\item{has_T_timeAxis}{A logical value. Indicates that the netCDF file
has a third, time dimension.}

\item{has_Z_verticalAxis}{A logical value. Indicates that the netCDF file
has a vertical (e.g. soil profile depths) dimension. If
 \var{has_T_timeAxis} is set to \code{FALSE} then the Z axis will be the
 third dimension. If \var{has_T_timeAxis} is set to \code{TRUE} then the
 Z axis will be the fourth dimension.}

\item{isGridded}{A logical value. Represents whether the location data is on
a regular grid or not.}

\item{grid}{filename (character). File containing the grid information
(i.e. resolution, extent) of the data. Supported file types are the native
raster package format and those that can be read via rgdal.}

\item{locations}{A SpatialPoints object or a matrix or data.frame with two
columns containing long and lat values. Data must be organized by long, lat}

\item{force_v4}{A logical value. Force version 4 of netCDF.}

\item{var_name}{A character string equal to the length of the number of
variables. The length of var_name should either be 1 if there is either a
time and/or vertical dimension \emph{or} equal to the number of columns
in the array if there is no time or vertical dimension.}
}
\value{
This function is used for the side-effect of filling a file.
}
\description{
This function add values to a structured netCDF created with the
\code{create_empty_netCDF_file} function.
The data array provided as an argument to this function should be the same as
that provided to the create_empty_netCDF_file function.
One of \var{locations} or \var{grid} needs to be provided, but not both.
}
\examples{
#############################################################################
# example - create an empty netCDF with a three dimensions (lat, long, time)
#############################################################################

# create dummy data ---------------------------------------------------------

someData <- rnorm(100, 7, 30)
data <- array(someData, c(10, 10))

locations <- data.frame(X_WGS84 = c(rep(-124.5938, 5), rep(-124.5312, 5)),
                        Y_WGS84 = rep(c(47.90625, 47.96875, 48.03125,
                        48.09375, 48.15625), 2))

annual_time_bounds <- c(43737, 44102, 44102, 44467, 44467, 44832, 44832,
                        45197, 45197, 45563, 45563, 45928, 45928, 46293,
                        46293, 46658, 46658, 47024, 47024, 47389)

outFileName <- "dummynetcdf.nc"

# define attributes ---------------------------------------------------------

# time attribute
time_attributes <- list(
   units = "days since 1900-01-01",
   calendar = "standard",
   unlim = "TRUE",
   vals = c(43554, 43920, 44285, 44650, 45015, 45381, 45746, 46111, 46476,
            46842)# mid point of year
   )

# variable attributes
var_attributes <- list(
   name = "JulyTemp",
   long_name = "Annual Mean July Temperature",
   units = "Celsius",
   description = "example data!",
   grid_mapping = "crs: lat lon"
   )

# CRS attributes
crs_attributes <- list(
 crs_wkt = sp::wkt(sp::CRS(SRS_string = "EPSG:4326")),
 grid_mapping_name = "latitude_longitude",
 longitude_of_prime_meridian = 0.0,
 semi_major_axis = 6378137.0,
 inverse_flattening = 298.257223563
 )

# global attributes
global_attributes <- list(
   title = "",
   institution = "Southwest Biological Science Center,
                   U.S. Geological Survey",
   description = "how this data was made",
   source = paste(
     "SOILWAT2 (v4.2.0);",
     "rSOILWAT2 (v2.3.2);",
     "rSFSW2 (v3.1.2)"
   ),
   source_id = "SOILWAT2",
   realm = "land",
   parent_mip_era = "CMIP5",
   parent_experiment_id = "RCP45",
   parent_source = "CanESM2",
   parent_variant_label = "r1i1p1",
   product = "model-output",
   projection = "Geographic",
   grid = "WGS84",
   grid_label = "gn",
   nominal_resolution = "10 km", # calculate_nominal_resolution
   further_info_url = "https://github.com/DrylandEcology/",
   contact = "you@email.com"
   )

# run create_empty_netCDF_file function -------------------------------------
create_empty_netCDF_file(
     data = data,
     has_T_timeAxis = TRUE,
     has_Z_verticalAxis = FALSE,
     time_bounds = annual_time_bounds,
     vert_bounds = NULL,
     var_attributes = var_attributes,
     time_attributes  = time_attributes,
     vertical_attributes = NULL,
     global_attributes = global_attributes,
     crs_attributes = crs_attributes,
     isGridded = TRUE,
     locations = locations,
     file = outFileName,
     force_v4 = TRUE,
     overwrite = TRUE
   )

# run function populate_netcdf_from_array
populate_netcdf_from_array(
     file = outFileName,
     data = data,
     var_names = var_attributes$name,
     has_T_timeAxis = TRUE,
     has_Z_verticalAxis = FALSE,
     isGridded = TRUE,
     locations = locations,
     force_v4 = TRUE
   )

 unlink(outFileName)

}
\seealso{
\code{create_empty_netCDF_file}

\url{http://cfconventions.org/cf-conventions/cf-conventions.html}

Demonstration of netCDF functionality in rSW2analysis vignette
}
