#' \pkg{rSW2analysis}: Tools to extract and analyse output from simulation
#'  experiments that were carried out by \pkg{rSFSW2}
#'
#' @section LICENSE:
#'    Copyright (C) \Sexpr{format(Sys.Date(), "\%Y")} by
#'    \Sexpr{packageDescription("rSFSW2")[["Maintainer"]]}
#'
#'    This program is free software: you can redistribute it and/or modify
#'    it under the terms of the GNU General Public License as published by
#'    the Free Software Foundation, version 3 of the License.
#'
#' @section DISCLAIMER:
#'    This program is distributed in the hope that it will be useful,
#'    but WITHOUT ANY WARRANTY; without even the implied warranty of
#'    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
#'    GNU General Public License for more details.
#'
#' @docType package
#' @name rSW2analysis
"_PACKAGE"

##------ Package level variables
glovars <- new.env()

##------ Import from other packages
## Package uses S3/S4 classes - they are defined in package:methods
## Package uses methods from 'RSQLite' package (which re-exports 'DBI' methods)
#' @import rSW2utils
#' @import methods
#' @import RSQLite
NULL

