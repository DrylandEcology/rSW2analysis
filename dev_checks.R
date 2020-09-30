library(roxygen2)
library(rSW2analysis)
setwd('~/Documents/Git/rSW2analysis/')

roxygen2::roxygenise()

?create_empty_netcdf_File
?populate_netcdf_from_array
?calculate_nominal_resolution

# ---------------------------- package testing
library(rmarkdown)
library(devtools)
library(lintr)

devtools::document()
devtools::load_all()
#devtools::run_examples()

#usethis::use_build_ignore("vignettes/dummynetcdf.nc")
#usethis::use_build_ignore("R/dummynetcdf.nc")
#usethis::use_build_ignore("dev_checks.R")

devtools::check(env_vars = c(NOT_CRAN = "true"))

#spelling::update_wordlist(pkg = ".", vignettes = TRUE, confirm = TRUE)
#lintr::lint('R/functions_netCDF.R')
#devtools::check()

# not for vignette to work need to actually iinstall new functionaliity

