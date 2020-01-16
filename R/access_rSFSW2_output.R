

find_scen_part <- function(n_elem_offset, sim_scens, meta_part = NULL) {
  temp <- strsplit(sim_scens, split = ".", fixed = TRUE)
  # old rSOILWAT2 wrapper had 3 nelems; new version (rSFSW2) with multiple
  # future time periods has 4 nelems:
  nelems <- lengths(temp)
  nmax <- max(nelems)
  temp <- temp[nelems == nmax]
  id_elem <- nmax - n_elem_offset
  temp <- stats::na.exclude(sapply(temp, function(x) x[id_elem]))

  if (is.null(meta_part)) {
    temp
  } else {
    meta_part[meta_part %in% temp]
  }
}

find_reqDS <- function(sim_scens, meta = NULL) {
  find_scen_part(n_elem_offset = 3L, sim_scens,
    meta_part = if (!is.null(meta)) meta[["sim_scens"]][["method_DS"]])
}

find_reqDeltaYR <- function(sim_scens, meta = NULL) {
  find_scen_part(n_elem_offset = 2L, sim_scens,
    meta_part = if (!is.null(meta))
      unique(meta[["sim_scens"]][["DeltaStr_yrs"]]))
}


find_reqCSs <- function(sim_scens, meta = NULL) {
  find_scen_part(n_elem_offset = 1L, sim_scens,
    meta_part = if (!is.null(meta)) meta[["sim_scens"]][["reqCSs"]])
}

find_reqMs <- function(sim_scens, meta = NULL) {
  find_scen_part(n_elem_offset = 0L, sim_scens,
    meta_part = if (!is.null(meta)) meta[["sim_scens"]][["reqMs"]])
}


cur_to_hist <- function(x) {
  x <- gsub("current", "historical", x, ignore.case = TRUE)
  gsub("cur", "hist", x, ignore.case = TRUE)
}



#' Extract data from \var{dbOutput}
#'
#' This function is internally "memoised" per variable, i.e., it loads an
#' \code{array} from \code{file} if previously extracted from
#' \var{dbOutput}; otherwise, it extracts first data (and then saves to a file
#' disk).
#'
#' @param file A character string. The file path to the \var{rds}-file(s)
#'   containing the memoised data. If \code{NULL}, then data is not written/read
#'   from file, but extracted from \var{dbOutput}.
#'
#' @seealso \code{\link{get.SeveralOverallVariables_Scenario}},
#'   \code{\link{saveRDS}}
#'
#' @return A four-dimensional numerical \code{array} with dimensions
#'   \code{runsN_sites}, \code{variables}, \code{scenarios}, and
#'   \code{experiments}.
#'
#' @export
extract_dbOut_to_array <- function(meta, fname_dbOut = NULL,
  file = NULL, MeanOrSD = "Mean", runsN_sites = NULL, variables = NULL,
  scenarios = NULL, experiments = NULL, verbose = FALSE) {

  # Prepare structure
  if (is.null(fname_dbOut)) {
    if (!is.null(meta)) {
      fname_dbOut <- meta[["fnames_out"]][["dbOutput"]]
    } else {
      stop("Argument 'fname_dbOut' is missing.")
    }
  }

  if (is.null(runsN_sites)) {
    if (!is.null(meta)) {
      runsN_sites <- meta[["sim_size"]][["runsN_sites"]]
    } else {
      stop("Argument 'runsN_sites' is missing.")
    }
  }

  if (is.null(variables)) {
    if (!is.null(meta)) {
      variables <- unlist(
        meta[["prj_todos"]][["aon_fields"]][["fields"]])
    } else {
      stop("Argument 'variables' is missing.")
    }
  }
  varsN <- length(variables)

  if (is.null(scenarios)) {
    if (!is.null(meta)) {
      scenarios <- meta[["sim_scens"]][["id"]]
    } else {
      stop("Argument 'scenarios' is missing.")
    }
  }
  scensN <- length(scenarios)

  if (is.null(experiments)) {
    whereClause <- NULL
    experiments <- "Default"
  } else {
    whereClause <- paste0("Experimental_Label='", experiments, "'")
  }
  expN <- length(experiments)


  # Prepare memoization per hyper-slab, i.e., per variable, per scenario,
  # per experiment
  if (!is.null(file)) {
    # Identifiable database file name
    temp <- strsplit(fname_dbOut, split = .Platform$file.sep)[[1]]
    n <- length(temp)
    temp <- paste(temp[max(1, n - 2):n], collapse = .Platform$file.sep)

    # Prepare unique call id
    temp <- list(
      fdbrSFSW2 = temp,
      MeanOrSD = MeanOrSD,
      runsN_sites = runsN_sites)
    call_id <- digest::digest(temp, algo = "sha1")

    # Path structure for memoized files
    dir_file <- dirname(file)
    dir.create(dir_file, recursive = TRUE, showWarnings = FALSE)
    base_file <- basename(file)
    ext_file <- ".rds"

    if (isTRUE(grepl(paste0(ext_file, "$"), base_file))) {
      temp <- substr(base_file, 1, nchar(base_file) - nchar(ext_file))

    } else {
      temp <- NULL
      dir_file <- file
    }

    tag_file <- paste("dbOut", temp, call_id, sep = "_")

    # Function to calculate memoized file for a specific hyper-slab
    get_ftemp <- function(dir = dir_file, tag = tag_file, ext = ext_file,
      var, scen, exp) {
      temp <- list(
        variable = as.character(var),
        scenario = as.character(scen),
        experiment = as.character(exp))
      id <- digest::digest(temp, algo = "sha1")

      file.path(dir, paste0(tag, "-", id, ext))
    }

  } else {
    ftemp <- NA_character_
  }


  # Prepare output container
  data <- array(NA, dim = c(runsN_sites, varsN, scensN, expN),
    dimnames = list(NULL, variables, scenarios, experiments))

  # Loop over dimensions of hyper-slab and extract data
  for (ie in seq_len(expN)) {
    for (isc in seq_len(scensN)) {
      # Load variables that have already been extracted for this unit of
      # "scenario x experiment" and compile list of variables that still
      # need to be extracted from dbOut
      vars_for_isc_x_ie <- NULL

      for (iv in seq_along(variables)) {
        if (!is.null(file)) {
          ftemp <- get_ftemp(var = variables[iv], scen = scenarios[isc],
            exp = experiments[ie])
        }

        if (file.exists(ftemp)) {
          data[, variables[iv], scenarios[isc], experiments[ie]] <-
            readRDS(ftemp)

        } else {
          vars_for_isc_x_ie <- c(vars_for_isc_x_ie, variables[iv])
        }
      }

      # Extract variables from dbOut and store in rds file
      if (length(vars_for_isc_x_ie) > 0) {
        if (verbose) {
          print(paste(Sys.time(), scenarios[isc], experiments[ie]))
        }

        datv <- rSFSW2::dbOut_read_variables_from_scenario(
          fname_dbOut = fname_dbOut,
          variables = vars_for_isc_x_ie,
          MeanOrSD = MeanOrSD,
          scenario = scenarios[isc],
          whereClause = if (!is.null(whereClause)) whereClause[ie]
        )
        datv <- as.matrix(datv) # 2-dim array: sites x variables

        if (identical(dim(datv), c(runsN_sites, length(vars_for_isc_x_ie)))) {
          data[, vars_for_isc_x_ie, scenarios[isc], experiments[ie]] <- datv

          if (!is.null(file)) for (iv in seq_along(vars_for_isc_x_ie)) {
            ftemp <- get_ftemp(
              var = vars_for_isc_x_ie[iv],
              scen = scenarios[isc],
              exp = experiments[ie])

            dir.create(dirname(ftemp), recursive = TRUE, showWarnings = FALSE)
            saveRDS(
              data[, vars_for_isc_x_ie[iv], scenarios[isc], experiments[ie]],
              file = ftemp)
          }

        } else {
          stop("Dimension mismatch:",
            " requested = ", paste(dim(data[, , isc, ie]), collapse = "/"),
            " extracted = ", paste(dim(datv), collapse = "/"),
            " for requested variables: ", paste(vars_for_isc_x_ie,
              collapse = "/"),
            " and extracted variables: ", paste(dimnames(datv)[[2L]],
              collapse = "/"))
        }
      }
    }
  }

  data
}


#' Prepare and load simulation output from \pkg{rSFSW2} into an array either
#' from a \var{sqlite3} database or from \var{netCDF} files.
#'
#' @param experiments A vector of character strings. Values of the
#'   \var{'Experimental_Label'} field. Use if experimental
#'   treatments are organized in one \var{dbOutput} database.
#' @param subprojects A vector of character strings. Same as \code{experiments},
#'   but use if experimental treatments are organized in different
#'   \var{dbOutput} databases. If used, then \code{fname_dbOuts} is a vector
#'   with values for each \code{subprojects}. Note: \code{experiments} and
#'   \code{subprojects} are mutually exclusive.
#'
#' @seealso \code{\link{extract_dbOut_to_array}} for extraction from
#'   \var{dbOut} database, \code{\link{create_netCDF_from_array_with_variables}}
#'   for writing/extracting from \var{netCDF} files
#'
#' @return The same structure as returned from
#'   \code{\link{extract_dbOut_to_array}}, i.e., a four-dimensional numerical
#'   \code{array} with dimensions \code{runsN_sites}, \code{variables},
#'   \code{scenarios}, and \code{experiments}.
#'
#' @export
load_rSFSW2_data_for_analysis <- function(meta, path, path_tmp,
  fname_dbOuts = NULL, variables, MeanOrSD = c("Mean", "SD"), sim_scenarios,
  sc_historical, experiments = NULL, subprojects = NULL, subset = NULL,
  write_to_netcdf = FALSE, ftag_gatt, ftag_gatt2, var_names, var_units,
  timeaggs, verbose = FALSE) {

  # Check inputs
  MeanOrSD <- match.arg(MeanOrSD)

  has_subprojects <- !is.null(subprojects)
  if (!has_subprojects) {
    subprojects <- "Default"
  }

  has_experiments <- !is.null(experiments)
  if (!has_experiments) {
    experiments <- "Default"
  }
  stopifnot(xor(has_subprojects, has_experiments))

  exps_4thdim <- if (has_subprojects) {
      subprojects
    } else if (has_experiments) {
      experiments
    } else {
      "Default"
    }

  if (is.null(subset)) {
    subset <- rep(TRUE, meta[["sim_size"]][["runsN_sites"]])
  }


  # Output container
  res <- array(NA,
        dim = c(meta[["sim_size"]][["runsN_sites"]],
          length(variables), length(sim_scenarios), length(exps_4thdim)),
        dimnames = list(NULL, variables, sim_scenarios, exps_4thdim))


  # Check whether a previous call with write_to_netcdf = TRUE created netCDFs
  fname_datall <- list.files(path = path,
    pattern = "(^All_LyrC_SOILWAT2-)[[:print:]]+(nc$)", full.names = TRUE)


  if (length(fname_datall) == length(sim_scenarios) * length(exps_4thdim)) {
    #--- Read from netCDF files and convert to array

    rdim <- dim(meta[["sim_space"]][["sim_raster"]])
    loc <- sp::coordinates(meta[["sim_space"]][["run_sites"]])
    id_map <- cbind(
      row = raster::colFromX(meta[["sim_space"]][["sim_raster"]], loc[, 1]),
      col = raster::rowFromY(meta[["sim_space"]][["sim_raster"]], loc[, 2])
    )

    for (sc in sim_scenarios) for (exp in exps_4thdim) {
      getM <- cur_to_hist(find_reqMs(sc))
      temp_pattern <- if (sc == sc_historical) {
          getM
        } else {
          paste0(getM, "_", find_reqCSs(sc))
        }
      temp_pattern <- paste0(temp_pattern, "_", exp)

      fname <- fname_datall[grep(temp_pattern, basename(fname_datall))]

      nc <- RNetCDF::open.nc(fname)
      dtemp <- RNetCDF::read.nc(nc)
      RNetCDF::close.nc(nc)

      stopifnot(
        length(dtemp[["lat"]]) == rdim[1],
        length(dtemp[["lon"]]) == rdim[2])

      for (iv in seq_along(variables)) {
        res[, variables[iv], sc, exp] <- dtemp[[var_names[iv]]][id_map]
      }

      # Check that data are correctly ordered
      if ("site_id" %in% variables) {
        stopifnot(identical(
          as.integer(res[, "site_id", sc, exp]),
          meta[["sim_size"]][["runIDs_sites"]]))
      }
    }

  } else {
    #--- Read from SQLite3 database and convert to array

    # Check inputs
    if (is.null(fname_dbOuts)) {
      fname_dbOuts <- meta[["fnames_out"]][["dbOutput"]]
      names(fname_dbOuts) <- subprojects
    }

    if (has_subprojects) {
      # Check that file names of dbOutput and subprojects have correct length
      if (length(fname_dbOuts) == 1 && length(subprojects) > 1) {
        fname_dbOuts <- rep_len(fname_dbOuts, length(subprojects))
      }

      stopifnot(length(fname_dbOuts) == length(subprojects))
    }


    # Extract data from simulation database `dbOutput`
    for (exp in exps_4thdim) {
      temp <- extract_dbOut_to_array(meta,
        fname_dbOut = if (has_subprojects) fname_dbOuts[exp] else fname_dbOuts,
        variables = variables,
        MeanOrSD = MeanOrSD,
        scenarios = sim_scenarios,
        experiments = if (has_subprojects || has_experiments) exp,
        file = file.path(path_tmp, "temp_dbOut", exp),
        verbose = verbose)

      # Subset to requested subset of sites
      icol_siteid <- which("site_id" == dimnames(res)[[2]])
      temp[!subset, -icol_siteid, , ] <- NA

      # Copy to output container
      res[, variables, , exp] <- temp


      # Prepare simulation data for sharing
      if (write_to_netcdf) {
        for (sc in sim_scenarios) {
          ftag_gatt2_temp <- ftag_gatt2
          ftag_gatt2_temp[["experiment_id"]] <- cur_to_hist(find_reqMs(sc))
          if (sc != sc_historical) {
            ftag_gatt2_temp[["parent_source_id"]] <- find_reqCSs(sc)
          }

          time_bounds <- if (sc == sc_historical) {
            c(meta[["sim_time"]][["startyr"]],
              meta[["sim_time"]][["endyr"]])
          } else {
            fut_yrs <- meta[["sim_time"]][["future_yrs"]][find_reqDeltaYR(sc), ]
            c(fut_yrs[["DSfut_startyr"]], fut_yrs[["DSfut_endyr"]])
          }

          fname_nc <- file.path(path,
            paste0("All_", # all variables together
              "LyrC_", # Land-realm; year-climatology
              "SOILWAT2-",
              if (sc == sc_historical) {
                cur_to_hist(sc)
              } else {
                paste0(ftag_gatt2_temp[["experiment_id"]], "_",
                  ftag_gatt2_temp[["parent_source_id"]])
              },
              "_",
              if (has_subprojects || has_experiments) {
                paste0(exp, "_")
              },
              "gn_", # grid-native
              paste(time_bounds, collapse = "-"),
              ".nc"))

          if (!file.exists(fname_nc)) {
            # Write raster to netCDF file
            create_netCDF_from_array_with_variables(
              x = res[, , sc, exp],
              locations = meta[["sim_space"]][["run_sites"]],
              grid = meta[["sim_space"]][["sim_raster"]],

              time_bounds = time_bounds,

              var_attributes = list(
                name = var_names,
                original_name = variables,
                units = var_units,
                cell_methods = paste("time:", timeaggs)
              ),

              global_attributes = c(ftag_gatt, ftag_gatt2_temp,
                list(variant_info = paste("forcing:", exp)),
                cell_measures = "data variables provided at cell centers"
              ),

              file = fname_nc
            )
          }
        }
      }
    }
  }

  res
}

