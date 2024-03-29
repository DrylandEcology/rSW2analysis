
#' Calculate cell-wise ensembles across \code{reqCSs}
#'
#' @param data An \code{array} with four dimensions -- as generated by
#'   \code{\link{extract_dbOut_to_array}}.
#' @param subset A logical vector used to subset gridcells from \code{data}
#'   for calculation of ensemble.
#' @param funs A vector of character strings or of functions. The functions of
#'   the form \code{f(x, ...)} which will be used to calculate the ensemble
#'   values across \var{GCMs}.
#' @param ... Optional arguments passed to every \code{funs} function, e.g.,
#'   \code{na.rm = TRUE}.
#' @param variables A vector of character strings. A subset of names of the
#'   second dimension of \code{data} or \code{NULL} which indicates to use
#'   the full set of the second dimension of \code{data}.
#' @param dim_ens An integer value. The dimension of \code{data} over which
#'   the ensembles are calculated. This has to be \code{3}.
#' @param req_scens A vector of character strings. A subset of
#'   (climate) scenarios from \code{SFSW2_prj_meta[["sim_scens"]][["reqCSs"]]}
#'   for which ensembles are calculated across participating models.
#'
#' @export
calc_cellwise_ensemble <- function(
  data,
  subset = NULL,
  funs = c("mean", "min", "max"),
  ...,
  variables = NULL,
  sc_historical = NULL,
  add_historical = FALSE,
  dim_ens = 3L,
  req_scens = NULL,
  req_dtimes = NULL,
  req_downs = NULL,
  verbose = FALSE
) {

  # Prepare functions
  funnames <- funs
  funs <- lapply(funs, match.fun) # find suitable function/method names
  nfuns <- length(funs)

  # Prepare data
  dim_ens <- as.integer(dim_ens)
  stopifnot(dim_ens == 3L)

  dn_data <- dimnames(data)
  dim_data <- dim(data)
  dims_fix <- c(1, 2, dim_ens) # 1, sites; 2, variables
  dims_margin <- seq_along(dim_data)[-dim_ens]

  if (is.null(subset)) {
    subset <- seq_len(dim_data[1])
  } else {
    if (is.logical(subset)) {
      subset <- which(subset)
    }
  }


  # Locate scenarios x downscaling types x time periods
  has_sim_scens_id <- dn_data[[dim_ens]]

  if (is.null(req_scens)) {
    req_scens <- unique(find_reqCSs(has_sim_scens_id))
  }

  if (is.null(req_downs)) {
    req_downs <- unique(find_reqDS(has_sim_scens_id))
  }

  if (is.null(req_dtimes)) {
    req_dtimes <- unique(find_reqDeltaYR(has_sim_scens_id))
  }

  # Put together ensemble grid
  xt <- expand.grid(
    dn = req_downs,
    dt = req_dtimes,
    sc = req_scens,
    stringsAsFactors = FALSE,
    KEEP.OUT.ATTRS = FALSE
  )
  xt_labels <- apply(xt, 1, paste, collapse = ".")
  xtN <- length(xt_labels)

  # Handle historical condition
  add_historical <-
    add_historical &&
    !is.null(sc_historical) &&
    (sc_historical %in% has_sim_scens_id)

  if (add_historical) {
    xt_labels2 <- c(sc_historical, xt_labels)
  } else {
    xt_labels2 <- xt_labels
  }
  xtN2 <- length(xt_labels2)

  # Locate variables
  if (is.null(variables)) {
    variables <- dn_data[[2]]
    ivariables <- seq_along(variables)
  } else {
    ivariables <- match(variables, dn_data[[2]])
    stopifnot(!anyNA(ivariables))
  }

  # Prepare ensemble data object
  data_ens <- array(
    data = NA,
    dim = c(nfuns, dim_data[1], length(variables), xtN2, dim_data[-dims_fix]),
    dimnames = c(
      list(funnames),
      list(NULL),
      list(variables),
      list(xt_labels2),
      dimnames(data)[-dims_fix]
    )
  )

  # Copy historical values
  if (add_historical) {
    ndim <- length(dim_data)

    for (k in seq_len(nfuns)) {
      if (ndim == 4) {
        data_ens[k, subset, , sc_historical, ] <- data[subset, , sc_historical, ] # nolint
      } else if (ndim == 3) {
        data_ens[k, subset, , sc_historical] <- data[subset, , sc_historical]
      } else {
        stop("Data object with ", ndim, " not implemented.")
      }
    }
  }

  # Calculate ensemble values
  k_offset <- if (add_historical) 1 else 0

  for (k in seq_len(xtN)) {
    if (verbose) {
      msg <- paste(Sys.time(), "'calc_cellwise_ensemble':", xt_labels[k])
    }

    iuse <- grep(xt_labels[k], has_sim_scens_id)

    if (length(iuse) > 0) {
      if (verbose) {
        print(paste(msg, "with", length(iuse), "members."))
      }

      # Prepare to slice data/data_ens arrays by indexing matrices
      # so that we can handle an arbitrary number of dimensions
      ils_data <- c(
        list(subset),
        list(ivariables),
        list(iuse),
        lapply(dim_data[-dims_fix], seq_len)
      )
      mid_data <- as.matrix(
        expand.grid(
          ils_data,
          stringsAsFactors = FALSE,
          KEEP.OUT.ATTRS = FALSE
        )
      )

      ils_ens <- c(
        list(seq_len(nfuns)),
        list(subset),
        list(ivariables),
        list(as.integer(k_offset + k)),
        lapply(dim_data[-dims_fix], seq_len)
      )

      mid_ens <- as.matrix(
        expand.grid(
          ils_ens,
          stringsAsFactors = FALSE,
          KEEP.OUT.ATTRS = FALSE
        )
      )

      # Apply functions
      data_ens[mid_ens] <- apply(
        array(data[mid_data], dim = lengths(ils_data)),
        MARGIN = dims_margin,
        function(x, ...) sapply(funs, do.call, args = list(x = x, ...))
      )

    } else {
      if (verbose) {
        print(paste(msg, "no members found."))
      }
    }
  }

  data_ens
}



#' Assign to cells the \var{inverse-ecdf} quantiles from extent-wide
#' (area-weighted) ranked, (equally-weighted) GCMs ensembles across
#' \code{reqCSs}
#'
#' This ensemble approach, unlike cell-wise ensembles, accounts for physical
#' interdependence among grid-points within a GCM projection
#' (Madsen et al. 2017).
#'
#' @param data A data.frame with three dimensions -- as generated by
#'   \code{\link{extract_dbOut_to_array}}.
#' @param subset A logical vector used to subset gridcells from \code{data}
#'   for calculation of ensemble.
#' @param area A numeric vector. Its length corresponds to the first dimension
#'   of \code{data} and represents the cell areas (e.g., that may vary by
#'   latitude). If not \code{NULL} or not all equal, then a \code{fcentral}
#'   value "mean" uses \code{\link[stats]{weighted.mean}} instead of
#'   \code{\link[base]{mean}} and a value of "median" uses
#'   \code{\link[Hmisc]{wtd.quantile}} instead of \code{\link[stats]{median}}
#'   where \code{area} are used as non-random "reliability" weights.
#' @param fcentral A character string naming the function to calculate a central
#'   tendency across the spatial extent for each \var{GCMs};
#'   one of "mean" or "median". The function is used to calculate the
#'   region-wide (weighted) statistic on which the GCMs are ranked. See
#'   \code{area}.
#' @param funs A vector of character strings or of functions. The functions of
#'   the form \code{f(x, ...)} which will be used to calculate the ensemble
#'   values across \var{GCMs}. Note: \var{"min"}, \var{"median"}, \var{"max"}
#'   will be replaced by their quantile equivalents,
#'   i.e., \code{probs = 0, 0.5, or 1}  respectively.
#' @param probs A numeric vector of probabilities with values in \code{[0,1]}
#'   to determine which quantiles from among the \code{fcentral} values
#'   across \var{GCMs} are returned for each cell.
#' @param ... Optional arguments passed to \code{\link[stats]{quantile}}, e.g.,
#'   \code{na.rm = TRUE}.
#' @param ties.method A character string. Specifies how ties are treated. See
#'   \code{link[base]{rank}}. The method must produce unique ranks if
#'   \code{funs} or \code{probs} are not empty.
#' @param variables A vector of character strings. A subset of names of the
#'   second dimension of \code{data} or \code{NULL} which indicates to use the
#'   full set of the second dimension of \code{data}.
#' @param reqCSs A vector of character strings. A subset of (climate) scenarios
#'   from \code{SFSW2_prj_meta[["sim_scens"]][["reqCSs"]]} for which ensembles
#'   are calculated.
#' @param reqMs A vector of character strings. A subset of (climate) models
#'   from \code{SFSW2_prj_meta[["sim_scens"]][["reqMs"]]} across which ensembles
#'   are calculated.
#' @param verbose A logical value.
#'
#' @return A 4-dimensional, numeric array where the first dimension represents
#'   \code{probs}, the second the cells/sites (first dimension of \code{data}),
#'   the third the \code{variables}, and the fourth the scenarios (ambient plus
#'   \code{reqCSs}).
#'
#' @references Madsen, M. S., P. L. Langen, F. Boberg, and J. H. Christensen.
#'   2017. Inflated Uncertainty in Multimodel-Based Regional Climate
#'   Projections. Geophysical Research Letters 44:11606-11613.
#'
#' @export
calc_extentwise_ensemble <- function(SFSW2_prj_meta, data, subset = NULL,
  area = NULL, fcentral = c("median", "mean"), funs = c("min", "median", "max"),
  probs = c(0, 0.5, 1), ..., ties.method = "first",
  variables = NULL, add_historic = TRUE, reqCSs = NULL, reqMs = NULL,
  verbose = FALSE) {

  dim_data <- dim(data)
  dnames_data <- dimnames(data)
  if (is.null(area)) {
    area <- rep(1, SFSW2_prj_meta[["sim_size"]][["runsN_sites"]])
  }
  if (is.null(subset)) {
    subset <- rep(TRUE, dim_data[1])
  }


  # Check that data is well formed
  stopifnot(
    identical(length(dim_data), 3L),
    identical(length(area), SFSW2_prj_meta[["sim_size"]][["runsN_sites"]]),
    identical(dim_data[1], length(subset)),
    identical(dim_data[1L], SFSW2_prj_meta[["sim_size"]][["runsN_sites"]]))

  # Determine central tendency function
  fcentral <- fun_central(method = fcentral, area = area[subset], na.rm = TRUE)

  # Determine aggregation functions: quantiles and other functions
  names_aggs <- funs

  # convert equivalent funs to probs: 0% == min, 50% == median, 100% == max
  equivalent_p_f <- data.frame(
    probs = c(0, 0.5, 1),
    funs = c("min", "median", "max"),
    stringsAsFactors = FALSE)

  for (k in seq_len(nrow(equivalent_p_f))) {
    has_q <-
      abs(probs - equivalent_p_f[k, "probs"]) <=
      sqrt(.Machine$double.eps)

    has_f <- funs %in% equivalent_p_f[k, "funs"]

    if (any(has_f)) {
      funs <- funs[!has_f]
      if (!any(has_q)) {
        probs <- c(probs, equivalent_p_f[k, "probs"])
      }
    }
  }

  probs <- sort(unique(probs))
  nprobs <- length(probs)
  iout_probs <- match(equivalent_p_f[, "funs"], names_aggs, nomatch = 0)
  nfuns <- length(funs)
  naggs <- nprobs + nfuns

  # If funs include `span` and/or `agreement`, then we need 0, 0.5, and 1
  i_span <- match("span", names_aggs, nomatch = 0)
  has_span <- i_span > 0
  i_agreement <- match("agreement", names_aggs, nomatch = 0)
  has_agreement <- i_agreement > 0
  probs_temp <- if (has_span || has_agreement) c(0, 0.5, 1)
  nprobs_temp <- length(probs_temp)

  # Find "other" funs
  i_ofuns <- seq_along(names_aggs)[-c(iout_probs, i_span, i_agreement)]
  n_ofuns <- length(i_ofuns)


  # Check how to resolve ties in ranks
  if ((nprobs > 0 || nprobs_temp > 0) &&
      !(ties.method %in% c("first", "last"))) {
    warning("Unique ranks required: `ties.method` set to 'first'.")
    ties.method <- "first"
  }

  # Locate GCMs and scenarios
  sc_current <- SFSW2_prj_meta[["sim_scens"]][["ambient"]]
  has_sim_scens_id <- dnames_data[[3L]]
  has_current <- sc_current %in% has_sim_scens_id

  if (is.null(reqCSs)) {
    reqCSs <- unique(find_reqCSs(has_sim_scens_id, SFSW2_prj_meta))
  }

  reqCS2s <- if (has_current) c(sc_current, reqCSs) else reqCSs

  if (is.null(reqMs)) {
    reqMs <- unique(find_reqMs(has_sim_scens_id, SFSW2_prj_meta))
  }

  # Locate variables
  vars_data <- dnames_data[[2L]]
  if (is.null(variables)) {
    variables <- vars_data
  } else {
    stopifnot(variables %in% vars_data)
  }

  # Prepare ensemble data object
  data_ens <- array(NA,
    dim = c(naggs, SFSW2_prj_meta[["sim_size"]][["runsN_sites"]],
      length(variables), length(reqCS2s)),
    dimnames = list(names_aggs, NULL, variables, reqCS2s))

  data_ens2 <- array(NA,
    dim = c(2, length(variables), length(reqMs), length(reqCSs)),
    dimnames = list(c("fcentral", "Rank"), variables, reqMs, reqCSs))


  # Copy current values
  if (has_current && add_historic) for (k in seq_len(naggs)) {
    data_ens[k, subset, , sc_current] <- data[subset, , sc_current]
  }

  # Calculate ensemble values
  for (sc in seq_along(reqCSs)) {
    if (verbose) {
      print(paste(Sys.time(), "'calc_extentwise_ensemble':", reqCSs[sc]))
    }

    # Identify available GCMs in ensemble
    isc <- grep(reqCSs[sc], has_sim_scens_id)
    igcms <- sapply(reqMs, function(m) any(grepl(m, has_sim_scens_id[isc])))
    k <- length(isc)

    # Ensembles
    if (k > 0) {
      temp_data <- data[subset, , isc, drop = FALSE]

      # Calculate region-wide `fcentral` (area-weighted) values for each
      # variable and GCM
      temp_data_fin <- temp_data

      # Deal with infinite values that arose from relative deltas
      # (division by zero): ignore for the calculation of overall central value;
      # propagate otherwise
      ids_inf <- is.infinite(temp_data)
      if (any(ids_inf)) {
        temp_data_fin[ids_inf] <- NA
      }

      data_ens2["fcentral", , igcms, reqCSs[sc]] <-
        apply(temp_data_fin, 2:3, fcentral)

      # Rank available GMCs according to region-wide `fcentral`
      data_ens2["Rank", , igcms, reqCSs[sc]] <-
        if (k > 1) {
          t(apply(data_ens2["fcentral", , igcms, reqCSs[sc]], 1, rank,
            ties.method = ties.method))
        } else {
          rank(data_ens2["fcentral", , igcms, reqCSs[sc]],
            ties.method = ties.method)
        }


      # Identify (equally-weighted) GCMs by matching probs to the quantiles of
      # the 'Inverse of empirical distribution function' type
      if (nprobs > 0) {
        id_probs <- stats::quantile(seq_len(k), probs = probs, type = 1L,
          names = FALSE)
      }
      if (nprobs_temp > 0) {
        id_probs_temp <- stats::quantile(seq_len(k), probs = probs_temp,
          type = 1L, names = FALSE)
      }

      # Apply functions that use ranked values
      for (iv in seq_along(variables)) {
        if (nprobs > 0) {
          # Match probs to available ranks
          id_ranks <- match(id_probs,
            data_ens2["Rank", variables[iv], igcms, reqCSs[sc]])

          # Extract ensemble quantiles
          data_ens[iout_probs, subset, variables[iv], 1L + sc] <-
            t(temp_data[, variables[iv], id_ranks])
        }

        if (nprobs_temp > 0) {
          # Extract ensemble quantiles for `span` and/or `agreement`
          temp <- temp_data[, variables[iv], match(id_probs_temp,
            data_ens2["Rank", variables[iv], igcms, reqCSs[sc]])]

          # Calculate funs
          if (has_span) {
            data_ens[i_span, subset, variables[iv], 1L + sc] <-
              apply(temp, 1, span, ...)
          }

          if (has_agreement) {
            # count agreement with sign of cell values for median GCM
            temp2 <- cbind(median = temp[, 2], temp_data[, variables[iv], ])
            data_ens[i_agreement, subset, variables[iv], 1L + sc] <-
              apply(temp2, 1, function(x)
                agreement(x[-1], val = x[1], ...))
          }
        }
      }

      # Apply other functions across GCMs
      if (n_ofuns > 0) {
        data_ens[i_ofuns, subset, , 1L + sc] <-
          apply(temp_data, MARGIN = 1:2, function(x, ...)
            sapply(names_aggs[i_ofuns], do.call, args = list(x = x, ...)))
      }
    }
  }

  list(ensemble_values = data_ens, ensemble_structure = data_ens2)
}


#' Assign to cells the \var{inverse-ecdf} quantiles from region-wide ranked GCMs
#' ensembles across \code{reqCSs}
#'
#' For each \code{region}, the function \code{\link{calc_extentwise_ensemble}}
#' is called on the set of cells belonging to that region.
#'
#' @inheritParams calc_extentwise_ensemble
#' @param region A character or numeric vector of a length equal to the first
#'   dimension of \code{data}, i.e., cells.
#' @seealso \code{\link{calc_extentwise_ensemble}}
#'
#' @export
calc_regionwise_ensemble <- function(SFSW2_prj_meta, data, subset = NULL,
  region, area = NULL, fcentral = c("median", "mean"),
  funs = c("min", "median", "max"), probs = c(0, 0.5, 1), ...,
  ties.method = "first", variables = NULL, add_historic = TRUE, reqCSs = NULL,
  verbose = FALSE) {

  stopifnot(
    length(region) == dim(data)[1L],
    is.null(area) || length(area) == dim(data)[1L],
    is.null(subset) || length(subset) == dim(data)[1L])
  region_set <- stats::na.exclude(unique(region))
  res <- NULL

  # Determine ensembles for each region seperately and combine cells back
  # together
  for (k in seq_along(region_set)) {
    if (verbose) {
      print(paste(Sys.time(), "'calc_regionwise_ensemble':", region_set[k]))
    }

    # Set all values not in region to NA
    id_region <- region %in% region_set[k]

    temp_data <- data
    temp_data[!id_region, , ] <- NA

    # Calculate region-wise ensemble
    x <- calc_extentwise_ensemble(SFSW2_prj_meta = SFSW2_prj_meta,
      data = temp_data, subset = subset, area = area,
      fcentral = fcentral, funs = funs, probs = probs, ...,
      ties.method = ties.method, variables = variables,
      add_historic = add_historic, reqCSs = reqCSs,
      verbose = verbose)[["ensemble_values"]]

    if (k == 1) {
      res <- array(NA, dim = dim(x), dimnames = dimnames(x))
    }

    # Store values for region
    res[, id_region, , ] <- x[, id_region, , ]
  }

  res
}



#' Ensemble from ranked GCM at the scale of interest (Madsen et al. 2017)
#' @references
#'   Madsen, M. S., P. L. Langen, F. Boberg, and J. H. Christensen. 2017.
#'   Inflated Uncertainty in Multimodel-Based Regional Climate Projections.
#'   Geophysical Research Letters 44:11606–11613.
#'
calculate_ensembles <- function(
  meta,
  data,
  data_names = names(data),
  subset,
  cell_area_km2,
  id_region,
  sc_historical, req_Downs, req_dTime,
  ens_wises = c("EnsCW", "EnsRW", "EnsGW"),
  ens_funs = c("min", "mean", "median", "max", "span", "agreement", "majority"),
  path,
  ftag
) {

  ens_wises <- match.arg(ens_wises, several.ok = TRUE)
  ens_funs <- match.arg(ens_funs, several.ok = TRUE)
  dir.create(path, recursive = TRUE, showWarnings = FALSE)

  if (missing(subset)) {
    subset <- rep(TRUE, dim(data[[1]])[1])
  }

  # Create unique file id number for memoization
  temp <- list(
    subset = as.vector(subset),
    cell_area_km2 = as.numeric(cell_area_km2),
    id_region = as.vector(id_region),
    sc_historical = as.character(sc_historical),
    req_Downs = as.character(req_Downs),
    req_dTime = as.character(req_dTime),
    ens_wises = as.character(ens_wises),
    ens_funs = as.character(ens_funs)
  )
  cid <- digest::digest(temp, algo = "sha1")

  # Output container
  dats_Ens <- vector(mode = "list", length = length(ens_wises))
  names(dats_Ens) <- ens_wises

  for (ew in ens_wises) {
    dats_Ens[[ew]] <- vector("list", length = length(data_names))
    names(dats_Ens[[ew]]) <- data_names

    for (k in seq_along(data_names)) {
      print(paste(Sys.time(), dQuote(ew), "ensembles for", data_names[k]))

      temp <- list(
        data = data[[data_names[k]]],
        data_names = as.character(data_names[k])
      )
      fid <- digest::digest(temp, algo = "sha1")

      fname_EnsW <- file.path(path, paste0(ftag, "_", cid, "_", fid, ".rds"))

      if (file.exists(fname_EnsW)) {
        dats_Ens[[ew]][[data_names[k]]] <- readRDS(fname_EnsW)

      } else {
        temp <-
          if (ew == "EnsCW") {
            # (i) cell-wise ensembles: for each cell, foo across GCMs are
            # extracted from the GCM with the foo-rank based on values for each
            # cell independently
            calc_cellwise_ensemble(
              data = data[[data_names[k]]],
              subset = subset,
              add_historical = TRUE,
              sc_historical = sc_historical,
              funs = ens_funs,
              na.rm = TRUE,
              verbose = TRUE
            )

          } else if (ew == "EnsRW") {
            # (ii) region-wise ensembles: for each cell, foo across GCMs are
            # extracted from the GCM with the foo-rank based on their regional
            # mean values
            calc_regionwise_ensemble(
              SFSW2_prj_meta = meta,
              data = data[[data_names[k]]],
              subset = subset,
              add_historic = TRUE,
              region = id_region,
              area = cell_area_km2,
              fcentral = "median",
              funs = ens_funs,
              na.rm = TRUE,
              verbose = TRUE
            )

          } else if (ew == "EnsGW") {
            # (iii) global ensembles: for each cell, foo across GCMs are
            # extracted from the GCM with the foo-rank based on their global
            # mean values
            calc_extentwise_ensemble(
              SFSW2_prj_meta = meta,
              data = data[[data_names[k]]],
              subset = subset,
              add_historic = TRUE,
              area = cell_area_km2,
              fcentral = "median",
              funs = ens_funs,
              na.rm = TRUE,
              verbose = TRUE
            )[["ensemble_values"]]
          }

        # Cast to same structure as `data`:
        # (i) fold 1st into 4th dimension
        dats_Ens[[ew]][[data_names[k]]] <- reshape2::acast(
          reshape2::melt(temp),
          Var2 ~ Var3 ~ Var4 + Var1
        )
        # (ii) fix naming scheme of scenario/3rd dimension
        sctemp <- dimnames(dats_Ens[[ew]][[data_names[k]]])[[3]]
        sctemp2 <- strsplit(sctemp, split = "_")
        itemp <- grep(sc_historical, sctemp)
        for (k2 in itemp) {
          sctemp[k2] <- paste(
            req_Downs,
            req_dTime[1],
            sctemp2[[k2]][1],
            sctemp2[[k2]][2],
            sep = "."
          )
        }
        for (k2 in seq_along(sctemp)[-itemp]) {
          sctemp[k2] <- paste(
            sctemp2[[k2]][1],
            sctemp2[[k2]][2],
            sep = "."
          )
        }
        dimnames(dats_Ens[[ew]][[data_names[k]]])[[3]] <- sctemp

        saveRDS(dats_Ens[[ew]][[data_names[k]]], file = fname_EnsW)
      }
    }


    # Make sure that we didn't introduce NAs
    hasNAs <- sapply(
      dats_Ens[[ew]],
      function(x) {
        temp <- grep(sc_historical, dimnames(x)[[3]])
        anyNA(x[subset, , -temp])
      }
    )

    if (any(hasNAs)) {
      stop(
        "We have NAs in: ",
        paste(shQuote(names(hasNAs)[hasNAs]), collapse = ", ")
      )
    }
  }

  dats_Ens
}
