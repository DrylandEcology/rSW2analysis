

#' Calculate absolute or relative change in response variables
#'
#' Change in response variables between a climate reference
#' condition (or scenario family) and each climate condition is calculated as
#' \itemize{
#'   \item \code{x[condition k] - x[reference]}
#'          if \code{method} is \code{"absolute"}
#'   \item \code{(x[condition k] - x[reference]) / x[reference]}
#'          if \code{method} is \code{"relative"}
#'   \item the result of \code{\link{compare_direction_2deltas}}
#'          if \code{method} is \code{"direction"}
#'   \item the result of \code{(x[condition k] > tol, x[condition k] < tol)}
#'          if \code{method} is relative direction, i.e., \code{"rdirection"}
#' }
#'
#' If \code{reference} is the same for
#' each scenario, e.g., "current", then the same value is used for each
#' \code{condition k}; if \code{reference} is a scenario family, e.g., "RCP45",
#' then the appropriate \code{x} values per GCM are used.
#'
#' @param data A four-dimensional numerical \code{array} with dimensions
#'   \code{runsN_sites}, \code{variables}, \code{scenarios},
#'   and \code{experiments}, e.g., the returned object from
#'   \code{\link{extract_dbOut_to_array}}.
#' @param ref_condition A vector of character strings. Each element is used
#'   as reference to calculate a separate set of change values.
#' @param sc_hist A character string. The name of the historical scenario that
#'   is the same for each other (future) projection; or \code{NULL}.
#' @param method A character string. See description.
#' @param ... Additional arguments, e.g., \describe{
#'   \item{tol}{two-sided tolerance in relative units to identify
#'         "no change" for \code{method == "direction"} and
#'         \code{method == "rdirection"}}
#'   \item{subset}{a logical vector of length \code{runsN_sites} used by
#'         \code{method == "rdirection"} to identify "no change"}
#'   }
#'
#' @return A list of four-dimensional numerical \code{array} with dimensions
#'   \code{runsN_sites}, \code{variables}, \code{scenarios},
#'   and \code{experiments}; each element is named as \var{delta_ref_met}
#'   where \var{ref} is one of \code{ref_condition} and \var{met} is the
#'   first three letters of \code{method} and the value of \code{tol} if
#'   provided.
#'
#' @export
calc_change_from_reference <- function(data, ref_condition,
  sc_hist = NULL, method = c("absolute", "relative", "direction", "rdirection"),
  ...) {

  method <- match.arg(method)
  dots <- list(...)
  temp <- dimnames(data)
  scenarios <- temp[[3L]]

  rtol <- sqrt(.Machine$double.eps)
  blind_elem <- "!X!"

  tol <- name_tol <- subset <- NULL
  if (method %in% c("direction", "rdirection")) {
    variables <- temp[[2L]]
    cat_dirs <- if (method == "direction") {
        define_direction_2deltas()
      } else {
        define_direction_1rel()
      }

    if ("tol" %in% names(dots)) {
      has_tol <- isTRUE(is.finite(dots[["tol"]]))
      tol <- if (has_tol) dots[["tol"]]
      name_tol <- paste0("_tol", sub(getOption("OutDec"), "p",
        formatC(if (has_tol) tol else 0, format = "f", flag = "0", digits = 3),
        fixed = TRUE))
    }

    if (method == "rdirection") {
      if ("subset" %in% names(dots)) {
        subset <- dots[["subset"]]
        stopifnot(is.logical(subset), length(subset) == dim(data)[1],
          !anyNA(subset))
      } else {
        subset <- rep(TRUE, dim(data)[1])
      }
    }
  }

  res <- vector("list", length = length(ref_condition))
  names(res) <- paste0("delta_", ref_condition, "_", substr(method, 1, 3),
    name_tol)

  for (k in seq_along(ref_condition)) {
    # Identify reference condition[s]
    sc_ref <- grep(ref_condition[k], scenarios)
    scenarios_ref <- scenarios[sc_ref]

    # Blind scenarios to reference element
    if (!is.null(sc_hist) && isTRUE(scenarios_ref == sc_hist)) {
      blind_scenarios <- rep(blind_elem, length(scenarios))

    } else {
      temp <- strsplit(scenarios_ref, split = ".", fixed = TRUE)
      elem_ref <- unique(sapply(temp, function(x) which(ref_condition[k] == x)))
      temp <- strsplit(scenarios, split = ".", fixed = TRUE)
      blind_scenarios <- sapply(temp, function(x) {
        x[[elem_ref]] <- blind_elem
        paste(x, collapse = ".")
      })
    }

    x <- array(NA, dim = dim(data), dimnames = dimnames(data))

    for (sc in seq_along(scenarios)) {
      # Locate reference for scenarios[sc]
      sc_related_to_ref <- which(blind_scenarios[sc] == blind_scenarios)
      sc_under_refs <- sc_related_to_ref[sc_related_to_ref %in% sc_ref]

      #--- Calculate absolute/relative/directional change
      if (length(sc_under_refs) == 1L) {
        x[, , sc, ] <-
          if (method == "absolute") {
            data[, , sc, ] - data[, , sc_under_refs, ]

          } else if (method == "relative") {
            temp <-
              (data[, , sc, ] - data[, , sc_under_refs, ]) /
              data[, , sc_under_refs, ]

            # Division by zero problems
            ids0 <- abs(data[, , sc_under_refs, ]) < rtol
            if (any(ids0, na.rm = TRUE)) {
              temp[ids0 & data[, , sc, ] > 0] <- Inf
              temp[ids0 & abs(data[, , sc, ]) < rtol] <- 0
              temp[ids0 & data[, , sc, ] < 0] <- -Inf
            }

            temp

          } else if (method == "rdirection") {
            as.matrix(compare_direction_1rel2ref(
              dx = data[, , sc_under_refs, ],
              dy = data[, , sc, ],
              vars = variables,
              cl = cat_dirs,
              tol = tol,
              subset = subset
            ))

          } else if (method == "direction") {
            as.matrix(compare_direction_2deltas(
              dx = data[, , sc_under_refs, ],
              dy = data[, , sc, ],
              vars = variables,
              cl = cat_dirs,
              tol = tol
            ))
          }

      }
    }

    res[[k]] <- x
  }

  res
}



#' @export
define_direction_1rel <- function() {
  data.frame(
    name = c("Increase", "No change", "Decrease"),
    rx_vs_tol = c(">", NA, "<"),
    stringsAsFactors = FALSE
  )
}


#' @export
define_direction_2deltas <- function(expected_dy_lt_dx = TRUE) {
  # Define categories
  cl <- data.frame(
      name = c("No change",
        "Smaller decrease", "Reverse to increase", "Larger increase",
        "Larger decrease", "Reverse to decrease", "Smaller increase"),
      dy_vs_dx = c(NA, ">", ">", ">", "<", "<", "<"),
      dx_vs_0 = c(NA, "<", "<", ">", "<", ">", ">"),
      dy_vs_0 = c(NA, "<", ">", ">", "<", "<", ">"),
      stringsAsFactors = FALSE
    )

  temp <- cl[, "dy_vs_dx"] == "<"
  cl[, "expected"] <- if (expected_dy_lt_dx) temp else !temp

  new_levels <- c("More severe", "Less severe")
  new_labels <- new_levels[1 + as.integer(cl[, "expected"])]
  new_labels[is.na(new_labels)] <- "No change"
  cl[, "name3"] <- new_labels

  cl
}

#' Convert data to factor according to
#' \code{define_direction_2deltas()[, "name"]}
#'
#' @export
factor_6directions <- function(data, cl = NULL) {
  if (is.null(cl)) {
    cl <- define_direction_2deltas()
  }

  if (is.factor(data)) {
    stopifnot(cl[, "name"] == levels(data))
    data <- data
  } else if (is.integer(data)) {
    stopifnot(stats::na.exclude(data) %in% seq_along(cl[, "name"]))
    data <- factor(data, levels = seq_along(cl[, "name"]),
      labels = cl[, "name"])
  } else if (is.character(data)) {
    stopifnot(stats::na.exclude(unique(data)) %in% cl[, "name"])
    data <- factor(data, levels = cl[, "name"])
  } else {
    stop("Data error")
  }

  data
}

#' Remap from six categories of directional modification to two categories
#'
#' @param data Integer, factor, or character vector coded according to
#'   \code{define_direction_2deltas()[, "name"]}
#' @return An object like \code{data} but coded according to
#'   \code{define_direction_2deltas()[, "expected"]}
#'
#' @export
remap_directions_6to2 <- function(data, expected_dy_lt_dx = TRUE, cl = NULL) {
  if (is.na(expected_dy_lt_dx)) {
    return(rep(NA, length(data)))
  }

  if (is.null(cl)) {
    cl <- define_direction_2deltas(expected_dy_lt_dx)
  }

  data <- factor_6directions(data, cl = cl)


  factor(data, levels = cl[, "name"], labels = cl[, "name3"])
}


#' @export
compare_direction_2deltas <- function(dx, dy, vars = NULL, cl = NULL,
  tol = NULL) {

  if (length(tol) != 1 || isFALSE(is.finite(tol))) {
    tol <- sqrt(.Machine$double.eps)
  }
  stopifnot(tol < 1 && tol >= 0)

  # Identify variables to compare
  if (is.null(vars)) {
    vars <- intersect(colnames(dx), colnames(dy))
  }
  stopifnot(vars %in% colnames(dx), vars %in% colnames(dy))

  if (is.null(cl)) {
    cl <- define_direction_2deltas()
  }
  ncls <- seq_len(nrow(cl))

  cl[, "ftol"] <- ifelse(cl[, "dy_vs_dx"] == "<", -1, 1)

  # Prepare output container
  n <- nrow(dx)
  res <- data.frame(array(NA, dim = c(n, length(vars)),
    dimnames = list(NULL, vars)))

  # Identify categories
  for (iv in seq_along(vars)) {
    dx2 <- dx[, vars[iv]]
    dy2 <- dy[, vars[iv]]

    tmp <- rep(NA, n)
    id_nochange <- which(is.na(cl[, "dy_vs_dx"]))
    ids_hasdata <- is.finite(dx2) & is.finite(dy2)

    for (k in ncls[-id_nochange]) {
      tol1 <- 1 + cl[k, "ftol"] * sign(dx2) * tol
      ids <-
        match.fun(cl[k, "dy_vs_dx"])(dy2, tol1 * dx2) &
        match.fun(cl[k, "dx_vs_0"])(dx2, 0) &
        match.fun(cl[k, "dy_vs_0"])(dy2, 0)

      tmp[ids_hasdata & ids] <- k
    }

    tmp[ids_hasdata & is.na(tmp)] <- id_nochange

    res[, vars[iv]] <- factor(tmp, levels = ncls, labels = cl[, "name"])
  }

  res
}

#' Compare directions of change
#' @param dx Reference data
#'
#' @export
compare_direction_1rel2ref <- function(dx, dy, vars = NULL, cl = NULL,
  tol = NULL, subset = NULL) {

  n <- nrow(dx)

  if (length(tol) != 1 || isFALSE(is.finite(tol))) {
    tol <- sqrt(.Machine$double.eps)
  }
  stopifnot(tol < 1 && tol >= 0)

  if (is.null(subset)) {
    subset <- rep(TRUE, n)
  }

  # Identify variables to compare
  if (is.null(vars)) {
    vars <- intersect(colnames(dx), colnames(dy))
  }
  stopifnot(vars %in% colnames(dx), vars %in% colnames(dy))

  if (is.null(cl)) {
    cl <- define_direction_1rel()
  }


  # Prepare output container
  res <- array(NA, dim = c(n, length(vars)), dimnames = list(NULL, vars))

  # Identify categories
  delta <- dy - dx
  rel <- sweep(delta, MARGIN = 1:2, STATS = dx, FUN = "/")

  ids_has_vals <- !is.na(dx) & !is.na(dy)

  tol2 <- sqrt(.Machine$double.eps)
  ids_ref_zero <- abs(dx) < tol2

  # no change: smaller absolute change than tol OR (in case rel is infinite:)
  #            both ref and res are zero
  ids_no_change <- abs(rel) <= tol | (ids_ref_zero & abs(dy) < tol2)
  res[ids_has_vals & ids_no_change] <- cl[is.na(cl[, "rx_vs_tol"]), "name"]

  # increase: larger change than tol OR (in case rel is infinite:)
  #           ref is zero and res is positive
  ids_inc <- rel > tol | (ids_ref_zero & dy > 0)
  res[ids_has_vals & !ids_no_change & ids_inc] <-
    cl[cl[, "rx_vs_tol"] %in% ">", "name"]

  # decrease: smaller change than -tol OR (in case rel is infinite:)
  #           ref is zero and res is negative
  ids_dec <- rel < -tol | (ids_ref_zero & dy < 0)
  res[ids_has_vals & !ids_no_change & ids_dec] <-
    cl[cl[, "rx_vs_tol"] %in% "<", "name"]

  stopifnot(!anyNA(res[subset, ]))

  res
}
