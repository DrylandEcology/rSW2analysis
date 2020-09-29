



setGeneric("span", function(x, na.rm = FALSE, ...) standardGeneric("span"))

setMethod("span", signature(x = "numeric"),
  function(x, na.rm = FALSE, ...) {
    res <- diff(range(x, na.rm = na.rm, ...))
    if (is.infinite(res)) {
      dots <- list(na.rm = na.rm, ...)
      if (isTRUE(dots[["na.rm"]]) || isTRUE(dots[["finite"]])) {
        res <- NA
      }
    }
    res
  })

setMethod("span", signature(x = "character"),
  function(x, na.rm = FALSE, ...) {
    if (na.rm) {
      x <- stats::na.exclude(x)
    } else {
      if (anyNA(x)) return(NA_integer_)
    }

    length(unique(x, ...))
  })

setMethod("span", signature(x = "factor"),
  function(x, na.rm = FALSE, ...) {
    if (na.rm) {
      x <- factor(x, exclude = NA)
    } else {
      if (anyNA(x) && !anyNA(levels(x))) return(NA_integer_)
    }

    nlevels(x)
  })


setGeneric(
  name = "agreement",
  def = function(x, val = NULL, ...) standardGeneric("agreement")
)

setMethod("agreement", signature(x = "numeric"),
  function(x, val = NULL, ...) {
    if (is.null(val)) {
      val <- stats::median(x, ...)

    } else {
      if (length(val) > 1) {
        val <- stats::median(val, ...)
      }
    }

    if (is.na(val) || all(is.na(x))) {
      NA
    } else {
      sum(sign(x) == sign(val), ...)
    }
  })

setMethod("agreement", signature(x = "character"),
  function(x, val = NULL, ...) {
    if (is.null(val)) {
      val <- rSW2utils::majority(x, ...)

    } else {
      if (length(val) > 1) {
        val <- rSW2utils::majority(val, ...)
      }
    }

    if (is.na(val) || all(is.na(x))) {
      NA
    } else {
      sum(x == val, ...)
    }
  })

setMethod("agreement", signature(x = "factor"),
  selectMethod("agreement", signature = "character"))



quantiles_areaweighted <- function(x, area, probs, na.rm = FALSE) {
  stopifnot(requireNamespace("Hmisc"))
  # non-random reliability weights: normwt = TRUE
  Hmisc::wtd.quantile(x, weights = area, probs = probs, normwt = TRUE,
    na.rm = na.rm)
}

fun_central <- function(method = c("median", "mean"), area = NULL,
  na.rm = FALSE) {

  method <- match.arg(method)

  if (!is.null(area) && isTRUE(stats::var(area) > 0)) {
    switch(method,
      median = function(x) quantiles_areaweighted(x, area = area, probs = 0.5,
        na.rm = na.rm),
      mean = function(x) stats::weighted.mean(x, w = area, na.rm = na.rm))

  } else {
    switch(method,
      median = function(x) stats::median(x, na.rm = na.rm),
      mean = function(x) mean(x, na.rm = na.rm))
  }
}






#' Univariate novelty/extrapolation NT1
#'
#' "NT1 ranges from infinite negative values to zero where zero indicates no
#' extrapolation beyond the univariate coverage of reference data"
#' (Mesgaran et al. 2014).
#'
#' @param refdat A numerical matrix. The reference values of variables organized
#'   in columns.
#' @param prodat A numerical matrix. The projected values. The columns must
#'   match those of \code{refdat}.
#'
#' @references Mesgaran, M. B., R. D. Cousens, B. L. Webber, and J. Franklin.
#'   2014. Here be dragons: a tool for quantifying novelty due to covariate
#'   range and correlation change when projecting species distribution models.
#'   Diversity and Distributions 20:1147-1159.
#'
#' @section Notes: The code is adapted from Bell & Schlaepfer 2015 (available
#'   at \url{https://github.com/bellland/SDM.Virtual.Species_Bell.Schlaepfer})
#'   which was based on a comment by Matthew Bayly made at
#nolint start
#'   \url{https://pvanb.wordpress.com/2014/05/13/a-new-method-and-tool-exdet-to-evaluate-novelty-environmental-conditions/}.
#nolint end
#'
#' @export
calc_NT1 <- function(refdat, prodat) {
  stopifnot(requireNamespace("matrixStats"))
  stopifnot(identical(colnames(refdat), colnames(prodat)))

  range_ref <- t(matrixStats::colRanges(refdat, na.rm = TRUE))
  range_ref_arr <- array(range_ref, dim = c(dim(range_ref), nrow(prodat)),
    dimnames = list(c("min", "max"), colnames(refdat), NULL))

  diffs_ref <- matrixStats::colDiffs(range_ref)
  diffs_ref_arr <- matrix(diffs_ref, nrow = nrow(prodat), ncol = ncol(prodat),
    byrow = TRUE)

  iud <- array(0, dim = c(dim(prodat), 3))
  iud[ , , 2] <- prodat - t(range_ref_arr["min", ,]) #nolint
  iud[ , , 3] <- t(range_ref_arr["max", ,]) - prodat #nolint

  UDs <- apply(iud, 1:2, min) / diffs_ref_arr
  rowSums(UDs)
}

#' Multivariate novelty/extrapolation NT2
#'
#' "NT2 can range from zero up to unbounded positive values. NT2 values
#' ranging from zero to one indicate similarity (in terms of both univariate
#' range and multivariate combination), with values closer to zero being more
#' similar. Values larger than one are indicative of novel combinations"
#' (Mesgaran et al. 2014).
#'
#' Calculates the Mahalanobis distance of each observation to the environmental
#' center of the reference set for both the reference and the projection data
#' set and calculate the ratio between the two.
#'
#' @param refdat A numerical matrix. The reference values of variables organized
#'   in columns.
#' @param prodat A numerical matrix. The projected values. The columns must
#'   match those of \code{refdat}.
#'
#' @references Mesgaran, M. B., R. D. Cousens, B. L. Webber, and J. Franklin.
#'   2014. Here be dragons: a tool for quantifying novelty due to covariate
#'   range and correlation change when projecting species distribution models.
#'   Diversity and Distributions 20:1147-1159.
#'
#' @section Notes: The code is adapted from Bell & Schlaepfer 2015 (available
#'   at \url{https://github.com/bellland/SDM.Virtual.Species_Bell.Schlaepfer})
#'   which was based on a comment by Matthew Bayly made at
#nolint start
#'   \url{https://pvanb.wordpress.com/2014/05/13/a-new-method-and-tool-exdet-to-evaluate-novelty-environmental-conditions/}.
#nolint end
#'
#' @export
calc_NT2 <- function(refdat, prodat) {
  stopifnot(identical(colnames(refdat), colnames(prodat)))

  # Calculate the center of reference data: average and covariance matrix
  ref_av  <- colMeans(refdat, na.rm = TRUE)
  ref_cov <- stats::var(refdat, na.rm = TRUE)

  # Mahalanobis distance of reference data to center of reference data
  mah_ref <- stats::mahalanobis(x = refdat, center = ref_av, cov = ref_cov)
  # Mahalanobis distance of projected data to center of reference data
  mah_pro <- stats::mahalanobis(x = prodat, center = ref_av, cov = ref_cov)

  # Ratio
  mah_max <- max(mah_ref[is.finite(mah_ref)])
  mah_pro / mah_max
}



#' Whittaker additive elements
#'
#' @references
#'   Whittaker, J. 1984. Model Interpretation from the Additive Elements of
#'   the Likelihood Function. Journal of the Royal Statistical Society.
#'   Series C (Applied Statistics) 33:52-64.
#'
#' @section Details:
#' Residual: G(:1234) = g_1234 = residual variation
#' Primaries: G(1:234) = g1_234 = variation uniquely attributable to x1
#' Secondaries: G(12:34) = g12_34 = variation attributable to x1 or x2 but
#'   not to both
#' 3rd order elements: G(123:4) = g123_4 =
#' 4th order elements: G(1234:) = g1234_ =
#'
#' @examples
#' ## Test code to match up with cement example of Whittaker 1984
#' \dontrun{
#' if (requireNamespace("MuMIn")) {
#'   if (packageVersion("bit") <= "4.0.4") {
#'     # bit:::chunk.default fails, see #3
#'     stopifnot(require("ff"))
#'   }
#'
#'   probs_cement <- whittaker_additive_elements(
#'     data = MuMIn::Cement,
#'     X1 = "X1", X2 = "X2", X3 = "X3", X4 = "X4"
#'   )$probs
#' }
#'
#' #            Residuals           Primary_X1           Primary_X2
#' #                  1.8                  1.0                  0.1
#' #           Primary_X3           Primary_X4      Secondary_X1:X2
#' #                  0.0                  0.0                  3.6
#' #      Secondary_X1:X3      Secondary_X1:X4      Secondary_X2:X3
#' #                 29.3                 12.6                  0.9
#' #      Secondary_X2:X4      Secondary_X3:X4    Tertiary_X1:X2:X3
#' #                 43.3                  0.4                 -4.1
#' #    Tertiary_X1:X2:X4    Tertiary_X1:X3:X4    Tertiary_X2:X3:X4
#' #                  9.1                -11.6                  0.2
#' # Quartery_X1:X2:X3:X4
#' #                 13.5
#' }
#'
#' @export
whittaker_additive_elements <- function(data, y = "y", w = NULL,
  X1 = "Region", X2 = "Shift", X3 = "GCM", X4 = "RCP",
  family = stats::gaussian()) {

  stopifnot(
    requireNamespace("biglm"),
    requireNamespace("ff"),
    requireNamespace("ffbase")
  )

  probs <- NA
  gtotal <- 0

  # Prepare for ff data frame
  ffdata <- ff::as.ffdf(data)

  if (stats::var(ffdata[, y]) > 0) {
    # extract residual SS == residual deviance

    op1 <- if (!is.null(X1)) "+" else NULL #nolint
    op2 <- if (!is.null(X2)) "+" else NULL
    op3 <- if (!is.null(X3)) "+" else NULL
    op4 <- if (!is.null(X4)) "+" else NULL

    # Total
    feq <- stats::as.formula(paste(y, "~ 1"))
    tmp_fit <- ffbase::bigglm.ffdf(feq,
      weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
      family = family, data = ffdata)
    gtotal <- stats::deviance(tmp_fit)

    # Residual
    feq <- stats::as.formula(paste(y, "~", X1, op2, X2, op3, X3, op4, X4))
    tmp_fit <- ffbase::bigglm.ffdf(feq,
      weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
      family = family, data = ffdata)
    g_1234 <- stats::deviance(tmp_fit)

    # Primaries
    feq <- stats::as.formula(paste(y, "~", X2, op3, X3, op4, X4))
    tmp_fit <- ffbase::bigglm.ffdf(feq,
      weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
      family = family, data = ffdata)
    g1_234 <- stats::deviance(tmp_fit) - g_1234

    feq <- stats::as.formula(paste(y, "~", X1, op3, X3, op4, X4))
    tmp_fit <- ffbase::bigglm.ffdf(feq,
      weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
      family = family, data = ffdata)
    g2_134 <- stats::deviance(tmp_fit) - g_1234

    feq <- stats::as.formula(paste(y, "~", X1, op2, X2, op4, X4))
    tmp_fit <- ffbase::bigglm.ffdf(feq,
      weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
      family = family, data = ffdata)
    g3_124 <- stats::deviance(tmp_fit) - g_1234

    feq <- stats::as.formula(paste(y, "~", X1, op2, X2, op3, X3))
    tmp_fit <- ffbase::bigglm.ffdf(feq,
      weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
      family = family, data = ffdata)
    g4_123 <- stats::deviance(tmp_fit) - g_1234


    #Secondaries
    g12_34 <- 0
    if (!is.null(X3) || !is.null(X4)) {
      feq <- stats::as.formula(paste(y, "~", X3, op4, X4))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g12_34 <- stats::deviance(tmp_fit) - g_1234 - g1_234 - g2_134
    }

    g13_24 <- 0
    if (!is.null(X2) || !is.null(X4)) {
      feq <- stats::as.formula(paste(y, "~", X2, op4, X4))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g13_24 <- stats::deviance(tmp_fit) - g_1234 - g1_234 - g3_124
    }

    g14_23 <- 0
    if (!is.null(X2) || !is.null(X3)) {
      feq <- stats::as.formula(paste(y, "~", X2, op3, X3))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g14_23 <- stats::deviance(tmp_fit) - g_1234 - g1_234 - g4_123
    }

    g23_14 <- 0
    if (!is.null(X1) || !is.null(X4)) {
      feq <- stats::as.formula(paste(y, "~", X1, op4, X4))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g23_14 <- stats::deviance(tmp_fit) - g_1234 - g2_134 - g3_124
    }

    g24_13 <- 0
    if (!is.null(X1) || !is.null(X3)) {
      feq <- stats::as.formula(paste(y, "~", X1, op3, X3))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g24_13 <- stats::deviance(tmp_fit) - g_1234 - g2_134 - g4_123
    }

    g34_12 <- 0
    if (!is.null(X1) || !is.null(X2)) {
      feq <- stats::as.formula(paste(y, "~", X1, op2, X2))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g34_12 <- stats::deviance(tmp_fit) - g_1234 - g3_124 - g4_123
    }

    #Order 3:
    g123_4 <- 0
    if (!is.null(X4)) {
      feq <- stats::as.formula(paste(y, "~", X4))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g123_4 <- stats::deviance(tmp_fit) -
        g_1234 - g1_234 - g2_134 - g3_124 - g12_34 - g13_24 - g23_14
    }

    g124_3 <- 0
    if (!is.null(X3)) {
      feq <- stats::as.formula(paste(y, "~", X3))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g124_3 <- stats::deviance(tmp_fit) -
        g_1234 - g1_234 - g2_134 - g4_123 - g12_34 - g14_23 - g24_13
    }

    g134_2 <- 0
    if (!is.null(X2)) {
      feq <- stats::as.formula(paste(y, "~", X2))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g134_2 <- stats::deviance(tmp_fit) -
        g_1234 - g1_234 - g3_124 - g4_123 - g13_24 - g14_23 - g34_12
    }

    g234_1 <- 0
    if (!is.null(X1)) {
      feq <- stats::as.formula(paste(y, "~", X1))
      tmp_fit <- ffbase::bigglm.ffdf(feq,
        weights = if (!is.null(w)) stats::as.formula(paste("~", w)),
        family = family, data = ffdata)
      g234_1 <- stats::deviance(tmp_fit) -
        g_1234 - g2_134 - g3_124 - g4_123 - g23_14 - g24_13 - g34_12
    }

    #Order 4:
    g1234_ <- gtotal - g_1234 -
      g1_234 - g2_134 - g3_124 - g4_123 -
      g12_34 - g13_24 - g14_23 - g23_14 - g24_13 - g34_12 -
      g123_4 - g124_3 - g134_2 - g234_1


    #Relative elements
    tmp <- c(
      g_1234,
      g1_234, g2_134, g3_124, g4_123,
      g12_34, g13_24, g14_23, g23_14, g24_13, g34_12,
      g123_4, g124_3, g134_2, g234_1,
      g1234_
    )
    probs <- round(100 * tmp / gtotal, 1)

    names(probs) <- c("Residuals",
      paste0("Primary_", c(X1, X2, X3, X4)),
      paste0("Secondary_", c(paste0(X1, ":", X2), paste0(X1, ":", X3),
        paste0(X1, ":", X4), paste0(X2, ":", X3), paste0(X2, ":", X4),
        paste0(X3, ":", X4))),
      paste0("Tertiary_", c(paste0(X1, ":", X2, ":", X3),
        paste0(X1, ":", X2, ":", X4), paste0(X1, ":", X3, ":", X4),
        paste0(X2, ":", X3, ":", X4))),
      paste0("Quartery_", X1, ":", X2, ":", X3, ":", X4))
  }

  list(probs = probs, total = gtotal)
}




#' @export
get_data_for_agreement_sign <- function(data_direction, data_agree) {
  ids_neg <- !is.na(data_direction) & data_direction < 0
  data_agree[ids_neg] <- -data_agree[ids_neg]
  data_agree
}

#' @export
get_zlim_for_agreement_sign <- function(data_agree, zlim = NULL) {
  if (is.null(zlim)) {
    range(data_agree, na.rm = TRUE)
  } else if (sum(data_agree < 0, na.rm = TRUE) > 0) {
    c(-1 * abs(zlim[1]), abs(zlim[2]))
  } else {
    zlim
  }
}
