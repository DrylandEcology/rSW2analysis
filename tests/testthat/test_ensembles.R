context("Ensemble of models")


n <- 10
d <- 0.5
xna <- rep(NA, 2 * n)

xs <- list(
  all_NAs = list(mode = NA, span = NA, x = xna),
  logical = list(mode = m <- TRUE, span = 2, x = c(rep(m, n), !m)),
  integer = list(
    mode = mi <- 1L,
    span = si <- 9,
    x = xi <- c(rep(m, n), seq.int(mi, mi + si))
  ),
  numeric = list(mode = mi + d, x = xi + d, span = si),
  character = list(
    mode = mc <- "test",
    span = sc <- nchar(mc),
    x = xc <- c(
      rep(mc, n),
      sapply(
        seq_len(sc),
        function(x) substr(mc, start = x, stop = sc)
      )
    )
  ),
  factor_character = list(
    x = x <- as.factor(xc),
    span = nlevels(x),
    mode = factor(mc, levels = levels(x))
  ),
  factor_integer = list(
    x = x <- as.factor(xi),
    span = nlevels(x),
    mode = factor(mi, levels = levels(x))
  )
)


test_that("output analysis: majority", {
  for (k in seq_along(xs)) {
    info <- paste("Case:", shQuote(names(xs)[k]))

    m <- xs[[k]][["mode"]]
    x1 <- xs[[k]][["x"]]

    # Without NAs
    expect_equal(majority(x1), m, info = info)
    expect_equal(majority(x1, na.rm = TRUE), m, info = info)

    # With NAs
    x2 <- if (is.factor(x1)) {
        factor(c(as.character(x1), xna), levels = levels(x1))
      } else {
        c(x1, xna)
      }
    expect_equal(majority(x2), m[NA], info = info)
    expect_equal(majority(x2, na.rm = TRUE), m, info = info)
  }

  # Additional cases:
  # `x` with two modes (one of which is randomly selected)
  tt <- xs[["integer"]]
  m2 <- 1L + tt[["mode"]]
  expect_true(majority(c(tt[["x"]], rep(m2, n))) %in% c(tt[["mode"]], m2))
})


test_that("output analysis: span", {
  for (k in seq_along(xs)) {
    cl <- names(xs)[k]
    info <- paste("Case:", shQuote(cl))
    if (grepl("_", cl)) {
      cl <- strsplit(cl, split = "_")[[1]][[1]]
    }

    if (
      is.function(selectMethod("span", signature = c(x = cl), optional = TRUE))
    ) {

      x1 <- xs[[k]][["x"]]
      s <- xs[[k]][["span"]]

      # Without NAs
      expect_equal(span(x1), s, info = info)
      expect_equal(span(x1, na.rm = TRUE), s, info = info)

      # With NAs
      x2 <- if (is.factor(x1)) {
        factor(c(as.character(x1), xna), levels = levels(x1))
      } else {
        c(x1, xna)
      }
      expect_equal(span(x2), s[NA], info = info)
      expect_equal(span(x2, na.rm = TRUE), s, info = info)
    }
  }

})
