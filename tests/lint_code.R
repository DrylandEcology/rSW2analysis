#--- Code style

#nolint start
# Problem: `R CMD check` doesn't allow hidden files
# including the lintr settings; thus, we exclude `.lintr` via `.Rbuildignore`.
# Consequently, we wouldn't be able to lint with our settings
# during package checks with `R CMD check`/`devtools::check()`.

# ==> Configure linters here in code instead of via file `.lintr`


# The linting code can be run via any the following options
# (assuming the current working directory is at the root of the source package)
#   - `Sys.setenv(NOT_CRAN = "true"); source("tests/lint_code.R")`
#   - `devtools::check(env_vars = c(NOT_CRAN = "true"))`
#   - `R CMD build . && NOT_CRAN="true" R CMD check *.tar.gz`
#nolint end

if (
  requireNamespace(
    "lintr",
    versionCheck = list(op = ">=", version = "2.0")
  ) &&
  # skip_on_cran
  isTRUE(tolower(Sys.getenv("NOT_CRAN")) %in% c(1, "yes", "true")) &&
  # skip_on_appveyor
  !isTRUE(tolower(Sys.getenv("APPVEYOR")) %in% c(1, "yes", "true")) &&
  # skip_on_covr
  !isTRUE(tolower(Sys.getenv("R_COVR")) %in% c(1, "yes", "true"))
) {

  # Locate package source directory
  is_package_source_path <- function(path) {
    dir.exists(path) && file.exists(file.path(path, "DESCRIPTION"))
  }

  # During interactive session and sourcing the file
  pkg_path <- "."

  # During unit testing, the current path is set to `tests/testthat/`
  if (!is_package_source_path(pkg_path)) {
    pkg_path <- file.path("..", "..")

    if (!is_package_source_path(pkg_path)) {
      # During package checks, the current path is a temporary build directory
      # Code chunk based on `spelling::spell_check_test`
      if (!is_package_source_path(pkg_path)) {
        pkg_path <- list.files(file.path("..", "00_pkg_src"), full.names = TRUE)

        if (!length(pkg_path)) {
          check_dir <- dirname(getwd())
          if (grepl("\\.Rcheck$", check_dir)) {
            source_dir <- sub("\\.Rcheck$", "", check_dir)
            if (file.exists(source_dir)) {
              pkg_path <- source_dir
            }
          }
        }
      }
    }
  }


  if (is_package_source_path(pkg_path)) {

    # List files that shouldn't be linted
    files_not_tolint <- file.path("R", "RcppExports.R")

    # `lintr::expect_lint_free` and `lintr::lint_package` lint R code
    # only in "R", "tests", "inst"
    paths <- file.path(
      pkg_path,
      c("data-raw", "doc", "demo", "R", "tests", "inst")
    )

    linters_config <- lintr::with_defaults(
      #------ DEFAULT LINTERS
      lintr::assignment_linter,
      lintr::closed_curly_linter(allow_single_line = TRUE),
      lintr::commas_linter,
      lintr::commented_code_linter,
      lintr::equals_na_linter,
      lintr::function_left_parentheses_linter,
      lintr::infix_spaces_linter,
      lintr::line_length_linter(80L),
      lintr::no_tab_linter,
      lintr::object_length_linter(30L),
      lintr::object_usage_linter,
      lintr::open_curly_linter(allow_single_line = TRUE),
      lintr::paren_brace_linter,
      lintr::pipe_continuation_linter,
      lintr::seq_linter,
      lintr::single_quotes_linter,
      lintr::spaces_inside_linter,
      lintr::spaces_left_parentheses_linter,
      lintr::trailing_blank_lines_linter,
      lintr::trailing_whitespace_linter,
      #--- Turn off default linters for now:
      object_name_linter = NULL,
      cyclocomp_linter = NULL,
      #------ NON-DEFAULT LINTERS
      #--- Not activated non-default linters:
      #lintr::extraction_operator_linter,
      #lintr::implicit_integer_linter,
      #lintr::todo_comment_linter,
      # see https://github.com/jimhester/lintr/issues/468
      #lintr::nonportable_path_linter(lax = TRUE),
      #--- Activated non-default linters:
      lintr::absolute_path_linter(lax = TRUE),
      lintr::nonportable_path_linter(lax = TRUE),
      lintr::infix_spaces_linter,
      lintr::T_and_F_symbol_linter,
      lintr::semicolon_terminator_linter(
        semicolon = c("compound", "trailing")
      ),
      lintr::undesirable_function_linter,
      lintr::undesirable_operator_linter,
      lintr::unneeded_concatenation_linter
    )

    lints <- lintr::lint_dir(
      path = normalizePath(paths[dir.exists(paths)]),
      exclusions = list(files_not_tolint),
      linters = linters_config,
      parse_settings = FALSE
    )

    if (length(lints) > 0) {
      print(lints)
      stop("Not lint free.")
    }

  } else {
    warning(
      "No linting: failed to find package source at ",
      shQuote(normalizePath(pkg_path, mustWork = FALSE))
    )
  }
}
