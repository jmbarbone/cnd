library(testthat)

scrub_environment_code <- function(x) {
  # macos: 9
  # linux: 12
  # windows: 16
  m <- regexpr("(?<=<environment: 0x)[0-9a-f]+(?=>)", x, perl = TRUE)
  regmatches(x, m) <- strrep("0", max(0, 12L))
  x
}

skip_if_no_cep <- function() {
  skip_if_not(dir.exists(cep_dir()), "tools/cep package directory not found")
}

cep_dir <- function() {
  requireNamespace("here", quietly = TRUE)
  here::here("tools/cep")
}

test_cep <- function() {
  skip_if_not_installed("pkgload")
  skip_if_no_cep()
  cep <- pkgload::load_all(cep_dir(), attach = FALSE, quiet = TRUE)$env
  expect_type(conditions(cep$example_function), "list")
  expect_error(cep$example_function(TRUE), NA)
  expect_error(cep$example_function(0), class = "cep:bad_argument")
}

test_documentation <- function(package) {
  skip_if_not_installed("roxygen2")
  con <- file()
  on.exit(close(con))
  cnd_document(package = package, path = con)
  parsed <- roxygen2::parse_text(readLines(con), NULL)
  expect_failure(expect_identical(parsed, list()))
}

local_reg <- function(name) {
  remove_registry(name)
  create_registry(name)
  do.call(on.exit, list(remove_registry(name)), envir = parent.frame())
  get_registry(name)
}
