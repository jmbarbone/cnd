
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

  withr::with_dir(cep_dir(), {
    cep <- pkgload::load_all(quiet = TRUE)$env
    pkgload::unload("cep")
    expect_type(conditions(cep$example_function), "list")
    expect_error(cep$example_function(TRUE), NA)
    expect_error(cep$example_function(0), class = "cep:bad_argument")
  })
}
