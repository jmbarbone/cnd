test_that("test_cep()", {
  on.exit(registrar$remove("cep"))
  skip_on_cran()
  test_cep()

  # retain here to get the registry
  test_documentation("cep")
})

test_that("cep R CMD check", {
  skip_on_cran()
  check_rcmdcheck()
})
