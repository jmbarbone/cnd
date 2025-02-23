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

test_that("cep in use", {
  skip_on_cran()
  expect_no_error(
    expect_no_warning(
      check_cep_in_use()
    )
  )
})
