test_that("test_cep()", {
  on.exit(registrar$remove("cep"))
  skip_on_cran()
  test_cep()
  test_documentation("cep")
})
