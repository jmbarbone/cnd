test_that("test_cep()", {
  on.exit(remove_registry("cep"))
  skip_on_cran()
  test_cep()
  test_documentation("cep", "tools/cep")
})
