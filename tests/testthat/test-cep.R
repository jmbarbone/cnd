test_that("test_cep()", {
  on.exit(registrar$remove("cep"))
  skip_on_cran()
  test_cep()

  # retain here to get the registry
  test_documentation("cep")
})

test_that("cep in use", {
  skip_on_cran()
  skip_if_not_installed("here")
  err <- tempfile()
  on.exit(file.remove(err))

  expect_no_error({
    system2(
      "Rscript",
      c("--vanilla", here::here("tools/test-cep-in-use.R")),
      stdout = FALSE,
      stderr = err
    )

    warn <- readLines(err)
    if (length(warn) > 0) {
      stop(paste(warn, collapse = "\n"))
    }
  })
})
