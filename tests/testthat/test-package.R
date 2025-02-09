test_that("package functions", {
  skip_on_cran()
  skip_if_not(dir.exists("tools/cep"))

  ok <- tempfile()
  bad <- tempfile()

  on.exit({
    unlink("cep_0.0.0.tar.gz")
    file.remove(ok, bad)
  })

  res <- system2(
    "R",
    c("--vanilla CMD build", file.path(getwd(), "tools", "cep")),
    stdout = ok,
    stderr = bad
  )

  expect_identical(res, 0L)
  expect_identical(readLines(bad), "")
  succeed(info = c("cnd_exports()", "cnd_document()"))
})
