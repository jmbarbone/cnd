test_that("documentation() works", {
  reg <- local_registry()

  expect_error(
    cnd_document(package = NULL),
    class = "cnd:cnd_document_pkg_reg"
  )

  expect_error(
    cnd_document(registry = NULL),
    class = "cnd:cnd_document_pkg_reg"
  )

  expect_warning(
    cnd_document("cnd", registry = reg, file = NULL),
    class = "cnd:cnd_document_conditions"
  )

  expect_error(
    cnd_document("cnd", file = NA),
    class = "cnd:cnd_document_file"
  )

  con <- file()
  on.exit(close(con))
  expect_s3_class(cnd_document("cnd", file = con), "connection")

  expect_type(cnd_document("cnd", file = NULL), "character")
  expect_type(cnd_section(cnd), "character")
})

test_that("test_documentation()", {
  skip_on_cran()
  test_documentation()
})

test_that("snapshots", {
  expect_snapshot(cat(cnd_section(cnd)))
  expect_snapshot(cnd_document("cnd", file = stdout()))
})
