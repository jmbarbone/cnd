test_that("registry found in package", {
  expect_s3_class(
    cep:::.__CND_REGISTRY__.,
    "cnd:registry"
  )
})

test_that("registrary is picking up the conditions from the cep package", {
  from_cnd <- cnd::conditions("cep")
  from_cep <- Filter(
    \(e) inherits(e, "cnd::condition_generator"),
    as.list(getNamespace("cep"), all.names = TRUE, sorted = TRUE)
  )
  expect_setequal(from_cnd, from_cep)
})

test_that("cep registry is loaded into cnd", {
  expect_identical(
    cnd:::registrar$.__REGISTRIES__.$cep,
    cep:::.__CND_REGISTRY__.
  )
})
