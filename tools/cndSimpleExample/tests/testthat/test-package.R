test_that("registry found in package", {
  expect_s3_class(
    .__CND_REGISTRY__.,
    "cnd:registry"
  )
})

test_that("registrary is picking up the conditions from the example package", {
  from_cnd <- cnd::conditions("cndSimpleExample")
  from_example <- Filter(
    \(e) inherits(e, "cnd::condition_generator"),
    as.list(getNamespace("cndSimpleExample"), all.names = TRUE, sorted = TRUE)
  )
  expect_setequal(from_cnd, from_example)
})

test_that("cndSimpleExample registry is loaded into cnd", {
  expect_identical(
    cnd:::registrar$.__REGISTRIES__.$cndSimpleExample,
    .__CND_REGISTRY__.
  )
})
