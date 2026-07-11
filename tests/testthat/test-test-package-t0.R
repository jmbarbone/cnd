test_that("registry found in package", {
  local_install_and_attach(test_path("t0"))
  expect_s3_class(
    get(".__CND_REGISTRY__.", envir = asNamespace("t0")),
    "cnd:registry"
  )
})

test_that("registrary is picking up the conditions from the example package", {
  from_cnd <- cnd::conditions("t0")
  from_example <- Filter(
    function(e) inherits(e, "cnd::condition_generator"),
    as.list(getNamespace("t0"), all.names = TRUE, sorted = TRUE)
  )
  expect_setequal(unname(from_cnd), unname(from_example))
})

test_that("cndSimpleExample registry is loaded into cnd", {
  expect_identical(
    cnd:::registrar$.__REGISTRIES__.$t0,
    get(".__CND_REGISTRY__.", envir = asNamespace("t0"))
  )
})
