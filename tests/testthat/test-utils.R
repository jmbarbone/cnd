test_that("utils works", {
  expect_identical(
    get_package(environment(get_package)),
    "cnd"
  )
})
