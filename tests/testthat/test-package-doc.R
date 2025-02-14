test_that("documentation() works", {
  expect_type(cnd_section("cnd"), "character")
  test_documentation("cnd")
})
