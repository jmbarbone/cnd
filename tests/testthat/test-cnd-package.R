test_that("force evaluation doesn't fail", {
  expect_invisible(cnd_evaluate())
})

test_that("internal_error()", {
  expect_s3_class(internal_error(), "cnd:internal_error")
})
