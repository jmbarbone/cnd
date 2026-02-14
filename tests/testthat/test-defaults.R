test_that("defaults works", {
  foo <- function(value) {
    cnd(class_error(x = value, expected = integer()))
  }

  expect_error(
    foo(TRUE),
    "Invalid class for `value`: got 'logical', expected 'integer'",
    class = "class_error",
    fixed = TRUE
  )
})
