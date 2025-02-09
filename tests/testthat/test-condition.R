test_that("condition() works", {
  expect_s3_class(condition, "cnd::condition_generator")
  con <- expect_no_error(condition("foo"))
  expect_s3_class(con, "cnd::condition_function")
})

test_that("condition() conditions", {
  # TODO replace with custom function
  expect_error(condition("foo", type = "bad"))

  expect_warning(
    condition("foo", exports = "exports", package = NULL),
    class = "cnd:no_package_exports"
  )

  expect_error(
    condition("foo", 1),
    class = "cnd:invalid_condition_message"
  )
})

test_that("condition() works", {
  expect_identical(
    conditions("cnd"),
    conditions(package = "cnd")
  )

  expect_identical(
    conditions(condition),
    conditions(fun = condition)
  )
})

test_that("conditions(x) <- value", {
  foo <- function() {}
  conditions(foo) <- condition("foo", package = "cnd:testing")
  expect_s3_class(foo, "cnd::conditioned_function")
})

test_that("cond() works", {
  expect_identical(cond(cond_cnd_class), cond_cnd_class)
})

test_that("cond() fails", {
  expect_error(cond("foo:bar"))
  expect_error(cond("foooo"))
})

test_that("cnd()", {
  con <- condition(
    "foo_message",
    "a message",
    type = "message",
    package = NULL
  )

  expect_message(cnd(con()), class = "foo_message")

  con <- condition(
    "foo_warning",
    "a warning",
    type = "warning",
    package = NULL
  )
  expect_warning(cnd(con()), class = "foo_warning")

  con <- condition(
    "foo_error",
    "an error",
    type = "error",
    package = NULL
  )
  expect_error(cnd(con()), class = "foo_error")
})

# TODO test for $help
# TODO test for multiple conditions()?
# TODO test for cnd() errors
# TODO test for conditions(fun) <- NULL
# TODO test for as.character.cnd::condition_function()
