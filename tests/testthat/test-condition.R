test_that("condition() works", {
  expect_s3_class(condition, "cnd::condition_progenitor")
  con <- expect_no_error(condition("foo"))
  expect_s3_class(con, "cnd::condition_generator")
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
  conditions(foo) <- NULL
  expect_failure(expect_s3_class(foo, "cnd::conditioned_function"))
})

test_that("find_cond() works", {
  expect_identical(find_cond(cond_cnd_class), cond_cnd_class)
})

test_that("find_cond() fails", {
  expect_error(find_cond("foo:bar"))
  expect_error(find_cond("foooo"))
})

test_that("cnd()", {
  expect_error(cnd(1), class = "cnd:cond_cnd_class")

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

  con <- condition(
    "foo_condition",
    "a condition",
    type = "condition",
    package = NULL
  )
  expect_output(
    expect_condition(cnd(con()), class = "foo_condition")
  )
})

test_that("condition(existing)", {
  foo <- condition("foo", package = "cnd_testing_existing")
  on.exit(remove_registration("cnd_testing_existing"))
  expect_identical(condition("cnd_testing_existing:foo"), foo)
})

test_that("condition(help = gets_collapsed)", {
  foo <- condition("foo", help = c("one", "two"), package = "cnd_testing_help")
  on.exit(remove_registration("cnd_testing_help"))
  expect_identical(foo$help, "onetwo")
})

test_that("conditions(..1)", {
  expect_warning(
    conditions("cond_cnd_class", "cnd"),
    # TODO replace simpleWarning with classed warning
    class = "simpleWarning"
  )
})

test_that("condition(type = 'condition')", {
  foo <- condition("foo", type = "condition", package = "cnd_testing_type")
  on.exit(remove_registration("cnd_testing_type"))
  expect_identical(foo, condition("cnd_testing_type:foo"))
  expect_snapshot(foo())
})

test_that("find_cond()", {
  expect_s3_class(
    find_cond("cnd:cond_cnd_class/error"),
    "cnd::condition_generator"
  )

  expect_warning(
    expect_s3_class(find_cond("/error"), "cnd::condition_generator"),
    class = "simpleWarning"
  )

  expect_warning(
    expect_type(find_cond("/error", .multi = TRUE), "list"),
    class = "simpleWarning"
  )
})

test_that("validate_condition()", {
  expect_error(
    validate_condition(1, NULL, NULL),
    class = "cnd:invalid_condition"
  )

  expect_error(
    validate_condition(letters, NULL, NULL),
    class = "cnd:invalid_condition"
  )

  expect_error(
    validate_condition("foo!bar", NULL, NULL),
    class = "cnd:invalid_condition"
  )

  expect_error(
    validate_condition("foo", NULL, 1),
    class = "cnd:invalid_condition"
  )

  expect_error(
    validate_condition("foo", 1, NULL),
    class = "cnd:invalid_condition"
  )
})

test_that("cget() and $ and [", {
  expect_identical(
    cget(cond_cnd_class, "class"),
    cond_cnd_class$class
  )

  expect_identical(
    cget(cond_cnd_class, "class"),
    cond_cnd_class["class"]
  )
})

test_that("as.character() error", {
  expect_error(
    as.character(cond_as_character_condition),
    class = "cnd:as_character_cnd_error"
  )
})
