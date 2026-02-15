test_that("condition() works", {
  expect_s3_class(condition, "cnd::condition_progenitor")
  con <- expect_no_error(condition("foo", register = FALSE))
  expect_s3_class(con, "cnd::condition_generator")
})

test_that("condition() conditions", {
  expect_error(condition("foo", type = "bad"))

  expect_warning(
    condition("foo", exports = "exports", package = NULL),
    class = "cnd:no_package_exports_warning"
  )

  expect_error(
    condition("foo", 1),
    class = "cnd:condition_message_error"
  )
})

test_that("condition() works", {
  expect_identical(
    conditions("cnd"),
    conditions()
  )

  expect_identical(
    conditions(condition),
    conditions(fun = condition)
  )
})

test_that("conditions(x) <- value", {
  reg <- local_registry()
  foo <- function() {}
  conditions(foo) <- condition("foo", package = "test", registry = reg)
  expect_s3_class(foo, "cnd::conditioned_function")
  conditions(foo) <- NULL
  expect_failure(expect_s3_class(foo, "test::conditioned_function"))
})

test_that("find_cond() works", {
  expect_identical(find_cond(cnd_class_error), cnd_class_error)
})

test_that("find_cond() fails", {
  expect_error(find_cond("foo:bar"))
  expect_error(find_cond("foooo"))
})

test_that("cnd()", {
  expect_error(cnd(1), class = "cnd:cnd_class_error")

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
  reg <- local_registry()
  foo <- condition("foo", package = "foo", registry = reg)
  expect_identical(condition("foo:foo"), foo)
})

test_that("condition(help = gets_collapsed)", {
  reg <- local_registry()
  foo <- condition("foo", help = c("one", "two"), registry = reg)
  expect_identical(foo$help, "onetwo")
})

test_that("conditions(..1)", {
  expect_warning(
    conditions("cnd_class_error", "cnd"),
    class = conditions_dots_warning$class
  )
})

test_that("condition(type = 'condition')", {
  reg <- local_registry()
  foo <- condition("foo", type = "condition", package = "help", registry = reg)
  expect_identical(foo, condition("help:foo"))
  expect_snapshot(foo())
})

test_that("find_cond()", {
  expect_s3_class(
    find_cond("cnd:cnd_class_error/error"),
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
    class = "cnd:invalid_condition_error"
  )

  expect_error(
    validate_condition(letters, NULL, NULL),
    class = "cnd:invalid_condition_error"
  )

  expect_error(
    validate_condition("foo!bar", NULL, NULL),
    class = "cnd:invalid_condition_error"
  )

  expect_error(
    validate_condition("foo", NULL, 1),
    class = "cnd:invalid_condition_error"
  )

  expect_error(
    validate_condition("foo", 1, NULL),
    class = "cnd:invalid_condition_error"
  )
})

test_that("cget() and $ and [", {
  expect_identical(
    cget(cnd_class_error, "class"),
    cnd_class_error$class
  )

  expect_identical(
    cget(cnd_class_error, "class"),
    cnd_class_error["class"]
  )
})

test_that("as.character() error", {
  expect_error(
    as.character(condition_as_character_error),
    class = "cnd:condition_as_character_error"
  )
})

test_that(".call", {
  get_call <- function(expr) tryCatch(expr, error = function(e) e$call)
  err <- condition("foo", type = "error", register = FALSE)
  foo <- function() stop(err())
  expect_identical(get_call(foo()), quote(foo()))
  expect_snapshot(foo(), error = TRUE)

  bar <- function() foo()
  expect_identical(get_call(bar()), quote(foo()))
  expect_snapshot(bar(), error = TRUE)

  foo <- function() cnd(err(.call = FALSE))
  expect_null(get_call(foo()))
  expect_snapshot(foo(), error = TRUE)

  fizz <- function() bar()

  foo <- function() stop(err(.call = 0))
  expect_identical(get_call(fizz()), quote(foo()))
  expect_snapshot(fizz(), error = TRUE)

  foo <- function() stop(err(.call = 1))
  expect_identical(get_call(fizz()), quote(bar()))
  expect_snapshot(fizz(), error = TRUE)

  foo <- function() stop(err(.call = 2))
  expect_identical(get_call(fizz()), quote(fizz()))
  expect_snapshot(fizz(), error = TRUE)

  foo <- function() stop(err(.call = "anything I want"))
  expect_identical(get_call(fizz()), "anything I want")
  expect_snapshot(fizz(), error = TRUE)
})

test_that("cnd(condition) handling", {
  foo <- condition(
    "foo",
    "CONDITION",
    type = "condition",
    register = FALSE
  )

  expect_condition(
    expect_output(cnd(foo()), "CONDITION"),
    class = "cnd::condition"
  )

  expect_output(
    expect_no_condition(
      suppress_conditions(cnd(foo())),
    ),
    NA
  )
})

test_that("conditinMessage(condition_generator)", {
  expect_error(
    conditionMessage(condition("foo", register = FALSE)),
    class = "cnd:condition_message_generator_error"
  )
})

test_that("condition_generators are closures, and not subsettable", {
  co <- condition("_", register = FALSE)
  co$extra <- TRUE
  expect_true(co$extra)
})

test_that("condition(class) is deprecated", {
  expect_warning(
    condition(class = "foo"),
    class = "deprecated_warning"
  )
})
