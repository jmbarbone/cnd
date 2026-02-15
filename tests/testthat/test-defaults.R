test_that("value_error()", {
  expect_s3_class(value_error(), "value_error")
})

test_that("class_error()", {
  expect_s3_class(class_error(), "class_error")

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

test_that("type_error()", {
  expect_s3_class(type_error(), "type_error")

  foo <- function(value) {
    cnd(type_error(x = value, expected = integer()))
  }

  expect_error(
    foo(TRUE),
    "Invalid type for `value`: got 'logical', expected 'integer'",
    class = "type_error",
    fixed = TRUE
  )
})

test_that("input_error()", {
  expect_s3_class(input_error(), "input_error")
})

test_that("use_error()", {
  expect_s3_class(use_error(), "use_error")
})

test_that("defunct_error()", {
  expect_s3_class(defunct_error(), "defunct_error")
  foo <- function() {
    cnd(defunct_error(
      defunct = quote(foo(old = )),
      replacement = quote(foo(new = ))
    ))
  }

  expect_error(
    foo(),
    "`foo(old = )` is defunct, use `foo(new = )` instead",
    class = "defunct_error",
    fixed = TRUE
  )
})

test_that("deprecated_warning()", {
  expect_s3_class(deprecated_warning(), "deprecated_warning")

  foo <- function() {
    cnd(deprecated_warning(
      deprecated = quote(foo(old = )),
      replacement = quote(foo(new = ))
    ))
  }

  expect_warning(
    foo(),
    "`foo(old = )` is deprecated, use `foo(new = )` instead",
    class = "deprecated_warning",
    fixed = TRUE
  )
})
