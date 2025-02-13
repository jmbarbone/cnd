test_that("registration works", {
  remove_registration("test-register")
  on.exit(remove_registration("test-register"))

  foo <- condition("foo", package = "test-register", register = FALSE)
  expect_null(register_condition(foo))
  expect_null(register_condition(foo))

  foo2 <- condition("foo", "new", package = "test-register", register = FALSE)
  expect_warning(register_condition(foo2), class = "cnd:condition_overwrite")
  expect_null(unregister_condition(foo2))

  expect_null(
    register_condition(
      condition("foobar", package = NULL, register = FALSE)
    )
  )

  # TODO expect custom condition
  expect_error(find_cond("foobar"), class = "simpleError")
})
