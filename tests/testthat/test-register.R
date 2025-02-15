test_that("registration works", {
  reg <- local_registry()
  foo <- condition(
    "foo",
    package = "cnd:test",
    registry = reg,
    register = FALSE
  )

  # manually registering
  expect_identical(registrar$register(foo, registry = reg), foo)
  expect_null(registrar$register(foo, registry = reg))

  foo2 <- condition(
    "foo",
    "new",
    package = "cnd:test",
    registry = reg,
    register = FALSE
  )
  # warning for overwriting
  expect_warning(
    registrar$register(foo2, registry = reg),
    class = "cnd:condition_overwrite"
  )
  expect_null(registrar$unregister(foo2, reg))

  foobar <- condition("foobar", package = NULL, register = FALSE)
  expect_null(registrar$register(foobar))

  expect_error(find_cond("foobar"), class = "simpleError")
})

test_that("cnd_create_registry()", {
  reg <- cnd_create_registry("test", name = ".__cnd__.")
  expect_identical(reg, get(".__cnd__."))
  registrar$remove(reg)
})

test_that("print(registrar)", {
  expect_snapshot(registrar)
})

test_that("print(registry) with new registries", {
  reg <- local_registry("test-register-snaps")
  conditions("foo", registry = reg)
  conditions("foo", registry = reg)
  expect_snapshot(reg)
})
