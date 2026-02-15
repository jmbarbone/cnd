test_that("printing snapshots", {
  reg <- local_registry()
  expect_snapshot(condition)
  expect_snapshot(cnd_class_error)
  expect_snapshot(cnd_class_error())
  expect_snapshot(condition_overwrite_warning)

  old <- condition("snapshot_test_old", package = "cnd:testing", registry = reg)
  new <- condition("snapshot_test_new", package = "cnd:testing", registry = reg)
  expect_snapshot(condition_overwrite_warning(old, new))

  fun <- function() NULL
  conditions(fun) <- condition(
    "snapshot_test_fun",
    package = "test-snapshots",
    register = FALSE
  )
  expect_snapshot(fun, transform = scrub_environment_code)

  # has a line return
  expect_snapshot(condition_message_error)
})

test_that("printing with cli", {
  override_cli("on", {
    expect_snapshot(condition)
  })
})
