test_that("printing snapshots", {
  reg <- local_registry()
  expect_snapshot(condition)
  expect_snapshot(cond_cnd_class)
  expect_snapshot(cond_cnd_class())
  expect_snapshot(cond_condition_overwrite)

  old <- condition("snapshot_test_old", package = "cnd:testing", registry = reg)
  new <- condition("snapshot_test_new", package = "cnd:testing", registry = reg)
  expect_snapshot(cond_condition_overwrite(old, new))

  fun <- function() NULL
  conditions(fun) <- condition(
    "snapshot_test_fun",
    package = "test-snapshots",
    register = FALSE
  )
  expect_snapshot(fun, transform = scrub_environment_code)

  # has a line return
  expect_snapshot(cond_condition_bad_message)
})

test_that("printing with cli", {
  override_cli("on", {
    expect_snapshot(condition)
  })
})
