test_that("printing snapshots", {
  expect_snapshot(condition)
  expect_snapshot(cond_cnd_class)
  expect_snapshot(cond_cnd_class())
  expect_snapshot(cond_condition_overwrite)

  old <- condition("snapshot_test_old", package = "cnd:testing")
  new <- condition("snapshot_test_new", package = "cnd:testing")
  expect_snapshot(cond_condition_overwrite(old, new))
})
