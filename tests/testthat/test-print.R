test_that("printing snapshots", {
  expect_snapshot(condition)
  expect_snapshot(cond_cnd_class)
  expect_snapshot(cond_cnd_class())
  expect_snapshot(cond_condition_overwrite)
  expect_snapshot(cond_condition_overwrite(cond_cnd_class))
})
