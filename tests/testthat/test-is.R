test_that("is functions", {
  expect_true(is_condition(simpleCondition("foo")))
  expect_true(is_cnd_condition(cond_cnd_class()))
  expect_true(is_cnd_generator(condition))
  expect_true(is_cnd_function(cond_cnd_class))
  expect_true(is_conditioned_function(cnd))
})
