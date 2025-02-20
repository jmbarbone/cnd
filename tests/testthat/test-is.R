test_that("is functions", {
  expect_true(is_condition(simpleCondition("foo")))
  expect_true(is_cnd_condition(cond_cnd_class()))
  expect_true(is_cnd_progenerator(condition))
  expect_true(is_cnd_generator(cond_cnd_class))
  expect_true(is_conditioned_function(cnd))
})
