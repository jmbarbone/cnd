test_that("cnd_message() works", {
  foo <- condition("foo", type = "message", register = FALSE)

  expect_message(cnd_message(foo(), "simple"), class = "cnd::condition")
  expect_snapshot(cnd_message(foo(), "simple"))

  expect_message(cnd_message(foo(), "verbose"), class = "cnd::condition")
  expect_snapshot(cnd_message(foo(), "verbose"))
})

test_that("cnd_condition() works", {
  foo <- condition("foo", type = "condition", register = FALSE)

  expect_output(cnd_condition(foo(), "simple"), class = "cnd::condition")
  expect_snapshot(cnd_condition(foo(), "simple"))

  expect_output(cnd_condition(foo(), "verbose"), class = "cnd::condition")
  expect_snapshot(cnd_condition(foo(), "verbose"))
})
