test_that("cli switching works", {
  expect_identical(override_cli("on", cli_switch(0L, 1L)), 0L)
  expect_identical(override_cli("off", cli_switch(0L, 1L)), 1L)
})
