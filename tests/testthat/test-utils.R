test_that("utils works", {
  expect_identical(
    get_package(environment(get_package)),
    "cnd"
  )
})

test_that("class functions", {
  obj <- list()
  class(obj) <- c("a", "a", "b")
  expect_identical(add_class(obj, "a"), obj)
  expect_identical(add_class(obj, "c"), `class<-`(obj, c("c", "a", "a", "b")))
  expect_identical(remove_class(obj, "a"), `class<-`(obj, "b"))
})

test_that("clean_padding()", {
  expect_identical(clean_padding("foo\nbar\n"), c("foo", "bar"))

  expect_identical(
    clean_padding(c("  one", "  two"), "--"),
    c("--one", "--two")
  )
})
