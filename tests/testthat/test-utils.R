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
  expect_identical(clean_padding("one\ntwo\n"), c("one", "two"))
  expect_identical(clean_padding(" one\n  two"), c("one", " two"))
})

test_that("clean_text() strips leading/trailing blank lines", {
  expect_identical(
    clean_text("\nfoo\nbar\n\n\n"),
    c("foo", "bar")
  )
})

test_that("rcode()", {
  expect_type(rcode("one"), "character")
  expect_snapshot(
    cat(rcode("expect_snapshot(rcode)"))
  )
})
