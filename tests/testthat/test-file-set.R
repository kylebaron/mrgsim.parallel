library(testthat)

context("file set")

test_that("generate file set list", {
  x <- file_set(10, tag = "foo", ext = ".bar")
  expect_length(x, 10)
  expect_equal(x[[5]]$i, 5)
  expect_equal(x[[8]]$file, "foo-08-10.bar")
  x <- file_set(1, where = tempdir())
  expect_equal(dirname(x[[1]]$file), tempdir())
})

