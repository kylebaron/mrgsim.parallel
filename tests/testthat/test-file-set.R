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

test_that("generate file set names", {
  x <- file_set(150, tag = "foo", file_only = TRUE)
  expect_length(x, 150)
  expect_is(x, "character")
  expect_equal(x[[122]], "foo-122-150")
})
