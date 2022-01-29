library(testthat)

context("file set")

test_that("generate file set list", {
  x <- file_set(10, prefix = "foo", ext = ".bar")
  expect_length(x, 10)
  expect_equal(x[[8]], "foo-08-10.bar")
  path <- normalizePath(tempdir(), winslash="/")
  x <- file_set(1, where = path)
  expect_equal(basename(dirname(x[[1]])), basename(path))
})

test_that("generate file set names", {
  x <- file_set(150, prefix = "foo")
  expect_length(x, 150)
  expect_is(x, "character")
  expect_equal(x[[122]], "foo-122-150")
})

test_that("create file stream", {
  ans <- file_stream(n = 3, ext = ".abc")
  expect_length(ans, 3)
  expect_equal(ans[[3]]$i, 3)
  expect_equal(ans[[2]]$file, "2-3.abc")
  expect_error(file_stream(n=0), regexp = "must be a positive numeric value")
})
