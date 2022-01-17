library(testthat)

context("locker")

test_that("set up locker", {
  x <- setup_locker(tempdir(), "foo", n = 3)
  expect_length(x, 3)
  expect_equal(basename(dirname(x[[3]])), "foo")
  expect_equal(basename(dirname(dirname(x[[3]]))), basename(tempdir()))
})
