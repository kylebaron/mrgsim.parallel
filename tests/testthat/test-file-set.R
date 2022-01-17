library(testthat)

context("file set")

test_that("generate file set list", {
  x <- file_set(10, tag = "foo", ext = ".bar")
  expect_length(x, 10)
  expect_equal(x[[5]]$i, 5)
  expect_equal(x[[8]]$file, "foo-08-10.bar")
  path <- normalizePath(tempdir(), winslash="/")
  x <- file_set(1, where = path)
  expect_equal(basename(dirname(x[[1]]$file)), basename(path))
})

test_that("generate file set names", {
  x <- file_set(150, tag = "foo", file_only = TRUE)
  expect_length(x, 150)
  expect_is(x, "character")
  expect_equal(x[[122]], "foo-122-150")
})

test_that("create object stream", {
  data <- letters
  ans <- object_stream(letters, ext = "-hij")
  expect_length(ans, 26)
  check <- ans[[4]]
  expect_equal(check$i, 4)
  expect_equal(check$x, "d")
  expect_equal(check$file, "04-26-hij")
  expect_error(object_stream(list()), regexp = "must have length >= 1")
})

test_that("create file stream", {
  ans <- file_stream(n = 3, ext = ".abc")
  expect_length(ans, 3)
  expect_equal(ans[[3]]$i, 3)
  expect_equal(ans[[2]]$file, "2-3.abc")
  expect_error(file_stream(n=0), regexp = "must be >= 1")
})
