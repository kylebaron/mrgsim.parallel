library(testthat)

context("locker")

test_that("set up locker", {
  x <- setup_locker(tempdir(), "foo", n = 3)
  expect_length(x, 3)
  expect_equal(basename(dirname(x[[3]])), "foo")
  expect_equal(basename(dirname(dirname(x[[3]]))), basename(tempdir()))
})

test_that("warn if directory isn't empty on reset", {
  unlink(temp_ds("foo"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("foo"))
  cat(letters, file = file.path(temp_ds("foo"), "letters.txt"))
  expect_warning(
    new_stream(2, locker = temp_ds("foo")), 
    regexp="Could not clear locker directory"
  )
})


