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

test_that("retire a locker", {
  locker <- temp_ds("foo")
  unlink(locker, recursive = TRUE)
  x <- new_stream(5, locker = locker)
  x <- new_stream(5, locker = locker)
  x <- new_stream(5, locker = locker)
  expect_true(noreset_locker(locker))
  cat("foo", file = file.path(locker, 'foo.fst'))
  expect_error(
    new_stream(5, locker = locker), 
    regexp = "but doesn't appear to be a valid locker"
  )
  expect_equal(list.files(locker), "foo.fst")
})
