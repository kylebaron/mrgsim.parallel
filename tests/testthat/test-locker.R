library(testthat)

context("locker")

test_that("set up locker", {
  unlink(temp_ds("foo"), recursive = TRUE)
  x <- setup_locker(tempdir(), "foo")
  expect_length(x, 1)
  expect_is(x, "character")
})

test_that("reset locker", {
  unlink(temp_ds("foo"), recursive = TRUE)
  x <- setup_locker(tempdir(), "foo")
  cat("...", file = file.path(x, "foo.fst"))
  y <- reset_locker(file.path(tempdir(), "foo"))
  expect_equal(basename(x), "foo")
  expect_true(is.null(y))
  expect_length(list.files(x), 0)
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

test_that("version a locker", {
  locker <- temp_ds("foo")  
  if(dir.exists(locker)) unlink(locker, recursive = TRUE)
  new_locker <- temp_ds("foo-v33")
  if(dir.exists(new_locker)) unlink(new_locker, recursive = TRUE)
  x <- setup_locker(locker)
  cat("...", file = file.path(locker, "1-1.fst"))
  x <- version_locker(locker, version = "v33")
  expect_true(dir.exists(x))
  expect_error(
    version_locker(locker, version = "v33"), 
    regexp = "A directory already exists"
  )
  unlink(x, recursive = TRUE)
  x <- version_locker(locker, version = "v33", overwrite = TRUE)
  expect_true(dir.exists(x))
  x <- version_locker(locker, version = "v33", overwrite = TRUE, noreset = TRUE)
  expect_false(mrgsim.parallel:::is_locker_dir(x))
})
