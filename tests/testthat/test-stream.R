library(testthat)

context("test-stream")

test_that("create new stream", {
  x <- new_stream(5)
  expect_equal(length(x), 5)
  expect_is(x, "file_stream")
  expect_is(x, "list")  
  expect_is(x[[1]], "list")
  expect_equal(x[[3]]$file, "3-5")
})

test_that("create new stream from list", {
  a <- list(mtcars, mtcars, mtcars)
  x <- new_stream(a)
  expect_equal(length(x), 3)
  expect_identical(x[[2]]$x, mtcars)
  expect_is(x, "file_stream")
  expect_is(x, "list")  
  expect_is(x[[1]], "list")
  expect_equal(x[[1]]$file, "1-3")
})

test_that("create new stream from character", {
  x <- new_stream(letters[1:5])
  expect_equal(length(x), 5)
  expect_identical(x[[4]]$x, "d")
  expect_is(x, "file_stream")
  expect_is(x, "list")  
  expect_is(x[[1]], "list")
  expect_equal(x[[1]]$file, "1-5")
})

test_that("create new stream with ext", {
  x <- new_stream(5, ext = ".bar")
  expect_equal(x[[3]]$file, "3-5.bar")
})

test_that("add ext to stream ", {
  x <- new_stream(11)
  x <- ext_stream(x, "feather")
  expect_equal(x[[2]]$file, "02-11.feather")
})

test_that("relocate stream ", {
  x <- new_stream(2)
  x <- locate_stream(x, temp_ds("kyle"))
  test <- basename(dirname(x[[1]]$file))
  expect_equal(test, "kyle")
})

test_that("create new stream with locker", {
  x <- new_stream(5, locker = temp_ds("foo"))
  expect_is(x, "locker_stream")
  expect_is(x, "file_stream")
  expect_is(x, "list")
  tst <- basename(dirname(x[[3]]$file))
  expect_equal(tst, "foo")
})

test_that("create new stream with locker and format", {
  x <- new_stream(5, locker = temp_ds("bar"), format = "fst")  
  expect_is(x, "locker_stream")
  expect_is(x, "file_stream")
  expect_is(x, "list")
  expect_is(x[[1]], "stream_format_fst")
  expect_is(x[[1]], "list")
})

test_that("add format to stream with locker", {
  x <- new_stream(5, locker = temp_ds("bar"))
  x <- format_stream(x, "rds")
  expect_is(x, "locker_stream")
  expect_is(x, "file_stream")
  expect_is(x, "list")
  expect_is(x[[1]], "stream_format_rds")
  expect_is(x[[1]], "list")
})

test_that("add format to stream without locker", {
  x <- new_stream(5)
  expect_warning(
    format_stream(x, "rds"), 
    regexpp = "format was set, but file name [1] has no directory specified"
  )
})

test_that("writer function - rds", {
  unlink(temp_ds("write/rds"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/rds"), format = "rds")
  write_stream(x[[1]], mtcars)
  mt <- readRDS(x[[1]]$file)
  expect_equal(mt, mtcars)
})

test_that("writer function - fst", {
  skip_if_not_installed("fst")
  unlink(temp_ds("write/fst"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/fst"), format = "fst")
  write_stream(x[[1]], mtcars)
  mt <- fst::read_fst(x[[1]]$file)
  expect_equivalent(mt, mtcars)
  expect_error(
    write_stream(x[[1]], list(mtcars)), 
    regexp="must be a data.frame"
  )
})

test_that("writer function - qs", {
  skip_if_not_installed("qs")
  unlink(temp_ds("write/qs"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/qs"), format = "qs")
  write_stream(x[[1]], mtcars)
  mt <- qs::qread(x[[1]]$file)
  expect_equivalent(mt, mtcars)
})

test_that("writer function - feather", {
  skip_if_not_installed("arrow")
  unlink(temp_ds("write/arrow"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/arrow"), format = "feather")
  write_stream(x[[1]], mtcars)
  mt <- arrow::read_feather(x[[1]]$file)
  expect_equivalent(mt, mtcars)
  expect_error(
    write_stream(x[[1]], list(mtcars)), 
    regexp="must be a data.frame"
  )
})
