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

test_that("create new stream from data.frame", {
  a <- expand.grid(ID = 1:50, B = c(2,3))
  a$ID <- seq(nrow(a))
  x <- new_stream(a, 5)
  expect_equal(length(x), 5)
  expect_is(x[[4]]$x, "data.frame")
  expect_equal(nrow(x[[4]]$x), 20)
  expect_is(x, "file_stream")
  expect_is(x, "list")  
  expect_is(x[[1]], "list")
  expect_equal(x[[1]]$file, "1-5")
  expect_error(
    new_stream(a, 101),
    regexp="`x` must have >= `nchunk` rows", 
    fixed = TRUE
  )
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
  x <- ext_stream(x, ".feather")
  expect_equal(x[[2]]$file, "02-11.feather")
})

test_that("relocate stream ", {
  x <- new_stream(2)
  x <- locate_stream(x, temp_ds("kyle"))
  test <- basename(dirname(x[[1]]$file))
  expect_equal(test, "kyle")
})

test_that("relocate and initialize stream", {
  x <- new_stream(2)
  dir <- file.path(tempdir(), "test-relocate-init")
  if(dir.exists(dir)) unlink(dir, recursive = TRUE)
  expect_false(dir.exists(dir))
  x <- locate_stream(x, where = dir, initialize = TRUE)
  expect_true(dir.exists(dir))
  expect_true(file.exists(file.path(dir, mrgsim.parallel:::.locker_file_name)))
  y <- new_stream(3)
  dir <- file.path(tempdir(), "test-relocate-init-2")
  dir.create(dir)
  expect_error( 
    locate_stream(y, where = dir, initialize = TRUE), 
    regexp="the dataset directory exists, but doesn't appear"
  )
  unlink(dir, recursive = TRUE)
})

test_that("create new stream with locker", {
  unlink(temp_ds("foo"), recursive = TRUE)
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
    format_stream(x, "rds", warn = TRUE), 
    regexp = "format was set, but file name [1] has no directory specified.", 
    fixed = TRUE
  )
})

test_that("writer function - rds", {
  unlink(temp_ds("write/rds"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/rds"), format = "rds")
  expect_true(write_stream(x[[1]], mtcars))
  mt <- readRDS(x[[1]]$file)
  expect_equal(mt, mtcars)
})

test_that("writer function - fst", {
  skip_if_not_installed("fst")
  expect_true(mrgsim.parallel:::fst_installed())
  unlink(temp_ds("write/fst"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/fst"), format = "fst")
  expect_true(write_stream(x[[1]], mtcars))
  mt <- fst::read_fst(x[[1]]$file)
  expect_equivalent(mt, mtcars)
  expect_error(
    write_stream(x[[1]], list(mtcars)), 
    regexp="must be a data.frame"
  )
})

test_that("writer function - qs", {
  skip_if_not_installed("qs")
  expect_true(mrgsim.parallel:::qs_installed())
  unlink(temp_ds("write/qs"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/qs"), format = "qs")
  expect_true(write_stream(x[[1]], mtcars))
  mt <- qs::qread(x[[1]]$file)
  expect_equivalent(mt, mtcars)
})

test_that("writer function - feather", {
  skip_if_not_installed("arrow")
  expect_true(mrgsim.parallel:::arrow_installed())
  unlink(temp_ds("write/arrow"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/arrow"), format = "feather")
  expect_true(write_stream(x[[1]], mtcars))
  mt <- arrow::read_feather(x[[1]]$file)
  expect_equivalent(mt, mtcars)
  expect_error(
    write_stream(x[[1]], list(mtcars)), 
    regexp="must be a data.frame"
  )
})

test_that("writer function - default", {
  unlink(temp_ds("write/default"), recursive = TRUE)
  x <- new_stream(1, locker = temp_ds("write/default"), ext = "rds")
  expect_false(write_stream(x[[1]], mtcars))
})
