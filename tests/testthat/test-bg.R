library(testthat)
library(dplyr)
library(mrgsim.parallel)

context("background simulation")

mod <- mrgsolve::modlib("popex", delta = 24, end = 168, rtol = 1e-4)
data <- mrgsolve::expand.ev(
  amt = c(100, 300), 
  ID = 1:3, 
  ii = 24, 
  addl = 6
)
data$dose <- data$amt
clean <- function(x) gsub("[[:punct:]]", "-", x, perl = TRUE)

test_that("bg simulation has same result as fg", { 
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV",
    .wait = TRUE , 
    .seed = 1232
  )
  bg <- bind_rows(bg$get_result())
  set.seed(1232, kind = "L'Ecuyer-CMRG")
  fg <- mrgsolve::mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV", 
    output = "df"
  )
  expect_identical(fg, bg)
})

test_that("bg is same as fg, not chunked", { 
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV",
    .wait = TRUE , 
    .seed = 1232
  )
  bg <- bind_rows(bg$get_result())
  set.seed(1232, kind = "L'Ecuyer-CMRG")
  fg <- mrgsolve::mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV", 
    output = "df"
  )
  expect_identical(fg, bg)
})

test_that("bg is same as fg, chunked", { 
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV",
    .wait = TRUE , 
    .seed = 123256L, 
    .plan = "sequential",
    nchunk = 2
  )
  bg <- bind_rows(bg$get_result())
  future::plan(future::sequential)
  fg <- future_mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV", 
    output = "df", 
    nchunk = 2,
    .seed = 123256L
  )
  expect_identical(fg, bg)
})

test_that("bg save results as fst", {
  locker <- tempdir()
  ds <- file.path(locker, "A1")
  if(dir.exists(ds)) unlink(ds, recursive = TRUE)
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .cores = 1,
    .locker = ds
  )
  sims <- internalize_fst(ds)
  expect_is(sims, "data.frame")
  expect_equal(names(sims), c("ID", "time", "dose", "DV"))
  files <- list.files(ds)
  expect_equal(files[1:2], c("bg-1-2.fst", "bg-2-2.fst"))
  files <- list.files(ds, all.files = TRUE, no.. = TRUE)
  target <- mrgsim.parallel:::.locker_file_name
  expect_match(files, target, fixed = TRUE, all = FALSE)
})

test_that("bg error when saving to existing directory", {
  locker <- tempdir()
  ds <- file.path(locker, "A1123")
  if(dir.exists(ds)) unlink(ds, recursive = TRUE)
  dir.create(ds)
  expect_error(
    bg <- bg_mrgsim_d(
      mod, 
      data, 
      .locker = ds
    ), 
    regexp = "the dataset directory exists"
  )
})

test_that("bg simulate parallel", {
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .plan = "sequential"
  )
  df <- bg$get_result()
  expect_is(df, "list")
  expect_length(df, 2)
})

test_that("bg locker, no tag", {
  loc <- file.path(tempdir(), "foo")
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    .locker = loc,
    .format = "fst",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .plan = "sequential"
  )
  files <- bg$get_result()
  expect_equal(
    clean(dirname(files[[1]])), 
    clean(file.path(tempdir(), "foo"))
  )
  bg2 <- bg_mrgsim_d(
    mod, 
    data, 
    .locker = loc,
    .format = "fst",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .plan = "sequential"
  )
  files2 <- bg2$get_result()
  expect_identical(files, files2)
  unlink(temp_ds("foo"), recursive = TRUE)
})

test_that("bg locker and tag", {
  loc <- file.path(tempdir(), "foo")
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    .locker = loc,
    .format = "fst",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .plan = "sequential"
  )
  files <- bg$get_result()[[1]]
  expect_equal(
    clean(dirname(files[1])), 
    clean(file.path(tempdir(), "foo"))
  )
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    .locker = loc,
    .tag = "run2", 
    .format = "fst",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .plan = "sequential"
  )
  files <- bg$get_result()
  expect_equal(
    clean(dirname(files[[1]])), 
    clean(file.path(tempdir(), "foo", "run2"))
  )  
  bg2 <- bg_mrgsim_d(
    mod, 
    data, 
    .locker = loc,
    .tag = "run2", 
    .format = "fst",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .plan = "sequential"
  )
  files2 <- bg2$get_result()
  expect_identical(files, files2)
})
