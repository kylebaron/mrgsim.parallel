library(testthat)

context("background simulation")

mod <- mrgsolve::modlib("popex", delta = 24, end = 168)
data <- mrgsolve::expand.ev(
  amt =c(100, 300, 450), 
  ID = 1:5, 
  ii = 24, 
  addl = 6
)
data$dose <- data$amt

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

test_that("save results as fst", {
  locker <- tempdir()
  dataset <- file.path(locker, "A1")
  bg <- bg_mrgsim_d(
    mod, 
    data, 
    carry_out = "dose", 
    outvars = "DV",
    .wait = TRUE, 
    .seed = 123256L, 
    nchunk = 2, 
    .ncores = 1,
    .dataset = dataset
  )
  sims <- internalize_fst(dataset)
  expect_is(sims, "data.frame")
  expect_equal(names(sims), c("ID", "time", "dose", "DV"))
  files <- list.files(dataset)
  expect_equal(files[1:2], c("bg-1-2.fst", "bg-2-2.fst"))
  files <- list.files(dataset, all.files=TRUE)
  expect_match(files, ".mrgsim-parallel-locker-dir.", fixed = TRUE, all = FALSE)
})

test_that("error when saving to existing directory", {
  locker <- tempdir()
  dataset <- file.path(locker, "A1123")
  dir.create(dataset)
  expect_error(
    bg <- bg_mrgsim_d(
      mod, 
      data, 
      .dataset = dataset
    ), 
    regexp = "the dataset directory exists"
  )
})

