library(testthat)

context("bbackground simulation")

mod <- mrgsolve::modlib("popex", delta = 24, end = 168)
data <- mrgsolve::expand.ev(
  amt =c(100, 300, 450), 
  ID = 1:100, 
  ii = 24, 
  addl = 6
)
data <- dplyr::mutate(data, dose = amt)

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
