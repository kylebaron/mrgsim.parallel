library(testthat)

context("mrgsim ms")

test_that("mrgsim after load", {
  mod <- mrgsolve:::house()
  out1 <- mrgsolve:::mrgsim(mod)
  out2 <- mrgsim_ms(mod)
  expect_identical(out1,out2)
})
