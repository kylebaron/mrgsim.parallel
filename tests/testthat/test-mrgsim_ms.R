library(testthat)

context("mrgsim ms")

test_that("mrgsim after load", {
  mod <- mrgsolve:::house()
  out1 <- mrgsolve:::mrgsim(mod)
  out2 <- mrgsim_ms(mod)
  expect_identical(out1,out2)
  data <- data.frame(ID = c(1,2,3), EVID = 1, AMT = c(100,200,50), CMT = 1, TIME = 0)
  out1 <- mrgsolve:::mrgsim(mod, data = data)
  out2 <- mrgsim_ms(mod, data = data)
  expect_identical(out1,out2)
})
