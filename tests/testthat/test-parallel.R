library(testthat)

context("parallel simulation")

mod <- suppressMessages(mrgsolve::modlib("pk1", end = 3))

data <- expand.ev(amt = seq(25))
data2 <- data
data2[["SUBJ"]] <- data2[["ID"]]

e <- ev(amt = 100)

idata <- expand.idata(CL = runif(36, 0.5, 1.5))
idata2 <- expand.idata(CL = runif(40, 0.5, 1.5))

test_that("sim data", {
  out <- mrgsim_d(mod, data, output="df")
  out2 <- future_mrgsim_d(mod,data)
  expect_identical(out,out2)
  
  out3 <- mc_mrgsim_d(mod,data)
  expect_identical(out,out3)
  expect_is(future_mrgsim_d(mod,data,.as_list=TRUE), "list")
  expect_is(mc_mrgsim_d(mod,data,.as_list=TRUE), "list")
})

test_that("sim idata", {
  out <- mrgsim_ei(mod, e, idata, output="df")
  out2 <- future_mrgsim_ei(mod, e, idata)
  expect_identical(out,out2)
  out3 <- mc_mrgsim_ei(mod, e, idata)
  expect_identical(out,out3)
  expect_is(future_mrgsim_ei(mod, e, idata,.as_list=TRUE), "list")
  expect_is(mc_mrgsim_ei(mod, e, idata,.as_list=TRUE), "list")
})

test_that("sim with nchunk=1", {
  data1 <- chunk_by_id(data,nchunk=1)
  outa <- fu_mrgsim_d(mod,data1)
  outb <- mrgsim_d(mod,data,output="df")
  expect_identical(outa,outb)
  idata1 <- chunk_by_row(idata,nchunk=1)
  outa <- mrgsim_ei(mod,e,idata,output="df")
  outb <- fu_mrgsim_ei(mod,e,idata1)
  expect_identical(outa,outb)
})

test_that("sim dry run", {
  expect_is(fu_mrgsim_d(mod,data,.dry = TRUE),"data.frame")
  expect_is(fu_mrgsim_ei(mod,e,idata,.dry = TRUE), "data.frame")
})

test_that("sim post processing function", {
  post <- function(sims,mod) {sims[,"post_add"] <- 1; sims}
  out1 <- fu_mrgsim_d(mod,data,nchunk=4)
  out2 <- fu_mrgsim_d(mod,data,nchunk=4,.p=post)
  expect_true(exists("post_add", out2))
  expect_true(!exists("post_add", out1))
  expect_true(all(out2[["post_add"]]==1))
})

test_that("sim pass in chunked data", {
  ch <- chunk_by_id(data,4)
  out1 <- fu_mrgsim_d(mod,data, nchunk=4)
  out2 <- fu_mrgsim_d(mod,ch)
  expect_identical(out1,out2)
})

test_that("sim reproducible results", {
  mod <- mrgsolve::modlib("popex", end = 6)
  data <- mrgsolve::expand.ev(amt = 100, ID = 1:4)
  set.seed(11223)
  out1 <- fu_mrgsim_d(mod,data)
  set.seed(11223)
  out2 <- fu_mrgsim_d(mod,data)
  set.seed(11221)
  out3 <- fu_mrgsim_d(mod,data)
  expect_identical(out1,out2)
  expect_false(identical(out1,out3))
})
