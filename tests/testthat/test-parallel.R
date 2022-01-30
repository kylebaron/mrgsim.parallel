library(testthat)

context("basic functionality")


mod <- suppressMessages(mrgsolve::modlib("pk1", end = 3))

data <- expand.ev(amt = seq(25))
data2 <- data
data2[["SUBJ"]] <- data2[["ID"]]

e <- ev(amt = 100)

idata <- expand.idata(CL = runif(36, 0.5, 1.5))
idata2 <- expand.idata(CL = runif(40, 0.5, 1.5))

test_that("chunk data", {
  x <- chunk_by_id(data, nchunk = 5)
  expect_identical(length(x), 5L)
  
  x2 <- chunk_by_id(data2, nchunk = 5, id_col = "SUBJ")
  expect_identical(length(x), 5L)
  
  x2 <- chunk_by_id(data2, nchunk = 5, mark = "test")
  expect_true(exists("test", x2[[3]]))
  expect_true(all(x[[4]][["test"]]==4))
  
  x <- chunk_by_row(data2, nchunk = 3, mark = "test")
  expect_true(exists("test", x[[3]]))
  expect_true(all(x[[3]]["test"]==3))
  
  x <- chunk_by_row(idata, nchunk = 10)
  expect_identical(length(x), 9L)
  
  x <- chunk_by_row(idata2, nchunk = 10)
  expect_identical(length(x), 10L)
  
  x <- chunk_by_row(idata2, nchunk = 2, mark = "test")
  expect_true(exists("test", x[[2]]))
  expect_true(all(x[[2]]["test"]==2))
  
})

test_that("chunk bad input", {
  expect_error(chunk_by_id(list(), 5))
  expect_error(chunk_by_id(matrix(0), 5))
  expect_error(chunk_by_id(data, 0))
  expect_error(chunk_by_id(data, "A"))
  expect_error(chunk_by_id(data, "kyletbaron"))
  expect_error(chunk_by_id(data, 26))
  expect_is(chunk_by_id(data,25),"list")
  expect_error(chunk_by_row(list(), 5))
  expect_error(chunk_by_row(matrix(0), 5))
  expect_error(chunk_by_row(data, 0))
  expect_error(chunk_by_row(data, "A"))
  expect_error(chunk_by_row(data, 99))
  expect_is(chunk_by_row(data,25), "list")
  expect_error(chunk_by_id(data,4,id_col="FOO"))
})

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
