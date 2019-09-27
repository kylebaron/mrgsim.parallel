context("basic functionality")


mod <- suppressMessages(mrgsolve::modlib("pk1", end = 3))

data <- expand.ev(amt = seq(25))
data2 <- data
data2[["SUBJ"]] <- data2[["ID"]]
data2[["ID"]] <- NULL

e <- ev(amt = 100)

idata <- expand.idata(CL = runif(36, 0.5, 1.5))
idata2 <- expand.idata(CL = runif(40, 0.5, 1.5))

test_that("chunk_data", {
  x <- chunk_by_id(data, nchunk = 5)
  expect_identical(length(x), 5L)

  x2 <- chunk_by_id(data2, nchunk = 5, id_col = "SUBJ")
  expect_identical(length(x), 5L)

  x <- chunk_by_row(idata, nchunk = 10)
  expect_identical(length(x), 9L)

  x <- chunk_by_row(idata2, nchunk = 10)
  expect_identical(length(x), 10L)

})

test_that("sim data", {
  out <- mrgsim_d(mod, data, output="df")
  out2 <- future_mrgsim_d(mod,data)
  expect_identical(out,out2)

  out3 <- mc_mrgsim_d(mod,data)
  expect_identical(out,out3)
  expect_is(future_mrgsim_d(mod,data,as_list=TRUE), "list")
  expect_is(mc_mrgsim_d(mod,data,as_list=TRUE), "list")
})

test_that("sim idata", {
  out <- mrgsim_ei(mod, e, idata, output="df")
  out2 <- future_mrgsim_ei(mod, e, idata)
  expect_identical(out,out2)
  out3 <- mc_mrgsim_ei(mod, e, idata)
  expect_identical(out,out3)
  expect_is(future_mrgsim_ei(mod, e, idata,as_list=TRUE), "list")
  expect_is(mc_mrgsim_ei(mod, e, idata,as_list=TRUE), "list")
})


