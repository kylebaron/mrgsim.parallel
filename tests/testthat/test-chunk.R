library(testthat)

context("chunk data sets")

data <- expand.grid(amt = seq(25))
data$ID  <- seq(nrow(data))
data2 <- data
data2[["SUBJ"]] <- data2[["ID"]]

idata <- expand.grid(CL = runif(36, 0.5, 1.5))
idata2 <- expand.grid(CL = runif(40, 0.5, 1.5))

idata$ID <- seq(nrow(idata))
idata2$ID <- seq(nrow(idata2))

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
  expect_identical(length(x), 10L)
  
  x <- chunk_by_row(idata2, nchunk = 10)
  expect_identical(length(x), 10L)
  
  x <- chunk_by_row(idata2, nchunk = 2, mark = "test")
  expect_true(exists("test", x[[2]]))
  expect_true(all(x[[2]]["test"]==2))
  
  set.seed(12345)
  sam <- seq(10)
  nch <- 7
  ltrs <- sample(letters)
  ids <- unlist(sapply(ltrs, function(x) rep(x, each = sample(sam, 1))))
  data <- data.frame(ID = ids, stringsAsFactors = FALSE, row.names = NULL)
  spl <- chunk_by_id(data, nchunk = nch)
  data2 <- as.data.frame(do.call(rbind, spl))
  rownames(data2) <- NULL
  expect_equal(data, data2)
  
  set.seed(36912)
  ltrs <- sample(letters)
  ech <- 5
  nch <- 13
  ids <- rep(sample(ltrs), each = ech)
  data <- data.frame(ID = ids, stringsAsFactors = FALSE, row.names = NULL)
  spl <- chunk_by_id(data, nchunk = nch)
  uni <- unique(vapply(spl, nrow, 1L))
  expect_equal(uni, ech*length(ltrs)/nch)
  data2 <- as.data.frame(do.call(rbind, spl))
  rownames(data2) <- NULL
  expect_equal(data, data2)
})

test_that("chunk data by multiple cols", {
  data <- data.frame(a = c(rep("a", 3), rep("b", 4)), 
                     b = c(rep("a", 4), rep("b", 2), "c"))
  chunked <- chunk_by_cols(data, nchunk = 3, cols = c("a", "b"))
  expect_equal(length(chunked), 3)
  tot <- sum(vapply(chunked, nrow, 1L))
  expect_equal(tot, nrow(data))
  
  set.seed(98765)
  nch <- 11
  data <- expand.grid(A = 1:6, B = letters[3:10], C = LETTERS[12:14], D = 5)
  data$N <- seq(nrow(data))
  sp1 <- chunk_by_row(data, nchunk = nch)
  sp2 <- chunk_by_cols(data, cols = c("A", "B", "C"), nchunk = nch)
  x <- do.call(rbind, sp1)
  y <- do.call(rbind, sp2)
  expect_identical(x, y)
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
