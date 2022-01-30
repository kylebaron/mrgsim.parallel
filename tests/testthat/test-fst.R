library(testthat)

context("test-fst")

test_that("save to fst", {
  df <- data.frame(a = seq(26), b = letters)
  data <- list(df, df, df)
  unlink(temp_ds("test-fst"), recursive = TRUE)
  x <- new_stream(data, locker = temp_ds("test-fst"), format = "fst")
  ans <- lapply(x, function(x) write_stream(x, x$x))

  lst <- list_fst(temp_ds("test-fst"))
  expect_length(lst, 3)
  expect_equal(basename(lst[[3]]), "3-3.fst")
  
  hd <- head_fst(temp_ds("test-fst"))
  expect_identical(hd, df[1:5,])

  ans1 <- as.data.frame(internalize_fst(temp_ds("test-fst")))
  rownames(ans1) <- NULL
  ans2 <- as.data.frame(do.call(rbind, data))
  rownames(ans2) <- NULL
  expect_equal(ans1, ans2)
})
