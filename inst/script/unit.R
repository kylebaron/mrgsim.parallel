library(testthat)
library(readr)
library(testthat)
library(mrgsim.parallel)
library(dplyr)

x <- test_dir("tests/testthat/")
x <- tibble::as_tibble(x)
x$result <- NULL

write_csv(x = x, file = "inst/docs/tests.csv")

