library(testthat)
library(readr)
library(testthat)
library(mrgsim.parallel)
library(dplyr)

x <- test_dir("tests/testthat/")
x <- tibble::as_tibble(x)
x$result <- NULL
x$user <- NULL
x$system <- NULL
x$real <- NULL
write_csv(x = x, file = "inst/docs/tests.csv")

