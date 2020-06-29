
Sys.setenv("R_TESTS" = "")

library(testthat)
library(mrgsim.parallel)
test_check("mrgsim.parallel", reporter="summary")


