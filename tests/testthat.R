
Sys.setenv("R_TESTS" = "")

library(testthat)
library(mrgsolve.parallel)
test_check("mrgsolve.parallel", reporter="summary")


