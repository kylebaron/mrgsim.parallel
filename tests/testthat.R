
Sys.setenv("R_TESTS" = "")

library(testthat)
library(mrgsolve.fu)
test_check("mrgsolve.fu", reporter="summary")


