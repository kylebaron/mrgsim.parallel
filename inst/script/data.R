library(dplyr)

id <- seq(10000)

data <- mrgsolve::expand.ev(amt = 100, ii = 24, addl = 27, ID = id)

saveRDS(file = "inst/datasets/data_big.RDS", data)

saveRDS(file = "inst/datasets/data.RDS", filter(data, ID <=3000))



nn <- 10000
idata <- dplyr::tibble(
  ID = seq(nn),
  CL = exp(rnorm(nn, log(1),  sqrt(0.1))),
  VC = exp(rnorm(nn, log(20), sqrt(0.04))),
  WT = exp(rnorm(nn, log(80), sqrt(1)))
)

saveRDS(file = "inst/datasets/idata_big.RDS", idata)
saveRDS(file = "inst/datasets/idata.RDS", filter(idata, ID <= 3000))