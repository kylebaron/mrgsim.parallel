
# mrgsolve.parallel

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mrgsolve/mrgsolve.parallel.svg?branch=master)](https://travis-ci.org/mrgsolve/mrgsolve.parallel)
<!-- badges: end -->

Any speed-up will be highly problem-dependent.

## Overview

parallel simulation with mrgsolve in R

## Backend

``` r
library(future)

library(mrgsolve.parallel)

options(future.fork.enable=TRUE, mc.cores = 8L)

plan(multiprocess,workers=8L)
```

## Data set

``` r
mod <- modlib("pk2cmt", end = 168*8, delta = 1)

data <- expand.ev(amt = 100*seq(1,3000), ii = 24, addl = 27*2+2) 

system.time(ans <- future_mrgsim_d(mod, data, nchunk = 8L))
```

    .    user  system elapsed 
    .  16.390   2.696   3.549

``` r
system.time(ans <- mc_mrgsim_d(mod, data, nchunk = 8L))
```

    .    user  system elapsed 
    .  13.918   1.490   2.623

``` r
system.time(ans <- mrgsim_d(mod,data))
```

    .    user  system elapsed 
    .   8.297   0.273   8.589
