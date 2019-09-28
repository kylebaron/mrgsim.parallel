
# mrgsolve.parallel

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mrgsolve/mrgsolve.parallel.svg?branch=master)](https://travis-ci.org/mrgsolve/mrgsolve.parallel)
<!-- badges: end -->

## Overview

mrgsolve.parallel facilitates parallel simulation with mrgsolve in R.
The future and parallel packages provide the parallelization.

There are 2 main workflows:

1.  Split a `data_set` into chunks by ID, simulate the chunks in
    parallel, then assemble the results back to a single data frame.
2.  Split an `idata_set` (individual-level parameters) into chunks by
    row, simulate the chunks in parallel, then assemble the results back
    to a single data frame.

The nature of the parallel backend requires some overhead to get the
parallel simulation done. So, it will take a reasonably-sized job to see
a speed increase and small jobs will likely take *longer* with
parallelization. The overhead will have less of an impact as the job
gets bigger and very large jobs (taking 20 or 60 minutes … or longer)
will make the overhead look negligible.

## Backend

``` r
library(dplyr)

library(future)

library(mrgsolve.parallel)

options(future.fork.enable=TRUE, mc.cores = 8L)

plan(multiprocess,workers=8L)
```

## First workflow: split and simulate a data set

``` r
mod <- modlib("pk2cmt", end = 168*8, delta = 1)

data <- expand.ev(amt = 100*seq(1,2000), ii = 24, addl = 27*2+2) 

data <- mutate(data, CL = runif(n(), 0.7, 1.3))

head(data)
```

    .   ID time amt ii addl cmt evid        CL
    . 1  1    0 100 24   56   1    1 0.8647896
    . 2  2    0 200 24   56   1    1 1.1437083
    . 3  3    0 300 24   56   1    1 0.7104544
    . 4  4    0 400 24   56   1    1 0.9833409
    . 5  5    0 500 24   56   1    1 0.8224065
    . 6  6    0 600 24   56   1    1 1.2895639

``` r
dim(data)
```

    . [1] 2000    8

We can simulate in parallel with the future package or the parallel
package like this:

``` r
system.time(ans <- future_mrgsim_d(mod, data, nchunk = 8L))
```

    .    user  system elapsed 
    .   9.555   1.112   2.403

``` r
system.time(ans <- mc_mrgsim_d(mod, data, nchunk = 8L))
```

    .    user  system elapsed 
    .   9.238   1.032   1.808

To compare an identical simulation done without parallelization

``` r
system.time(ans <- mrgsim_d(mod,data))
```

    .    user  system elapsed 
    .   5.334   0.177   5.516

## Second workflow: split and simulate a batch of parameters

Backend and the model

``` r
plan(multiprocess, workers = 6)

mod <- modlib("pk1cmt", end = 168*4, delta = 0.5)
```

For this workflow, we have a set of parameters (`idata`) along with an
event object that gets applied to all of the parameters

``` r
idata <- tibble(CL = runif(8000, 0.5, 1.5), ID = seq_along(CL))

head(idata)
```

    . # A tibble: 6 x 2
    .      CL    ID
    .   <dbl> <int>
    . 1 0.600     1
    . 2 0.649     2
    . 3 0.570     3
    . 4 0.747     4
    . 5 1.28      5
    . 6 1.07      6

``` r
dose <- ev(amt = 100, ii = 24, addl = 27)

dose
```

    . Events:
    .   time amt ii addl cmt evid
    . 1    0 100 24   27   1    1

Run it

``` r
system.time(ans1 <- mrgsim_ei(mod, dose, idata, output="df"))
```

    .    user  system elapsed 
    .   8.381   0.561   8.952

``` r
system.time(ans2 <- mc_mrgsim_ei(mod, dose, idata, nchunk = 8))
```

    .    user  system elapsed 
    .  14.695   3.140   3.599

``` r
identical(ans1,ans2)
```

    . [1] TRUE

## Utility functions

You can access the chunking functions for your own parallel workflows

``` r
dose <- ev_seq(ev(amt = 100), ev(amt = 50, ii = 12, addl = 2))
dose <- ev_rep(dose, 1:5)

dose
```

    .    time amt ii addl cmt evid ID
    . 1     0 100  0    0   1    1  1
    . 2     0  50 12    2   1    1  1
    . 3     0 100  0    0   1    1  2
    . 4     0  50 12    2   1    1  2
    . 5     0 100  0    0   1    1  3
    . 6     0  50 12    2   1    1  3
    . 7     0 100  0    0   1    1  4
    . 8     0  50 12    2   1    1  4
    . 9     0 100  0    0   1    1  5
    . 10    0  50 12    2   1    1  5

``` r
chunk_by_id(dose, nchunk = 2)
```

    . $`1`
    .   time amt ii addl cmt evid ID
    . 1    0 100  0    0   1    1  1
    . 2    0  50 12    2   1    1  1
    . 3    0 100  0    0   1    1  2
    . 4    0  50 12    2   1    1  2
    . 5    0 100  0    0   1    1  3
    . 6    0  50 12    2   1    1  3
    . 
    . $`2`
    .    time amt ii addl cmt evid ID
    . 7     0 100  0    0   1    1  4
    . 8     0  50 12    2   1    1  4
    . 9     0 100  0    0   1    1  5
    . 10    0  50 12    2   1    1  5

See also: `chunk_by_row`
