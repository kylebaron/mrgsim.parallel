
# mrgsolve.fu

<!-- badges: start -->

[![Travis build
status](https://travis-ci.org/mrgsolve/mrgsolve.fu.svg?branch=master)](https://travis-ci.org/mrgsolve/mrgsolve.fu)
<!-- badges: end -->

## Overview

mrgsolve.fu (em-are-gee solve dot few) facilitates parallel simulation
with mrgsolve in R. The future and parallel packages provide the
parallelization.

There are 2 main workflows:

1.  Split a `data_set` into chunks by ID, simulate the chunks in
    parallel, then assemble the results back to a single data frame.
2.  Split an `idata_set` (individual-level parameters) into chunks by
    row, simulate the chunks in parallel, then assemble the results back
    to a single data frame.

The nature of the parallel backend requires some overhead to get the
parallel simulation done. So, it will take a reasonably-sized job to see
a speed increase and small jobs will likely take *longer* with
parallelization. But jobs taking more than a handful of seconds could
benefit from this type of parallelization.

## Backend

``` r
library(dplyr)

library(future)

library(mrgsolve.parallel)

options(future.fork.enable=TRUE, mc.cores = 6L)

plan(multiprocess, workers = 6L)
```

## First workflow: split and simulate a data set

``` r
mod <- modlib("pk2cmt", end = 168*8, delta = 1)

data <- expand.ev(amt = 100*seq(1,2000), ii = 24, addl = 27*2+2) 

data <- mutate(data, CL = runif(n(), 0.7, 1.3))

head(data)
```

    .   ID time amt ii addl cmt evid        CL
    . 1  1    0 100 24   56   1    1 1.0072478
    . 2  2    0 200 24   56   1    1 0.9859220
    . 3  3    0 300 24   56   1    1 1.1771392
    . 4  4    0 400 24   56   1    1 1.1546121
    . 5  5    0 500 24   56   1    1 0.8762627
    . 6  6    0 600 24   56   1    1 1.2346878

``` r
dim(data)
```

    . [1] 2000    8

We can simulate in parallel with the future package or the parallel
package like this:

``` r
system.time(ans1 <- future_mrgsim_d(mod, data, nchunk = 6L))
```

    .    user  system elapsed 
    .  13.017   0.963   3.265

``` r
system.time(ans2 <- mc_mrgsim_d(mod, data, nchunk = 6L))
```

    .    user  system elapsed 
    .  12.630   0.963   2.700

To compare an identical simulation done without parallelization

``` r
system.time(ans3 <- mrgsim_d(mod,data))
```

    .    user  system elapsed 
    .   9.108   0.176   9.299

``` r
identical(ans2,as.data.frame(ans3))
```

    . [1] TRUE

## Second workflow: split and simulate a batch of parameters

Backend and the model

``` r
plan(multiprocess, workers = 6)

mod <- modlib("pk1cmt", end = 168*4, delta = 1)
```

For this workflow, we have a set of parameters (`idata`) along with an
event object that gets applied to all of the parameters

``` r
idata <- tibble(CL = runif(4000, 0.5, 1.5), ID = seq_along(CL))

head(idata)
```

    . # A tibble: 6 x 2
    .      CL    ID
    .   <dbl> <int>
    . 1 1.13      1
    . 2 1.25      2
    . 3 1.47      3
    . 4 1.40      4
    . 5 1.12      5
    . 6 0.779     6

``` r
dose <- ev(amt = 100, ii = 24, addl = 27)

dose
```

    . Events:
    .   time amt ii addl cmt evid
    . 1    0 100 24   27   1    1

Run it in parallel

``` r
system.time(ans1 <- mc_mrgsim_ei(mod, dose, idata, nchunk = 6))
```

    .    user  system elapsed 
    .   8.320   0.892   2.058

And without parallelization

``` r
system.time(ans2 <- mrgsim_ei(mod, dose, idata, output = "df"))
```

    .    user  system elapsed 
    .   6.023   0.178   6.237

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

## Do a dry run to check the overhead of parallelization

``` r
plan(transparent)
system.time(x <- fu_mrgsim_d(mod, data, nchunk = 8, .dry = TRUE))
```

    .    user  system elapsed 
    .   0.015   0.001   0.016

``` r
plan(multiprocess,workers = 8L)
system.time(x <- fu_mrgsim_d(mod, data, nchunk = 8, .dry = TRUE))
```

    .    user  system elapsed 
    .   0.126   0.215   0.166

## Pass a function to post process on the worker

First check the range of times from the previous example

``` r
summary(ans1$time)
```

    .    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    .     0.0   167.0   335.5   335.5   504.0   672.0

The post-processing function has arguments the simulated data and the
model object

``` r
post <- function(sims, mod) {
  filter(sims, time > 600)  
}

dose <- ev(amt = 100, ii = 24, addl = 27)

ans3 <- mc_mrgsim_ei(mod, dose, idata, nchunk = 6, .p = post)
```

``` r
summary(ans3$time)
```

    .    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    .   601.0   618.8   636.5   636.5   654.2   672.0

The main use case here is to summarize or some how decrease the volume
of data before returning the combined simulations. In case memory is
able to handle the simulation volume, this post-processing could be done
on the combined data as well.

<hr>

## More info

See [inst/docs/about.md (on GitHub only)](inst/docs/about.md) for more
details.
