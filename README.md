
# mrgsim.parallel

<!-- badges: start -->
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
parallelization. But jobs taking more than a handful of seconds could
benefit from this type of parallelization.

## Backend

``` r
library(dplyr)

library(future)

library(mrgsim.parallel)

options(future.fork.enable = TRUE, parallelly.fork.enable = TRUE, mc.cores = 4L)
```

## First workflow: split and simulate a data set

``` r
mod <- modlib("pk2cmt", end = 168*8, delta = 1)

data <- expand.ev(amt = 100*seq(1,2000), ii = 24, addl = 27*2+2) 

data <- mutate(data, CL = runif(n(), 0.7, 1.3))

head(data)
```

    .   ID time amt ii addl cmt evid        CL
    . 1  1    0 100 24   56   1    1 1.2621689
    . 2  2    0 200 24   56   1    1 0.7284933
    . 3  3    0 300 24   56   1    1 0.8319464
    . 4  4    0 400 24   56   1    1 0.7376281
    . 5  5    0 500 24   56   1    1 0.8033678
    . 6  6    0 600 24   56   1    1 1.2224112

``` r
dim(data)
```

    . [1] 2000    8

We can simulate in parallel with the future package or the parallel
package like this:

``` r
plan(multisession, workers = 4L)
system.time(ans1 <- future_mrgsim_d(mod, data, nchunk = 4L))
```

    .    user  system elapsed 
    .   0.467   0.170   4.159

``` r
plan(multicore, workers = 4L)
system.time(ans1b <- future_mrgsim_d(mod, data, nchunk = 4L))
```

    .    user  system elapsed 
    .   5.295   0.575   1.880

``` r
system.time(ans2 <- mc_mrgsim_d(mod, data, nchunk = 4L))
```

    .    user  system elapsed 
    .   5.261   0.576   1.765

To compare an identical simulation done without parallelization

``` r
system.time(ans3 <- mrgsim_d(mod,data))
```

    .    user  system elapsed 
    .   5.022   0.104   5.152

``` r
identical(ans2,as.data.frame(ans3))
```

    . [1] TRUE

## Second workflow: split and simulate a batch of parameters

Backend and the model

``` r
plan(multisession, workers = 6)

mod <- modlib("pk1cmt", end = 168*4, delta = 1)
```

For this workflow, we have a set of parameters (`idata`) along with an
event object that gets applied to all of the parameters

``` r
idata <- tibble(CL = runif(4000, 0.5, 1.5), ID = seq_along(CL))

head(idata)
```

    . # A tibble: 6 × 2
    .      CL    ID
    .   <dbl> <int>
    . 1 1.29      1
    . 2 0.612     2
    . 3 0.881     3
    . 4 1.11      4
    . 5 0.837     5
    . 6 1.44      6

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
    .   3.785   0.520   1.545

And without parallelization

``` r
system.time(ans2 <- mrgsim_ei(mod, dose, idata, output = "df"))
```

    .    user  system elapsed 
    .   3.523   0.092   3.635

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

    .    ID time amt ii addl cmt evid
    . 1   1    0 100  0    0   1    1
    . 2   1    0  50 12    2   1    1
    . 3   2    0 100  0    0   1    1
    . 4   2    0  50 12    2   1    1
    . 5   3    0 100  0    0   1    1
    . 6   3    0  50 12    2   1    1
    . 7   4    0 100  0    0   1    1
    . 8   4    0  50 12    2   1    1
    . 9   5    0 100  0    0   1    1
    . 10  5    0  50 12    2   1    1

``` r
chunk_by_id(dose, nchunk = 2)
```

    . $`1`
    .   ID time amt ii addl cmt evid
    . 1  1    0 100  0    0   1    1
    . 2  1    0  50 12    2   1    1
    . 3  2    0 100  0    0   1    1
    . 4  2    0  50 12    2   1    1
    . 5  3    0 100  0    0   1    1
    . 6  3    0  50 12    2   1    1
    . 
    . $`2`
    .    ID time amt ii addl cmt evid
    . 7   4    0 100  0    0   1    1
    . 8   4    0  50 12    2   1    1
    . 9   5    0 100  0    0   1    1
    . 10  5    0  50 12    2   1    1

See also: `chunk_by_row`

## Do a dry run to check the overhead of parallelization

``` r
plan(transparent)
system.time(x <- fu_mrgsim_d(mod, data, nchunk = 8, .dry = TRUE))
```

    .    user  system elapsed 
    .   0.015   0.002   0.016

``` r
plan(multisession, workers = 8L)
system.time(x <- fu_mrgsim_d(mod, data, nchunk = 8, .dry = TRUE))
```

    .    user  system elapsed 
    .   0.049   0.003   5.564

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
