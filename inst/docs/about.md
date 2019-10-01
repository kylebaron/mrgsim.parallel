
# mrgsolve.fu

## User Stories

1. I want to split a data set by `ID` and simulate the chunks in parallel.
1. I want to simulate a batch of model parameters with a single event object
   in parallel.
1. I want to post-process my simulated data on the worker.
   
## Summary of requirements

1. Parallel back ends include `future.apply::future_lapply` and 
   `parallel::mclapply`.
1. The user can simulate form one of two workflows:
    - Split a `data_set` by `ID` and simulate the chunks
    - Split an `idata_set` by row and simulate the chunks with an event 
      object
1. For both workflows, the user can pass in the data set and specify the number 
   of chunks at runtime; otherwise, and list of custom-chunked data can be passed
1. Arguments can be passed to the mrgsolve functions (according to workflow) 
   through `...`
1. For future backend the number of workers is set with `plan` and for 
   parallel backend the number of workers is set with `options(mc.cores)`
1. Chunks are bound together at the end of the simulation unless the user 
   requests the lists of chunks
1. Chunking functions are available to split by row or by a single column
1. When chunking a data frame the integer chunk number can be optionally added
   to the data frame with user-specified column name
1. Globals are prevented from getting passed to the worker when simulating 
   with the future backend
1. When parallel (`mclapply`) backend is selected on a windows system, the 
   simulation is carried out sequentially (with `lapply`)
1. A post-processing function can be passed to the worker to process simulated
   data there
1. An argument (`.dry`) can be used to bypass the simulation and post-processing
   step in order to evaluate the overhead of parallelization; in this case, 
   the input data set is returned

# Coverage
See [inst/covr/coverage.md](../covr/coverage.md)



