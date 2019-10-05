
# mrgsolve.fu

## User Stories Overview

1. I want to split a data set by `ID` and simulate the chunks in parallel
1. I want to simulate a batch of model parameters with a single event object
   in parallel
1. I want to chunk a data frame by row or by a column

## 1 - Simulate a data set in parallel

Issue: #4

- Split a data set by a single column and simulate in parallel
- The mrgsolve simulation function is `mrgsim_d`

### Tests

- sim data

## 2 - Simulate an idata set with event in parallel

Issue: #5

- Split a data set by a single column and simulate in parallel
- The mrgsolve simulation function is `mrgsim_d`

### Tests

- sim idata

## 3 - Common to both data and idata simulation

Issue: #4, #5

- Parallel back end includes `future.apply::future_lapply` and 
   `parallel::mclapply`
- For future backend the number of workers is set with `plan` and for 
  parallel backend the number of workers is set with `options(mc.cores)`
- Results are bound up into a single data frame by default or return as a list
  of chunked simulation results by argument 
- Data frames can be pre-chunked by the user and passed as a list
- A dry run maybe perfomed by argument
- A post-processing function may be passed along to the simulation function; 
  simulated data gets processed through the post function prior to returning
  from the worker
- Additional arguments get passed allong to the simulation function as `...`
- When parallel (`mclapply`) backend is selected on a windows system, the 
  simulation is carried out sequentially (with `lapply`)
- Globals are prevented from getting passed to the worker when simulating 
  with the future backend
  
### Tests

- dry run
- pass in chunked data
- reproducible results
- sim data
- sim idata
  
## 3 - Chunk a data frame

Issue: #6

- The data frame can get chunked by row
- The data frame can get chunked by the value in a single column
- The chunk number can optionally get attached to each chunk

### Tests
- chunk data
- bad input

# Coverage
See [inst/covr/coverage.md](../covr/coverage.md)
