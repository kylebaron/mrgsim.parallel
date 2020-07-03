#' @importFrom future.apply future_lapply
#' @importFrom dplyr bind_rows
#' @importFrom parallel mclapply
#' @importFrom mrgsolve loadso mrgsim_d mrgsim_ei house mrgsim_i
#' @importFrom utils assignInMyNamespace
NULL

#' Simulate with 'mrgsolve' in Parallel
#' 
#' @section Resources:
#' - GitHub: [https://github.com/kylebaron/mrgsim.parallel](https://github.com/kylebaron/mrgsim.parallel)
#' - Docs: [https://kylebaron.github.io/mrgsim.parallel](https://kylebaron.github.io/mrgsim.parallel)
#' 
#' @section Package options:
#' 
#' - `mrgsim.parallel.mc.able`: if `TRUE`, multicore will be used if appropriate.
#' 
#' @name mrgsim.parallel
#' @rdname mrgsim.parallel
NULL

#nocov start
#mc_able <- .Platform$OS.type!="windows"

globalVariables(c("mod", "event"))

# nocov end
