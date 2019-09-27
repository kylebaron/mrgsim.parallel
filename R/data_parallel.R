

.simd <- function(dat,...) {
  loadso(mod);
  mrgsim_d(mod,dat,...,output="df")
}


#' Simulate a data set in parallel
#'
#' Use [future_mrgsim_d] to simulate with the `future` package.  Use
#' [mc_mrgsim_d] to simulate with [parallel::mclapply].
#'
#' @param mod mrgsolve model object
#' @param data data set to simulate
#' @param nchunk number of chunks in which to split the data set
#' @param ... passed to [mrgsim_d]
#' @param as_list if `TRUE` a list is return; otherwise (default) a data frame
#'
#' @return A data frame or list of simulated data
#'
#' @name parallel_mrgsim_d
#' @export
future_mrgsim_d <- function(mod, data, nchunk = 4, ..., as_list = FALSE) {
  data <- chunk_by_id(data,nchunk)
  pa <- "mrgsolve"
  gl <- list(mod = mod)
  ans <- future_lapply(
    X = data,
    future.globals = gl, future.packages = pa,
    FUN=.simd
  )
  if(as_list) return(ans)
  return(bind_rows(ans))
}

#' @rdname parallel_mrgsim_d
#' @export
mc_mrgsim_d <- function(mod, data, nchunk = 4, ..., as_list = FALSE) {
  if(!mc_able) {
    stop("mclapply cannot be used on this system; use future_mrgsim_d instead",call.=FALSE)
  }
  data <- chunk_by_id(data,nchunk)
  ans <- mclapply(X = data,  FUN = .simd)
  if(as_list) return(ans)
  return(bind_rows(ans))
}




