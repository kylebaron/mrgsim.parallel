
.simd <- function(dat,mod,...) {
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
#' @examples
#'
#' mod <- modlib("pk2")
#'
#' data <- expand.ev(amt = seq(10))
#'
#' out <- future_mrgsim_d(mod,data, nchunk = 2)
#'
#' @name parallel_mrgsim_d
#' @export
future_mrgsim_d <- function(mod, data, nchunk = 4, ..., as_list = FALSE) {
  data <- chunk_by_id(data,nchunk)
  pa <- "mrgsolve"
  ans <- future_lapply(
    X = data,
    future.packages = pa,
    mod = mod,
    FUN=.simd
  )
  if(as_list) return(ans)
  return(bind_rows(ans))
}

#' @rdname parallel_mrgsim_d
#' @export
mc_mrgsim_d <- function(mod, data, nchunk = 4, ..., as_list = FALSE) {
  data <- chunk_by_id(data,nchunk)
  if(mc_able) {
    ans <- mclapply(X = data, mod = mod, FUN = .simd)
  } else {
    ans <- lapply(X = data, mod = mod, FUN = .simd)
  }
  if(as_list) return(ans)
  return(bind_rows(ans))
}




