
# Workflow for event and idata ------------------------------

.simi <- function(idat, ...) {
  loadso(mod)
  mrgsim_ei(mod, event, idat, ..., output="df")
}
#' Simulate an idata set in parallel
#'
#' Use [future_mrgsim_ei] to simulate with the `future` package.  Use
#' [mc_mrgsim_ei] to simulate with [parallel::mclapply].
#'
#' @param mod mrgsolve model object
#' @param event a mrgsolve event object
#' @param idata `idata` set to simulate
#' @param nchunk number of chunks in which to split the data set
#' @param ... passed to [mrgsim_ei]
#' @param as_list if `TRUE` a list is return; otherwise (default) a data frame
#'
#' @return A data frame or list of simulated data
#'
#' @name parallel_mrgsim_ei
#' @export
future_mrgsim_ei <- function(mod, event, idata, nchunk = 4, ..., as_list=FALSE) {
  idata <- chunk_by_row(idata,nchunk)
  gl <- list(mod = mod, event = event)
  pa <- "mrgsolve"
  ans <- future_lapply(
    X=idata,
    future.globals = gl, future.packages = pa,
    FUN=.simi
  )
  if(as_list) return(ans)
  bind_rows(ans)
}

#' @rdname parallel_mrgsim_ei
#' @export
mc_mrgsim_ei <- function(mod, event, idata, nchunk = 4, ..., as_list = FALSE) {
  if(!mc_able) {
    stop("mclapply cannot be used on this system; use future_mrgsim_d instead",call.=FALSE)
  }
  idata <- chunk_by_row(idata,nchunk)
  ans <- mclapply(X=idata, FUN = .simi)
  if(as_list) return(ans)
  return(bind_rows(ans))
}

