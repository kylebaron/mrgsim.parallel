
# Workflow for event and idata ------------------------------

.simi <- function(idat, mod, event, ...) {
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
#' @examples
#'
#' mod <- modlib("pk2")
#'
#' event <- ev(amt = 100)
#'
#' idata <- data.frame(CL = runif(10, 0.5, 1.5))
#'
#' out <- future_mrgsim_ei(mod, event, idata)
#'
#' @name parallel_mrgsim_ei
#' @export
future_mrgsim_ei <- function(mod, event, idata, nchunk = 4, ..., as_list=FALSE) {
  idata <- chunk_by_row(idata,nchunk)
  pa <- "mrgsolve"
  ans <- future_lapply(
    X=idata,
    future.packages = pa,
    mod = mod,
    event = event,
    FUN=.simi
  )
  if(as_list) return(ans)
  bind_rows(ans)
}

#' @rdname parallel_mrgsim_ei
#' @export
mc_mrgsim_ei <- function(mod, event, idata, nchunk = 4, ..., as_list = FALSE) {
  idata <- chunk_by_row(idata,nchunk)
  if(mc_able) {
    ans <- mclapply(X=idata, mod = mod, event = event, FUN = .simi)
  } else {
    ans <- lapply(X=idata, mod = mod, event = event, FUN = .simi)
  }
  if(as_list) return(ans)
  return(bind_rows(ans))
}

