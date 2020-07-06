
# Workflow for event and idata ------------------------------

.simi <- function(idat,mod,events,.p,.dry,...) {
  if(!inherits(mod,"packmod")) loadso(mod)
  if(.dry) {
    return(idat)
  } else {
    if(is.null(events)) {
      return(.p(mrgsim_i(mod,idata=idat,...,output="df"),mod))  
    } else {
      return(.p(mrgsim_ei(mod,events=events,idata=idat,...,output="df"),mod)) 
    }
  }
}

.nothing <- function(sims,mod) sims

#' Simulate an idata set in parallel
#'
#' Use [future_mrgsim_ei] to simulate with the `future` package.  Use
#' [mc_mrgsim_ei] to simulate with `parallel::mclapply`.
#'
#' @inheritParams parallel_mrgsim_d
#' @param events an event object from mrgsolve; see [mrgsolve::ev]
#' @param idata an idata set of parameters, one per simulation unit (individual);
#' see [mrgsolve::idata_set]
#'
#' @return A data frame or list of simulated data
#'
#' @examples
#'
#' mod <- mrgsolve::house()
#'
#' events <- mrgsolve::ev(amt = 100)
#'
#' idata <- data.frame(CL = runif(10, 0.5, 1.5))
#'
#' out <- future_mrgsim_ei(mod, events, idata)
#'
#' @seealso [future_mrgsim_ei]
#'
#' @name parallel_mrgsim_ei
#' @export
future_mrgsim_ei <- function(mod, events, idata, nchunk = 4, ..., .as_list=FALSE,
                             .p  = NULL, .dry = FALSE, .seed = TRUE, 
                             .parallel = TRUE) {
  
  if(!inherits(idata,"list")) idata <- chunk_by_row(idata,nchunk)
  if(!is.function(.p)) .p <- .nothing
  pa <- "mrgsolve"
  if(length(idata)==1) {
    return(.simi(idata[[1]],mod,events,...,.p=.p,.dry=.dry))
  }
  if(isTRUE(.parallel)) {
    ans <- future_lapply(
      X = idata,
      future.packages = pa,
      future.globals = character(0),
      future.seed = .seed,
      mod = mod,
      events = events,
      .p = .p,
      .dry = .dry,
      FUN=.simi, 
      ...
    ) 
  } else {
    ans <- lapply( #nocov start
      X = idata, mod = mod, events = events, .p = .p, .dry = .dry,
      FUN = .simi, 
      ...
    ) #nocov end
  }
  if(.as_list) return(ans)
  bind_rows(ans)
}

#' @rdname parallel_mrgsim_ei
#' @export
fu_mrgsim_ei <- future_mrgsim_ei #nocov

#' @rdname parallel_mrgsim_ei
#' @export
fu_mrgsim_ei0 <- function(...,.dry=TRUE) fu_mrgsim_ei(...,.dry = TRUE) #nocov

#' @rdname parallel_mrgsim_ei
#' @export
mc_mrgsim_ei <- function(mod, events, idata, nchunk = 4, ..., .as_list = FALSE,
                         .p = NULL, .dry = FALSE, .seed=NULL, 
                         .parallel = TRUE) {
  
  if(!inherits(idata,"list")) idata <- chunk_by_row(idata,nchunk)
  if(!is.function(.p)) .p <- .nothing
  if(length(idata)==1) {
    return(.simi(idata[[1]],mod,events,...,.p=.p,.dry=.dry))
  }
  if(mc_able() & isTRUE(.parallel)) {
    ans <- mclapply(
      X = idata, mod = mod, events = events, .p = .p, .dry = .dry,
      FUN = .simi, ... 
    )
  } else {
    ans <- lapply( #nocov start
      X = idata, mod = mod, events = events, .p = .p, .dry = .dry,
      FUN = .simi, 
      ...
    ) #nocov end
  }
  if(.as_list) return(ans)
  return(bind_rows(ans))
}
