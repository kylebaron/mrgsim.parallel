
.simd <- function(dat,mod,...,.p,.dry) {
  if(!inherits(mod, "packmod")) loadso(mod)
  if(.dry) {
    return(dat)
  } else {
    return(.p(mrgsim_d(mod,dat,...,output="df"),mod))
  }
}

#' Simulate a data set in parallel
#'
#' Use [future_mrgsim_d()] to simulate with the `future` package.  Use
#' [mc_mrgsim_d()] to simulate with `parallel::mclapply`.
#'
#' @param mod The mrgsolve model object see [mrgsolve::mrgmod-class].
#' @param data Data set to simulate; see [mrgsolve::data_set()].
#' @param nchunk Number of chunks in which to split the data set; chunking will 
#' be based on the `ID` column, which is required in `data`.
#' @param ... Passed to [mrgsolve::mrgsim_d()].
#' @param .as_list If `TRUE` a list is return; otherwise (default) a data frame
#' @param .p Post processing function executed on the worker; arguments should
#' be (1) the simulated output (2) the model object.
#' @param .dry If `TRUE` neither the simulation nor the post processing will
#' be done.
#' @param .seed Passed to [future.apply::future_lapply()] as `future.seed`.
#' @param .parallel if `FALSE`, the simulation will not be parallelized; this is
#' intended for debugging and testing use only.
#'
#' @return 
#' A data frame or list of simulated data.
#'
#' @seealso [future_mrgsim_ei()]
#'
#' @examples
#'
#' mod <- mrgsolve::house()
#'
#' data <- mrgsolve::expand.ev(amt = seq(10))
#'
#' out <- future_mrgsim_d(mod, data, nchunk = 2)
#'
#' @name parallel_mrgsim_d
#' @export
future_mrgsim_d <- function(mod, data, nchunk = 4, ..., .as_list = FALSE,
                            .p = NULL, .dry = FALSE, .seed = TRUE, 
                            .parallel = TRUE) {
  if(!inherits(data,"list")) data <- chunk_by_id(data,nchunk)
  pa <- "mrgsolve"
  if(!is.function(.p)) .p <- .nothing
  if((length(data)==1)) {
    return(.simd(data[[1]],mod,...,.p=.p,.dry=.dry))
  }
  if(isTRUE(.parallel)) {
    ans <- future_lapply(
      X = data,
      future.packages = pa,
      future.globals = character(0),
      future.seed = .seed,
      mod = mod,
      .p = .p,
      .dry = .dry,
      FUN = .simd, 
      ...
    )
  } else {
    ans <- lapply(X = data, mod = mod, .p = .p, .dry = .dry, FUN = .simd,...) #nocov
  }
  if(.as_list) return(ans)
  return(bind_rows(ans))
}

#' @rdname parallel_mrgsim_d
#' @export
mc_mrgsim_d <- function(mod, data, nchunk = 4, ..., .as_list = FALSE,
                        .p = NULL, .dry = FALSE, .seed = NULL, 
                        .parallel = TRUE) {
  if(!inherits(data,"list")) data <- chunk_by_id(data,nchunk)
  if(!is.function(.p)) .p <- .nothing
  if((length(data)==1)) {
    return(.simd(data[[1]],mod,...,.p=.p,.dry=.dry))
  }
  if(mc_able() & isTRUE(.parallel)) {
    ans <- mclapply(X = data, mod = mod, .p = .p, .dry = .dry, FUN = .simd, ...)
  } else { 
    ans <- lapply(X = data, mod = mod, .p = .p, .dry = .dry, FUN = .simd,...) #nocov
  }
  if(.as_list) return(ans)
  return(bind_rows(ans))
}

#' @rdname parallel_mrgsim_d
#' @export
fu_mrgsim_d <- future_mrgsim_d #nocov

#' @rdname parallel_mrgsim_d
#' @export
fu_mrgsim_d0 <- function(...,.dry=TRUE) fu_mrgsim_d(...,.dry=TRUE) #nocov
