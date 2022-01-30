do_bg_mclapply <- function(X, FUN, mc.cores = 1, seed = NULL, ...) { #nocov start
  if(is.numeric(seed)) {
    set.seed(seed, kind="L'Ecuyer-CMRG")  
  }
  if(mc_able()) {
    mclapply(X, FUN, mc.cores = mc.cores, ...)
  } else {
    lapply(X, FUN, ...)
  }
}

#' Multicore lapply in the background
#' 
#' @inheritParams bg_mrgsim_d
#' @param X A list. 
#' @param FUN The function to be applied to each element of `X`.
#' @param mc.cores Passed to [parallel::mclapply()].
#' @param ... Arguments passed to `FUN`.
#' 
#' @examples
#' ans <- bg_mclapply(seq(10), sqrt, mc.cores = 2)
#' 
#' @return
#' A list of output data.
#' 
#' @export
bg_mclapply <- function(X, FUN, mc.cores = 1, ..., .wait = TRUE, .seed = NULL) {
  args <- list(...)
  args$X <- X
  args$FUN <- FUN
  args$mc.cores <- mc.cores
  args$seed <- .seed
  ans <- r_bg(
    func = do_bg_mclapply,
    args = args,
    env = c(RSTUDIO="0", rcmd_safe_env()),
    package = TRUE
  )
  if(.wait) ans$wait()
  ans$get_result()
} # nocov end
