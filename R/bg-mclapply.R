do_bg_mclapply <- function(X, FUN, mc.cores = 1, seed = NULL, ...) {
  if(is.numeric(seed)) {
    set.seed(seed, kind="L'Ecuyer-CMRG")  
  }
  mclapply(X, FUN, mc.cores = mc.cores, ...)
}

#' Multicore lapply in the background
#' 
#' @param X A list. 
#' @param FUN The function to be applied to each element of `X`.
#' @param mc.cores Passed to [parallel::mclapply()].
#' @param ... Arguments passed to `FUN`.
#' 
#' @examples
#' ans <- bg_mclapply(seq(100), sqrt, mc.cores = 2)
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
}
