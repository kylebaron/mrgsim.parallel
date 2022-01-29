# nocov start
check_loaded <- function(mod) {
  mod@package %in% names(getLoadedDLLs())  
}

dll_exists <- function(mod) {
  file.exists(as.list(mod)[["sodll"]])
}

#' Run mrgsim after trying to load the shared object
#' 
#' Use this function when running mrgsolve while parallelizing on a 
#' multisession worker node where the model dll might not be loaded.
#' 
#' @param mod a model object
#' @param ... passed to [mrgsolve::mrgsim()]
#' 
#' @examples
#' 
#' mrgsim_worker(mrgsolve:::house())
#' 
#' @export
mrgsim_ms <- function(mod, ...) {
  if(inherits(mod, "packmod")) {
    return(mrgsim(mod,...))  
  }
  if(!check_loaded(mod)) {
    if(!dll_exists(mod)) {
      stop("the model dll could not be found", call. = FALSE)  
    }
    loadso(mod)
    if(!check_loaded(mod)) {
      stop("the model dll could not be loaded", call. = FALSE)  
    }
  }
  mrgsim(mod,...)
}

#' @rdname mrgsim_ms
#' @export
mrgsim_worker <- mrgsim_ms

#nocov end