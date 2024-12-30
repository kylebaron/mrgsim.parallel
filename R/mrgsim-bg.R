
bg_sim_env <- function() {
  c(RSTUDIO="0", rcmd_safe_env())  
}

#' Run mrgsim in the background
#' 
#' This function uses [callr::r_bg()] to simulate a dataset in the background, 
#' optionally in parallel and optionally saving the results directly to 
#' disk in  `fst`, `arrow` or `rds` format. Parallelization can be mediated 
#' by the `parallel` package on unix or macos or `future` on any os. 
#' 
#' [bg_mrgsim_d()] returns a [processx::process] object (follow that link to 
#' see a list of methods). You will have to call `process$get_result()` to 
#' retrieve the result. When an output `.locker` is not specified, simulated 
#' data are returned; when an output `.locker` is specified, the path to 
#' the `fst` file on disk is returned.  The `fst` files  should be read with 
#' [fst::read_fst()]. When the results are not saved to `.locker`, you will 
#' get a single data frame when `nchunk` is 1 or a list of data frames when 
#' `nchunk` is greater than 1. It is safest to call [dplyr::bind_rows()] or 
#' something equivalent on the result if you are expecting data frame.
#' 
#' @inheritParams parallel_mrgsim_d
#' 
#' @param mod A model object.
#' @param ... Arguments passed to [mrgsolve::mrgsim()].
#' @param .locker A directory for saving simulated data; use this to collect 
#' results from several different runs in a single folder.
#' @param .tag A name to use for the current run; results are saved under 
#' `.tag` in `.locker` folder.
#' @param .format The output format for saving simulations; using format
#' `fst` will allow saved results to be read with [fst::read_fst()]; using
#' format `arrow` will allow saved results to be read with 
#' [arrow::open_dataset()] with `format = "feather"` or `format = "parquet"`; 
#' note that `fst` is installed with `mrgsim.parallel` but `arrow` may need 
#' explicit installation.
#' @param .wait If `FALSE`, the function returns immediately; if `TRUE`, then 
#' wait until the background job is finished.
#' @param .seed A `numeric` value used to set the seed for the simulation; 
#' this is the only way to control the random number generation for your 
#' simulation.
#' @param .cores The number of cores to parallelize across; pass 1 to run the 
#' simulation sequentially.
#' @param .plan The name of a [future::plan()] strategy; if passed, the 
#' parallelization will be handled by the `future` package.
#' 
#' @examples
#' mod <- mrgsolve::house(delta = 24, end = 168)
#' data <- mrgsolve::expand.ev(
#'   amt = c(100, 300, 450), 
#'   ID = 1:100, 
#'   ii = 24, 
#'   addl = 6
#' )
#' data <- dplyr::mutate(data, dose = amt)
#' process <- bg_mrgsim_d(
#'   mod, 
#'   data, 
#'   carry_out = "dose", 
#'   outvars = "CP",
#'   .wait = TRUE
#' ) 
#' process$get_result()
#'  
#'  
#' ds <- file.path(tempdir(), "sims")
#' files <- bg_mrgsim_d(
#'   mod, data, carry_out = "dose", 
#'   .wait = TRUE, 
#'   .locker = ds,
#'   .format = "fst"
#' )
#' files
#' sims <- internalize_fst(ds)
#' head(sims)
#'   
#' 
#' @return 
#' An `r_process` object; see [callr::r_bg()]. Call `process$get_result()` to 
#' get the actual result (see `details`). If a `.locker` path is supplied, 
#' the simulated data is saved to disk and a list of file names is returned. 
#' 
#' @seealso [future_mrgsim_d()], [internalize_fst()], [list_fst()], 
#' [head_fst()], [setup_locker()]
#' 
#' @export
bg_mrgsim_d <- function(mod, data, nchunk = 1,   
                        ..., 
                        .locker = NULL, .tag = NULL, 
                        .format = c("fst", "feather", "parquet", "rds"),
                        .wait = TRUE, .seed = FALSE, 
                        .cores = 1, .plan = NULL) {
  
  .format <- match.arg(.format)
  .path <- NULL  
  Plan <- list(workers = .cores)
  create_locker <- is.character(.locker)
  
  if(is.character(.plan)) {
    Plan$strategy <- .plan
    if(.cores==1) Plan$workers <- NULL
  }
  if(create_locker) {
    .path <- .locker
    if(is.character(.tag)) {
      .path <- file.path(.locker, .tag)
    }
  }
  if(is.data.frame(data)) {
    if(nchunk <= 1) {
      data <- list(data)
    } else {
      data <- chunk_by_id(data, nchunk = nchunk)      
    }
  }
  if(!is.list(data)) {
    stop("`data` didn't resolve to list format.")  
  }
  
  if(create_locker) {
    ext <- ifelse(substr(.format, 1, 1)=='.', .format, paste0(".", .format))
    locker_loc <- setup_locker(dirname(.path), basename(.path))
    output_paths <- file_set(
      n = length(data),
      prefix = "bg", 
      ext = ext, 
      where = locker_loc
    )
  } else {
    output_paths <- vector(mode = "list", length = length(data))  
  }
 
  if(length(data)==1) {
    func <- bg_mrgsim_d_impl
    args <- list(...)
    args$mod <- mod
    args$data <- data[[1]]
    args$output <- output_paths[[1]]
    args$.seed <- .seed
    args$.format <- .format
  } else {
    func <- bg_mrgsim_apply
    args <- list()
    args$more <- list(mod = mod, ...)
    args$output <- output_paths
    args$data <- data
    args$Plan <- Plan
    args$.seed <- .seed
    args$.format <- .format
  }
  a <- r_bg(func, args = args, package = TRUE, env = bg_sim_env())
  if(isTRUE(.wait)) {
    a$wait()  
  }
  a
}

bg_mrgsim_apply <- function(data, .plan, more, output, .seed = FALSE, 
                            .format = "none", Plan = list(), ...) {
  
  future <- "strategy" %in% names(Plan)
  mc <- is.numeric(Plan$workers) && Plan$workers > 1
  
  if(mc && !future && mc_able()) {
    set.seed(.seed, kind = "L'Ecuyer-CMRG")
    ans <- mcmapply(
      FUN = bg_mrgsim_d_impl,
      data = data,
      output = output,
      MoreArgs = more,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE,
      .format = .format, 
      mc.cores = Plan$workers
    )
    return(ans)    
  }
  
  if(future) {
    do.call(plan, Plan)
    ans <- future_mapply(
      FUN = bg_mrgsim_d_impl,
      data = data,
      output = output,
      MoreArgs = more,
      SIMPLIFY = FALSE,
      USE.NAMES = FALSE,
      future.seed = .seed,
      .format = .format
    )
    return(ans)
  }
  
  set.seed(.seed, kind = "L'Ecuyer-CMRG")
  ans <- mapply(
    FUN = bg_mrgsim_d_impl,
    data = data,
    output = output,
    MoreArgs = more,
    SIMPLIFY = FALSE,
    USE.NAMES = FALSE,
    .format = .format
  )
  return(ans) 
}

bg_mrgsim_d_impl <- function(data, mod, output = NULL, .seed = NULL, 
                             .format = "none",  ...) {
  if(is.numeric(.seed)) set.seed(.seed, kind = "L'Ecuyer-CMRG")
  out <- mrgsim(mod, data, ..., output = "df")
  if(is.null(output)) return(out)
  if(.format == "fst") {
    write_fst(x = out, path = output)
    return(output)
  }
  if(.format == "feather") { #nocov start
    arrow::write_feather( 
      x = out,
      sink = output
    )
    return(output)
  } 
  if(.format == "parquet") { #nocov start
    arrow::write_parquet( 
      x = out,
      sink = output
    )
    return(output)
  }  
  saveRDS(object = out, file = output)
  return(output) #nocov end
}
