
n_of_N <- function(i) {
  pad <- ceiling(log10(max(i))) + 1
  mx <- max(i)
  i <- formatC(i, width = pad, flag = "0")
  mx <- formatC(mx, width = pad, flag = "0")
  paste0(i, "-", mx)  
}

make_output_files <- function(n) {
  i <- seq_len(n)
  stems <- n_of_N(i)
  files <- paste0(stems, "-bg.fst")
  files
}

setup_locker <- function(locker, tag, n) {
  will_save <- is.character(locker) && length(locker)==1
  output_paths <- vector(mode = "list", length = n)
  if(is.character(locker)) {
    output_folder <- file.path(locker,tag)
    output_files <- make_output_files(n)
    output_paths <- file.path(output_folder, output_files)
    
    if(!dir.exists(locker)) {
      dir.create(locker, recursive = TRUE)  
    }
    if(dir.exists(output_folder)) {
      unlink(output_folder, recursive = TRUE)  
    }
    dir.create(output_folder)
    
    for(path in output_paths) {
      write_fst(tibble(ID = 0)[0,], path = path)
    }
  }  
  if(will_save) {
    output_paths <- structure(output_paths, class = c("will_save", "list"))  
  } 
  output_paths
}

#' List all fst files in a directory
#' 
#' 
#' @param dir the directory to search
#' @export
list_fst <- function(dir) {
  list.files(
    dir, 
    full.names = TRUE, 
    pattern = "*\\.fst$"
  )
}

#' Get the head of an fst file set
#' 
#' 
#' @param dir the directory to search
#' @param n number of rows to show
#' @param i which output output chunk to show 
#' @export
head_fst <- function(dir, n = 5, i = 1) {
  x <- list_fst(dir)
  read_fst(x[i], from = 1, to = n)
}

#' Run mrgsim in the background
#' 
#' This function uses [callr::r_bg()] to run your simulation in the background, 
#' optionally in parallel and optionally saving the results directly to 
#' disk in the fst format. 
#' 
#' @inheritParams parallel_mrgsim_d
#' 
#' @param mod a model object
#' @param ... arguments passed to [mrgsolve::mrgsim()]
#' @param .plan passed to [future::plan()]; this controls how the problem is 
#' parallelized when `nchunk` is greater than 1
#' @param .path a directory for saving simulated data; use this to collect 
#' results from several different runs in a single folder
#' @param .tag a name to use for the current run; results are saved under 
#' `.tag` in `.path` folder
#' @param .wait if `FALSE`, the function returns immediately; if `TRUE`, then 
#' wait until the background job is finished
#' @param .seed numeric; passed to [future.apply::future_mapply()] as 
#' `future.seed`, but only numeric values are accepted
#' 
#' @details 
#' [bg_mrgsim_d()] returns a [processx::process] object (follow that link to 
#' see a list of methods). You will have to call `process$get_result()` to 
#' retrieve the result. When an output `.path` is not specified, simulated 
#' results are returned; when an output `.path` is specified, the path to 
#' the `fst` file on disk is returned.  The `fst` files  should be read with 
#' [fst::read_fst()]. When the results are not saved to `.path`, you will 
#' get a single data frame when `nchunk` is 1 or a list of data frames when 
#' `nchunk` is greater than 1. It is safest to call [dplyr::bind_rows()] or 
#' something equivalent on the result if you are expecting data frame.
#' 
#' @examples
#' mod <- mrgsolve::house(delta = 24, end = 168)
#' data <- mrgsolve::expand.ev(
#'   amt =c(100, 300, 450), 
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
#'   ) 
#'  process$get_result()
#'   
#' @return 
#' An `r_process` object; see [callr::r_bg()]. Call `process$get_resuilt()` to 
#' get the actual result (see `details`).
#' 
#' @export
bg_mrgsim_d <- function(mod, data, nchunk = 1,   
                        ..., 
                        .plan = future::plan(),
                        .path = NULL, .tag = NULL, 
                        .wait = FALSE, .seed = FALSE) {
  
  if(is.data.frame(data)) {
    if(nchunk <= 1) {
      data <- list(data)
    } else {
      data <- chunk_by_id(data, nchunk = nchunk)      
    }
  }
  
  if(is.null(.tag)) {
    .tag <- mod@model
  }
  stopifnot(is.character(.tag))
  stopifnot(length(.tag)==1)
  
  output_paths <- setup_locker(.path, .tag, n = length(data))
  
  if(length(data)==1) {
    func <- bg_mrgsim_d_impl
    args <- list(...)
    args$mod <- mod
    args$data <- data[[1]]
    args$output <- output_paths[[1]]
    args$.seed <- .seed
  } else {
    func <- bg_mrgsim_apply
    args <- list()
    args$more <- list(mod = mod, ...)
    args$output <- output_paths
    args$data <- data
    args$.plan <- .plan
    args$.seed <- .seed
  }

  a <- r_bg(func, args = args, package = TRUE)
  if(isTRUE(.wait)) {
    a$wait()  
  }
  a
}

bg_mrgsim_apply <- function(data, .plan, more, output, .seed = FALSE, ...) {
  options(future.fork.enable = TRUE)
  future::plan(.plan)  
  future.apply::future_mapply(
    FUN = bg_mrgsim_d_impl, 
    data = data,
    output = output,
    MoreArgs = more,
    SIMPLIFY = FALSE, 
    future.seed = .seed
  )  
}

bg_mrgsim_d_impl <- function(data, mod, output = NULL, .seed = NULL,  ...) {
  if(is.numeric(.seed)) set.seed(.seed)
  out <- mrgsim(mod, data, ..., output = "df")
  if(is.null(output)) return(out)
  fst::write_fst(path = output, x = out)
  return(output)
}
