
bg_sim_env <- function() {
  c(RSTUDIO="0", rcmd_safe_env())  
}

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
  files <- paste0(stems, "-bg.")
  files
}

setup_locker <- function(locker, tag, n, .format = "fst") {
  will_save <- is.character(locker) && length(locker)==1
  output_paths <- vector(mode = "list", length = n)
  if(is.character(locker)) {
    output_folder <- file.path(locker,tag)
    output_files <- make_output_files(n)
    output_files <- paste0(output_files, .format)
    output_paths <- file.path(output_folder, output_files)
    if(!dir.exists(locker)) {
      dir.create(locker, recursive = TRUE)  
    }
    if(dir.exists(output_folder)) {
      unlink(output_folder, recursive = TRUE)  
    }
    dir.create(output_folder)
    if(.format == "fst") {
      for(path in output_paths) {
        write_fst(tibble(ID = 0)[0,], path = path)
      }
    }
    if(.format == "feather") {
      for(path in output_paths) {
        arrow::write_feather(tibble(ID = 0)[0,], sink = path)
      }
    }
  }  
  if(will_save) {
    output_paths <- structure(output_paths, class = c("will_save", "list"))  
  } 
  output_paths
}

#' List all output files in a fst file set
#' 
#' Use the funcation to read all of the `.fst` files that were saved when 
#' `bg_mrgsim_d` was called and `.path` was passed along with `.format = "fst"`.
#' 
#' @param path the (full) directory path to search
#' 
#' @export
list_fst <- function(path) {
  list.files(
    path, 
    full.names = TRUE, 
    pattern = "*.\\-bg\\.fst$"
  )
}

#' Get the contents of an fst file set
#' 
#' @inheritParams head_fst
#' @param .as_list should the results be returned as a list (`TRUE`) or a 
#' tibble (`FALSE`)
#' 
#' @seealso [list_fst()], [head_fst()]
#' 
#' @export
get_fst <- function(path, .as_list = FALSE) {
  files <- list_fst(path)
  ans <- lapply(files, read_fst)
  if(isTRUE(.as_list)) {
    return(ans)  
  }
  as_tibble(bind_rows(ans))
}

#' Get the head of an fst file set
#' 
#' 
#' @param path the directory to search
#' @param n number of rows to show
#' @param i which output output chunk to show 
#' 
#' @seealso [get_fst()], [list_fst()]
#' 
#' @export
head_fst <- function(path, n = 5, i = 1) {
  x <- list_fst(path)
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
#' @param .path a directory for saving simulated data; use this to collect 
#' results from several different runs in a single folder
#' @param .tag a name to use for the current run; results are saved under 
#' `.tag` in `.path` folder
#' @param .wait if `FALSE`, the function returns immediately; if `TRUE`, then 
#' wait until the background job is finished
#' @param .seed numeric; passed to [future.apply::future_mapply()] as 
#' `future.seed`, but only numeric values are accepted
#' @param .format the output format for saving simulations; using format
#' `fst` will allow saved results to be read with [fst::read_fst()]; using
#' format `arrow` will allow saved results to be read with 
#' [arrow::open_dataset()] with `format = "feather` or [arrow::read_feather()]; 
#' note that `fst` is installed with `mrgsim.parallel` but `arrow` may need 
#' explicit installation.
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
                        .path = NULL, .tag = NULL, 
                        .wait = FALSE, .seed = FALSE, 
                        .format = c("fst", "rds", "feather")) {
  
  .plan <- future::plan()
  notag <- is.null(.tag)
  .format <- match.arg(.format)
  if(notag) {
    .tag <- mod@model  
  }
  if(is.character(.path)) {
    if(.format == "arrow") {
      stopifnot(requireNamespace("arrow")) 
    }
    if(notag) {
      .tag <- basename(.path)
    }
    .path <- dirname(.path)
  }
  if(!is.character(.tag)) {
    stop(".tag must have type character")  
  }
  if(length(.tag) != 1) {
    stop(".tag must have length 1")  
  }
  
  if(is.data.frame(data)) {
    if(nchunk <= 1) {
      data <- list(data)
    } else {
      data <- chunk_by_id(data, nchunk = nchunk)      
    }
  }
  
  output_paths <- setup_locker(.path, .tag, n = length(data), .format = .format)
  
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
    args$.plan <- .plan
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
                            .format = "none", ...) {
  plan(.plan)
  #plan(future::multisession, workers = 6)
  future_mapply(
    FUN = bg_mrgsim_d_impl,
    data = data,
    output = output,
    MoreArgs = more,
    SIMPLIFY = FALSE,
    future.seed = .seed,
    .format = .format
  )
  # parallel::mcmapply(
  #   FUN = bg_mrgsim_d_impl,
  #   data = data,
  #   output = output,
  #   MoreArgs = more,
  #   SIMPLIFY = FALSE,
  #   future.seed = .seed,
  #   .format = .format
  # )
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
  if(.format == "feather") {
    arrow::write_feather( 
      x = out,
      sink = output
    )
    return(output)
  } 
  saveRDS(object = out, file = output)
  return(output)
}
