
# mark_done <- function(dataset, start_time) {
#   end_time <- date()
#   file <- file.path(dataset, "DONE")
#   cat(file = file, paste0("start: ", start_time), "\n")
#   cat(file = file, paste0("end:   ", end_time), "\n", append = TRUE)
# }

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

#' Set up a data storag locker
#' 
#' @param locker the directory that contains tagged directories of run 
#' results
#' @param tag the name of a folder under `locker`; this directory must not 
#' exist the first time the locker is set up and will be deleted and re-created
#' each time it is used to store output from a new simulation run
#' @param n the number of output files to be saved
#' @param .format the format extension for output files; use `fst` to create
#' outputs using [fst::write_fst()]; use `feather` to create outputs using 
#' @param write_dummy if `TRUE` placeholder files are created containing 
#' tibbles with no rows
#' [arrow::write_feather()]
setup_locker <- function(locker, tag, n = 0, .format = "fst", 
                         write_dummy = FALSE) {
  will_save <- is.character(locker) && length(locker)==1
  output_paths <- vector(mode = "list", length = n)
  if(!will_save) return(output_paths)
  output_folder <- file.path(locker, tag)  
  locker_file <- file.path(output_folder, ".locker-dir")
  if(!dir.exists(locker)) {
    dir.create(locker, recursive = TRUE)
  }
  if(dir.exists(output_folder)) {
    if(!file.exists(locker_file)) {
      msg <- c(
        "the dataset directory exists, but doesn't appear to be a valid ",
        "dataset location; please manually remove the folder or specify a new ",
        "folder and try again."
      )
      stop(msg)
    }
    unlink(output_folder, recursive = TRUE)  
  }
  dir.create(output_folder)
  cat(file = locker_file, "#")
  if(n > 0) {
    output_files <- make_output_files(n)
    output_files <- paste0(output_files, .format)
    output_paths <- file.path(output_folder, output_files)
    example_data <- tibble(ID = 0)[0,]
    if(.format == "fst" && write_dummy) {
      for(path in output_paths) {
        write_fst(example_data, path = path)
      }
    }
    if(.format == "feather" && write_dummy) {
      require_arrow()
      for(path in output_paths) {
        arrow::write_feather(example_data, sink = path)
      }
    }
  }
  class(output_paths) <- c("will_save", "list")
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
#' @param ... not used
#' 
#' @seealso [list_fst()], [head_fst()]
#' 
#' @export
internalize_fst <- function(path, .as_list = FALSE, ...) {
  files <- list_fst(path)
  ans <- lapply(files, read_fst)
  if(isTRUE(.as_list)) {
    return(ans)  
  }
  as_tibble(bind_rows(ans)) 
}
#' @export
#' @rdname internalize_fst
get_fst <- internalize_fst

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
#' This function uses [callr::r_bg()] to simulate a dataset in the background, 
#' optionally in parallel and optionally saving the results directly to 
#' disk in  `fst`, `arrow` or `rds` format. Parallelization can be mediated 
#' by the `parallel` package on unix or macos or `future` on any os. 
#' 
#' @inheritParams parallel_mrgsim_d
#' 
#' @param mod a model object
#' @param ... arguments passed to [mrgsolve::mrgsim()]
#' @param .dataset a directory for saving simulated data; use this to collect 
#' results from several different runs in a single folder
#' @param .tag a name to use for the current run; results are saved under 
#' `.tag` in `.path` folder
#' @param .format the output format for saving simulations; using format
#' `fst` will allow saved results to be read with [fst::read_fst()]; using
#' format `arrow` will allow saved results to be read with 
#' [arrow::open_dataset()] with `format = "feather`; note that `fst` is 
#' installed with `mrgsim.parallel` but `arrow` may need explicit installation
#' @param .wait if `FALSE`, the function returns immediately; if `TRUE`, then 
#' wait until the background job is finished
#' @param .seed numeric; used to set the seed for the simulation; this is the 
#' only way to control the random number generation for your simulation
#' @param .cores the number of cores to parallelize across; pass 1 to run the 
#' simulation sequentially
#' @param .plan the name of a [future::plan()] strategy; if passed, the 
#' parallelization will be handled by the `future` package
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
#' ) 
#' process$get_result()
#'  
#'  
#' ds <- file.path(tempdir(), "sims")
#' files <- bg_mrgsim_d(
#'   mod, data, carry_out = "dose", 
#'   .wait = TRUE, 
#'   .dataset = ,
#'   .format = "fst"
#' )
#' files
#' sims <- internalize_fst(ds)
#' head(sims)
#'   
#' 
#' @return 
#' An `r_process` object; see [callr::r_bg()]. Call `process$get_resuilt()` to 
#' get the actual result (see `details`). If a `.dataset` path is supplied, 
#' the simulated data is saved to disk and a list of file names is returned. 
#' 
#' @export
bg_mrgsim_d <- function(mod, data, nchunk = 1,   
                        ..., 
                        .dataset = NULL, .tag = NULL, 
                        .format = c("fst", "feather", "rds"),
                        .wait = TRUE, .seed = FALSE, 
                        .cores = 1, .plan = NULL) {
  
  .format <- match.arg(.format)
  .path <- NULL  
  notag <- is.null(.tag)
  Plan <- list(workers = .cores)
  
  if(is.character(.plan)) {
    Plan$strategy <- .plan
    if(.cores==1) Plan$workers <- NULL
  }
  if(notag) {
    .tag <- mod@model  
  }
  if(is.character(.dataset)) {
    if(.format == "arrow" && !arrow_installed()) {
      stop("the arrow package must be installed to complete this task.")
    }
    if(notag) {
      .tag <- basename(.dataset)
    }
    .path <- dirname(.dataset)
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
  if(!is.list(data)) {
    stop("`data` didn't resolve to list format.")  
  }
  
  output_paths <- setup_locker(
    .path, 
    .tag, n = length(data), 
    .format = .format, 
    write_dummy = TRUE
  )
  
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
