
n_of_N <- function(i) {
  pad <- ceiling(log10(max(i))) + 1
  mx <- max(i)
  i <- formatC(i, width = pad, flag = "0")
  mx <- formatC(mx, width = pad, flag = "0")
  paste0(i, "-", mx)  
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
#' @export
head_fst <- function(dir, n = 5, i = 1) {
  x <- list_fst(dir)
  read_fst(x[i], from = 1, to = n)
}

#' Run mrgsim in the background
#' 
#' 
#' 
#' @param mod a model object
#' @param ... arguments passed to [mrgsolve:::mrgsim()]
#' @param path a directory for saving simulated data
#' 
#' 
#' 
#' @export
bg_mrgsim_d <- function(mod, data,  ..., split = 1, 
                        locker = NULL, tag = NULL, 
                        plan = future::plan()) {

  if(is.data.frame(data)) {
    if(split <= 1) {
      data <- list(data)
    } else {
      data <- chunk_by_id(data, nchunk = split)      
    }
  }
  
  stems <- n_of_N(seq_along(data))
  output_files <- paste0(stems, "-bg.fst")

  if(is.character(locker)) {
    if(!dir.exists(locker)) {
      dir.create(locker, recursive = TRUE)  
    }
    if(is.null(tag)) {
      tag <- mod@model
    }
    folder <- file.path(locker,tag)
    if(dir.exists(folder)) {
      unlink(folder, recursive = TRUE)  
    }
    dir.create(folder)
    output_paths <- file.path(folder, output_files)
    for(i in output_paths) {
      write_fst(tibble(ID = 0)[0,], path = i)
    }
  }
  func <- bg_mrgsim_apply
  if(length(data)==1) {
    func <- bg_mrgsim_d_impl
    data <- data[[1]]
  }
  args <- list()
  args$more <- list(mod = mod, folder = folder, ...)
  args$plan <- plan
  args$data <- data
  r_bg(
    func, 
    args = args, 
    package = TRUE
  )
}

bg_mrgsim_apply <- function(data, plan, more, i,...) {
  options(future.fork.enable=TRUE)
  future::plan(plan)  
  future.apply::future_mapply(
    FUN = bg_mrgsim_d_impl, 
    data = data,
    i = n_of_N(seq_along(data)),
    MoreArgs = more,
    SIMPLIFY = FALSE, 
    future.seed = TRUE
  )  
}

bg_mrgsim_d_impl <- function(data, i, mod,  ..., folder = NULL) {
  out <- mrgsim(mod, data, ..., output = "df")
  if(is.null(folder)) return(out)
  file <- paste0(folder, "/", paste0(i, "-bg.fst"))
  if(!dir.exists(dirname(file))) {
    dir.create(dirname(file), recursive = TRUE)  
  }
  fst::write_fst(path = file, x = out)
  return(file)
}
