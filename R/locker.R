n_of_N <- function(i) {
  pad <- ceiling(log10(max(i))) + 1
  mx <- max(i)
  i <- formatC(i, width = pad, flag = "0")
  mx <- i[length(i)]
  paste0(i, "-", mx)  
}

make_output_files <- function(n) {
  i <- seq_len(n)
  stems <- n_of_N(i)
  files <- paste0(stems, "-bg.")
  files
}

locker_tag <- function(locker) basename(locker)

#' Set up a data storage locker
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
#' 
#' @details
#' 
#' The `arrow` package must be installed to `write_dummy` files when `.format`
#' is `feather`.
#' 
#' @examples
#' 
#' x <- setup_locker(tempdir(), tag = "my-sims", n = 5)
#' x
#' 
#' @export
setup_locker <- function(locker, tag = locker_tag(locker), n = 0, 
                         .format = "fst",  write_dummy = FALSE) {
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
    example_data <- tibble()
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

