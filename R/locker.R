
locker_tag <- function(locker) {
  basename(locker)
}

#' Initialize the locker directory
#' 
#' @param where The full path to the locker. 
#' 
#' @export
reset_locker <- function(where, ext = NULL) {
  locker_file <- ".mrgsim-parallel-locker-dir."
  locker_path <- file.path(where, locker_file)
  if(dir.exists(where)) {
    if(!file.exists(locker_path)) {
      msg <- c(
        "the dataset directory exists, but doesn't appear to be a valid ",
        "dataset location; please manually remove the folder or specify a new ",
        "folder and try again."
      )
      stop(msg)
    }
    if(!is.character(ext)) {
      pat <- "\\.(fst|feather|csv|qs|rds)$"
    } else {
      pat <- paste0("\\.(", paste0(ext, collapse = "|"), ")$") 
    }
    files <- list.files(
      where, 
      pattern = pat, 
      full.names = TRUE
    )
    unlink(files, recursive = TRUE)  
    if(length(list.files(where)) > 0) {
      msg <- c(
        "Could not clear locker directory; ", 
        "use initialize_locker to manually clear the files."
      )
      warning(msg)  
    }
  }
  if(!dir.exists(where)) dir.create(where, recursive = TRUE)
  cat(file = locker_path, "#")
  return(invisible(NULL))
}

#' Set up a data storage locker
#' 
#' A locker is a directory structure where an enclosing folder contains 
#' subfolders that in turn contain the results of different simulation runs. 
#' When the number of simulation result sets is known, a stream of file names 
#' (`setup_locker`) or file name objects (`sim_locker`) can be returned, which 
#' can then be fed into any number of parallel apply-like functions.
#' 
#' `dir` must exist when setting up the locker. The directory `tag` will be 
#' created under `dir` and must not exist except if it had previously been 
#' set up using `setup_locker`. Established `tag` directories will have a 
#' hidden file in them indicating that they are established simulation output
#' folders. When recreating the `tag` directory, it will be unlinked and created
#' new. 
#' 
#' @inheritParams file_set
#' @param ... Arguments passed to `setup_locker`. 
#' @param dir The directory that contains tagged directories of run 
#' results.
#' @param tag The name of a folder under `dir`; this directory must not 
#' exist the first time the locker is set up and __will be deleted__ and 
#' re-created each time it is used to store output from a new simulation run.
#' @param n The number of output files to be saved.
#' @param ext The format extension for output files, including `.`.
#' 
#' @return
#' When `n` is given, `setup_locker` returns a character vector of target file 
#' names; `sim_locker` returns a list of objects, containing the file name 
#' (`file`) and an index of the list position (`i`). When `n` is not given, an 
#' empty list is returned. 
#' 
#' @examples
#' x <- setup_locker(tempdir(), tag = "my-sims", n = 2)
#' x
#' 
#' y <- sim_locker(tempdir(), tag = "my-sim-2", n = 2)
#' y
#' 
#' @seealso [file_set()]
#' 
#' @export
sim_locker <- function(..., file_only = FALSE) {
  file <- setup_locker(...)
  if(isTRUE(file_only)) return(file)
  Map(file, seq_along(file), f = new_file_object, USE.NAMES = FALSE)
}

#' @rdname sim_locker
#' @export
setup_locker <- function(dir, tag = locker_tag(dir), 
                         n = 0, ext = "", prefix = NULL) {
  will_save <- is.character(dir) && length(dir)==1
  output_paths <- vector(mode = "list", length = n)
  if(!will_save) return(output_paths)
  if(missing(tag)) {
    output_folder <- dir
    dir <- dirname(dir)
  } else {
    output_folder <- file.path(dir, tag)
  }
  if(!dir.exists(dir)) {
    dir.create(dir, recursive = TRUE)
  }
  reset_locker(output_folder)
  if(n > 0) {
    output_files <- file_set(n, prefix = prefix, file_only = TRUE)
    output_files <- paste0(output_files, ext)
    output_paths <- file.path(output_folder, output_files)
  }
  class(output_paths) <- c("will_save", "list")
  output_paths
}
