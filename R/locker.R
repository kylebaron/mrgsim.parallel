
locker_tag <- function(locker) {
  basename(locker)
}

#' Check if a directory is dedicated locker space
#' 
#' @param where The locker location.
#' 
#' @export
is_locker_dir <- function(where) {
  file.exists(file.path(where, .locker_file_name))  
}

.locker_file_name <- ".mrgsim-parallel-locker-dir" 

clear_locker <- function(where, locker_path, pattern) {
  if(!file.exists(locker_path)) {
    msg <- c(
      "the dataset directory exists, but doesn't appear to be a valid ",
      "locker location; please manually remove the folder or specify a new ",
      "folder and try again."
    )
    stop(msg)
  }
  if(!is.character(pattern)) {
    pattern <- "\\.(fst|feather|parquet|csv|qs|rds|ext)$"
  } 
  files <- list.files(
    where, 
    pattern = pattern, 
    full.names = TRUE
  )
  unlink(files, recursive = TRUE)  
  if(length(list.files(where)) > 0) {
    msg <- c(
      "Could not clear locker directory; ", 
      "use unlink(\"locker/location\", recursive = TRUE) to manually clear 
        the files or select a different location."
    )
    warning(msg)  
  }
}

#' Initialize the locker directory
#' 
#' This function is called by [setup_locker()] to initialize and 
#' re-initialize a locker directory. We call it `reset_locker` because it is 
#' expected that the locker space is created once and then repeatedly 
#' reset and simulations are run and re-run. 
#' 
#' For the locker space to be initialized, the `where` directory must not 
#' exist; if it exists, there will be an error. It is also an error for 
#' `where` to exist and not contain a particular hidden locker file name
#' that marks the directory as established locker space. 
#' 
#' __NOTE__: when the locker is reset, all contents are cleared according 
#' to the files matched by `pattern`. If any un-matched files exist after
#' clearing the directory, a warning will be issued. 
#' 
#' @param where The full path to the locker. 
#' @param pattern A regular expression for finding files to clear from the 
#' locker directory.
#' 
#' @seealso [setup_locker()], [noreset_locker()], [version_locker()]
#' 
#' @export
reset_locker <- function(where, pattern = NULL) {
  locker_file <- .locker_file_name
  locker_path <- file.path(where, locker_file)
  if(dir.exists(where)) {
    clear_locker(where, locker_path, pattern)
  } else {
    dir.create(where, recursive = TRUE)
  }
  cat(file = locker_path, "#")
  return(invisible(NULL))
}

#' Set up a data storage locker
#' 
#' A locker is a directory structure where an enclosing folder contains 
#' subfolders that in turn contain the results of different simulation runs. 
#' When the number of simulation result sets is known, a stream of file names
#' is returned. This function is mainly called by other functions; an exported
#' function and documentation is provided in order to better communicate how
#' the locker works. 
#' 
#' `where` must exist when setting up the locker. The directory `tag` will be 
#' created under `where` and must not exist except if it had previously been 
#' set up using `setup_locker`. Existing `tag` directories will have a 
#' hidden file in them indicating that they are established simulation output
#' folders. 
#' 
#' When recreating the `tag` directory, it will be unlinked and created new.
#' To not try to set up a locker directory that already contains outputs that 
#' need to be preserved. You can call [noreset_locker()] on that directory
#' to prevent future resets. 
#' 
#' @param where The directory that contains tagged directories of run 
#' results.
#' @param tag The name of a folder under `where`; this directory must not 
#' exist the first time the locker is set up and __will be deleted__ and 
#' re-created each time it is used to store output from a new simulation run.
#' 
#' @return
#' The locker location.
#' 
#' @examples
#' x <- setup_locker(tempdir(), tag = "my-sims")
#' x
#' 
#' @seealso [reset_locker()], [noreset_locker()], [version_locker()]
#' 
#' @export
setup_locker <- function(where, tag = locker_tag(where)) {
  if(missing(tag)) {
    output_folder <- where
    where <- dirname(where)
  } else {
    output_folder <- file.path(where, tag)
  }
  if(!dir.exists(where)) {
    dir.create(where, recursive = TRUE)
  }
  reset_locker(output_folder)
  return(invisible(output_folder))
}

#' Prohibit a locker space from being reset
#' 
#' This function removes the the hidden locker file which designates a directory
#' as a locker. Once the locker is modified this way, it cannot be reset again 
#' by calling [setup_locker()] or [new_stream()].
#' 
#' @param where The locker location. 
#' 
#' @return
#' A logical value indicating if write ability was successfully revoked. 
#' 
#' @seealso [setup_locker()], [reset_locker()], [version_locker()]
#' 
#' @export
noreset_locker <- function(where) {
  locker_file <- file.path(where, .locker_file_name)
  if(!file.exists(locker_file)) {
    stop("`where` does not appear to be a locker.")  
  }
  ans <- file.remove(locker_file)
  return(invisible(ans))
}

#' Version locker contents
#' 
#' @param where The locker location. 
#' @param version A tag to be appended to `where` for creating a backup of the 
#' locker contents.
#' @param overwrite If `TRUE`, the new location will be removed with [unlink()]
#' if it exists.
#' @param noreset If `TRUE`, [noreset_locker()] is called **on the new version**.
#' 
#' @return
#' A logical value indicating whether or not all files were successfully copied
#' to the backup, invisibly. 
#' 
#' @examples
#' locker <- file.path(tempdir(), "version-locker-example")
#' 
#' if(dir.exists(locker)) unlink(locker, recursive = TRUE)
#' 
#' x <- new_stream(1, locker = locker)
#' 
#' cat("test", file = file.path(locker, "1-1"))
#' 
#' dir.exists(locker)
#' 
#' list.files(locker, all.files = TRUE)
#' 
#' y <- version_locker(locker, version = "y")
#' 
#' y
#' 
#' list.files(y, all.files = TRUE)
#' 
#' @seealso [reset_locker()], [noreset_locker()], [setup_locker()]
#' @export
version_locker <- function(where, version = "save", overwrite = FALSE, 
                           noreset = FALSE) {
  if(!is_locker_dir(where)) {
    stop("`where` does not appear to be a locker.")  
  }
  saved <- paste0(where, "-", version)
  if(dir.exists(saved)) {
    if(isTRUE(overwrite)) {
      unlink(saved, recursive = TRUE)  
    } else {
      stop("A directory already exists with this version.")  
    }
  }
  dir.create(saved, recursive = TRUE)
  files <- list.files(where, full.names = TRUE, all.files = TRUE, no..=TRUE)
  ans <- file.copy(files, saved)
  if(!all(ans)) stop("There was a problem copying files to new version.")
  if(isTRUE(noreset)) noreset_locker(saved)
  return(invisible(saved))
}
