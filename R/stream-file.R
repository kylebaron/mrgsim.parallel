#' Generate a sequence of file objects
#' 
#' File names have a numbered core that communicates the current file number
#' as well as the total number of files in the set. For example, `02-20` would
#' indicate the second file in a set of 20. Other customizations can be added. 
#' 
#' @param n The number of file names to create.
#' @param where An optional output file path.
#' @param prefix A character prefix for the file name.
#' @param pad If `TRUE`, numbers will be padded with zeros.
#' @param sep Separator character.
#' @param ext A file extension, including the dot.
#' 
#' @return
#' By default a list length `n` of lists length 2; each sublist contains the 
#' integer file number as `i` and the file name as `file`. 
#' 
#' @examples
#' 
#' x <- file_set(3, where = "foo/bar")
#' length(x)
#' x[2]
#' 
#' x <- file_set(25, ext = ".feather")
#' x[17]
#' 
#' @seealso [setup_locker()]
#' 
#' @export
file_set <- function(n, where = NULL, prefix = NULL, pad = TRUE, sep = "-", 
                     ext = "") {
  if(!(is.numeric(n) && length(n) == 1 && n >= 1)) {
    stop("`n` must be a positive numeric value")  
  }
  n <- floor(n)
  nn <- seq(n)
  if(isTRUE(pad)) {
    nn <- formatC(nn, width = floor(log10(n)) + 1, flag = "0")    
  }
  file <- paste0(nn, sep, nn[length(nn)], ext)
  if(is.character(prefix)) {
    file <- paste0(prefix, sep, file)  
  }
  if(is.character(where)) {
    file <- file.path(where, file)  
  }
  file
}

#' Create a stream of files
#' 
#' Optionally, setup a locker storage space on disk with a specific file 
#' format (e.g. `fst` or `feather`).
#' 
#' Pass `locker` to set up locker space for saving outputs; this involves
#' clearing the `locker` directory (see [setup_locker()] for details). Passing 
#' `locker` also sets the path for output files. If you want to set up the path 
#' for output files without setting up `locker` space, pass `where`. 
#' 
#' @inheritParams format_stream
#' @inheritParams new_stream
#' @inheritParams file_set
#' @param n The number of file names to generate; must be a single numeric 
#' value greater than or equal to 1.
#' @param where An optional file path; this is replaced by `locker` if it is 
#' also passed. 
#' @param ... Additional arguments passed to [file_set()].
#' 
#' @examples
#' x <- file_stream(3, locker = temp_ds("foo"), format = "fst")
#' x[[1]]
#' 
#' @seealso [format_stream()], [locate_stream()], [ext_stream()], [new_stream()],
#'          [file_set()]
#' 
#' @export
file_stream <- function(n, locker = NULL, format = NULL, where = NULL, ...) {
  if(!is.null(locker)) {
    where <- locker  
  } 
  files <- file_set(n = n, where = where, ...)
  ans <- Map(files, seq_along(files), f = new_file_object, USE.NAMES = FALSE)
  if(is.character(locker)) {
    blah <- setup_locker(where = locker)
    class(ans) <- c("locker_stream", class(ans))
  }
  class(ans) <- unique(c("file_stream", class(ans)))
  if(is.character(format)) {
    ans <- format_stream(ans, format)  
  }
  ans
}

#' @export
summary.file_stream <- function(object, ...) { #nocov start
  x <- object
  a <- paste0("Length: ", length(x))
  format <- class(x[[1]])[1]
  b <- paste0("Format: ", format)
  c <- paste0("Type: ", class(x[[1]]$x)[1])
  d <- paste0("Locker: ", is.locker_stream(x))
  e <- paste0("File[1]: ", x[[1]]$file)
  cat(a, c, b, e, d, sep = "\n")
} #nocov end
