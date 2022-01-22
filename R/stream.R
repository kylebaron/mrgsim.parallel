stream_types <- c("fst", "feather", "qs", "rds")
stream_format_classes <- paste0("stream_format_", stream_types)
names(stream_format_classes) <- stream_types

re_set_ext <- function(x, ext) {
  x$file <- tools::file_path_sans_ext(x$file)
  x$file <- paste0(x$file, ".", ext)
  x
}

re_set_dir <- function(x, where) {
  x$file <- file.path(where, basename(x$file))
  x
}

write_stream_dir_check <- function(file, dir = NULL) {
  if(dirname(file)=="." && is.null(dir)) {
    stop("Cannot use `write_stream` to save to the working directory.") 
  }
  if(is.character(dir) && length(dir)==1) {
    file.path(dir, file)  
  } else {
    file  
  }
}

new_file_object <- function(file, i) {
  ans <- list(i = i, file = file)  
  attr(ans, "file_set_item") <- TRUE
  ans
}

stream_add_object <- function(stream, object) {
  stream$x <- object
  stream
}

is.locker_stream <- function(x) inherits(x, "locker_stream")
is.file_stream <- function(x) inherits(x, "file_stream")
is.file_set_item <- function(x) !is.null(attr(x, "file_set_item", exact=TRUE))


#' Create a stream of outputs and inputs
#' 
#' By stream we mean a list that pre-specifies the output file names, 
#' replicate numbers and possibly input objects for a simulation. Passing 
#' `locker` initiates a call to [setup_locker()], which sets up or resets
#'  the output directories. 
#' 
#' @param x A list or vector to template the stream; for the `numeric` method, 
#' passing a single number will fill `x` with a sequence of that length.
#' @param locker Passed to [setup_locker()] as `dir`; important to note that the 
#' directory will be unlinked if it exists and is an established locker 
#' directory. 
#' @param format Passed to [format_stream()].
#' @param ... Passed to [file_set()].
#' 
#' @return
#' A list with the following elements: 
#' 
#' - `i` the position number
#' - `file` the output file name
#' - `x` the input object.
#' 
#' The list has class `file_stream` as well as `locker_stream`( if `locker` was
#' passed) and a class attribute for the output if `format` was passed.
#' 
#' @examples
#' x <- new_stream(3)
#' x[[1]]
#' 
#' new_stream(2, locker = file.path(tempdir(), "foo"))
#' 
#' df <- data.frame(ID = c(1,2,3,4))
#' data <- chunk_by_id(df, nchunk = 2)
#' x <- new_stream(data)
#' x[[1]]
#' 
#' @seealso [format_stream()], [locate_stream()], [ext_stream()], [file_stream()], 
#'          [file_set()]
#' 
#' @export
new_stream <- function(x, ...) UseMethod("new_stream")

#' @rdname new_stream
#' @export
new_stream.list <- function(x, locker = NULL, format = NULL, ...) {
  if(length(x)==0) {
    stop("`x` must have length >= 1.")  
  }
  ans <- file_stream(locker = locker, n = length(x),  ...)
  cl <- class(ans)
  ans <- Map(ans, x, f = stream_add_object, USE.NAMES = FALSE)
  class(ans) <- cl
  if(is.character(format)) {
    ans <- format_stream(ans, format)  
  }
  ans
}

#' @rdname new_stream
#' @export
new_stream.numeric <- function(x, ...) {
  if(length(x)==1) {
    x <- seq(x)
  } 
  new_stream(as.list(x), ...)
}

#' @rdname new_stream
#' @export
new_stream.character <- function(x, ...) {
  new_stream(as.list(x), ...)
}

#' Set the format for a stream_file object
#' 
#' The format is set on the file objects inside the list so that the file 
#' object can be used to call a write method. See [write_stream()].
#' 
#' @param x A `file_stream` object.
#' @param type The file format type; if `feather` is chosen, then a check will
#' be made to ensure the `arrow` package is loaded. 
#' @param set_ext If `TRUE`, the existing extension (if it exists) is stripped
#' and a new extension is added based on the value of `type`.
#' @param warn If `TRUE` a warning will be issued in case the output format 
#' is set but there is no directory path associated with the `file` spot in 
#' `x[[1]]`.
#' 
#' @return
#' `x` is returned with a new calss attribute reflecting the expected output
#' format (`fst`, `feather` (arrow), `qs` or `rds`).
#' 
#' @seealso [locate_stream()], [ext_stream()], [new_stream()], [file_stream()], 
#'          [file_set()]
#' 
#' @examples
#' fs <- new_stream(2)
#' fs <- format_stream(fs, "fst")
#' fs[[1]]
#' 
#' @export
format_stream <- function(x, type = c("fst", "feather", "qs", "rds"), 
                          set_ext = TRUE, warn = FALSE) {
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object.")  
  }
  type <- match.arg(type)
  format <- stream_format_classes[type]
  if(type=="feather") require_arrow()
  if(type=="qs") require_qs()
  clx <- class(x)
  cl <- c(format, "list")
  cl <- unique(cl)
  ans <- lapply(x, function(xx) {
    class(xx) <- cl
    xx
  })
  if(isTRUE(set_ext)) {
    ans <- lapply(ans, re_set_ext, ext = type)  
  }
  if(dirname(ans[[1]]$file)=='.' & isTRUE(warn)) {
    warning("format was set, but file name [1] has no directory specified.")  
  }
  class(ans) <- clx
  ans
}
#' Set or change the directory for file_stream objects
#' 
#' Add or update the directory location for items in a `file_stream` object. 
#' If a directory path already exists, it is removed first. 
#' 
#' @param x A `file_stream` object.
#' @param where The new location. 
#' 
#' @examples
#' x <- new_stream(5)
#' x <- locate_stream(x, file.path(tempdir(), "foo"))
#' x[[1]]$file
#' 
#' @seealso [format_stream()], [ext_stream()], [new_stream()], [file_stream()], 
#'          [file_set()]
#' 
#' @export
locate_stream <- function(x, where) {
  clx <- class(x)
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object.")  
  }
  ans <- lapply(x, re_set_dir, where = where)
  class(ans) <- clx
  ans
}

#' Set or change the file extension on file_stream names
#' 
#' Add or update the file extension for items in a `file_stream` object. 
#' If a file extension exists, it is removed first. 
#' 
#' @param x A `file_stream` object.
#' @param ext The new extension. 
#' 
#' @examples
#' x <- new_stream(3)
#' x <- ext_stream(x, "feather")
#' x[[1]]$file
#' 
#' @seealso [format_stream()], [locate_stream()], [new_stream()], [file_stream()], 
#'          [file_set()]
#' 
#' @export
ext_stream <- function(x, ext) {
  clx <- class(x)
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object.")  
  }
  ans <- lapply(x, re_set_ext, ext = ext)
  class(ans) <- clx
  ans
}
