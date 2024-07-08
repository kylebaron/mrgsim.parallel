

re_set_ext <- function(x, ext) {
  x$file <- tools::file_path_sans_ext(x$file)
  x$file <- paste0(x$file, ext)
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

#' Check if an object inherits from locker_stream
#' 
#' @param x An object. 
#' 
#' @return 
#' Logical value indicating if `x` inherits from `locker_stream`.
#' 
#' @examples
#' x <- new_stream(2, locker = temp_ds("locker-stream-example"))
#' is.locker_stream(x)
#' 
#' @export
is.locker_stream <- function(x) inherits(x, "locker_stream")
#' Check if an object inherits from file_stream
#' 
#' @param x An object. 
#' 
#' @return 
#' Logical value indicating if `x` inherits from `file_stream`.
#' 
#' @examples
#' x <- new_stream(2)
#' is.file_stream(x)
#' 
#' @export
is.file_stream <- function(x) inherits(x, "file_stream")
#' Check if an object is a file_set_item
#' 
#' @param x An object. 
#' 
#' @return 
#' Logical value indicating if `x` has the `file_set_item` attribute set..
#' 
#' @examples
#' x <- new_stream(2)
#' is.file_set_item(x[[2]])
#' 
#' @export
is.file_set_item <- function(x) !is.null(attr(x, "file_set_item", exact = TRUE))
#' Check format status of file set item
#' 
#' This can be used to check if a file set item has been assigned an output 
#' format (e.g. `fst`, `feather`, `parquet`, `qs` or `rds`). If the check returns 
#' `FALSE` it would signal that data should be returned rather than calling
#' [write_stream()].
#' 
#' @param x An object, usually a `file_set_item`. 
#' 
#' @return 
#' Logical indicating if `x` inherits from one of the stream format classes. .
#' 
#' @export
format_is_set <- function(x) {
  inherits(x, .pkgenv$stream_format_classes)  
}
#' @rdname format_is_set
#' @export
is.stream_format <- format_is_set

#' Create a stream of outputs and inputs
#' 
#' By stream we mean a list that pre-specifies the output file names, 
#' replicate numbers and possibly input objects for a simulation. Passing 
#' `locker` initiates a call to [setup_locker()], which sets up or resets
#'  the output directories. 
#'  
#'  For the `data.frame` method, the data are chunked into a list by columns 
#'  listed in `cols`. Ideally, this is a singlel column that operates as 
#'  a unique `ID` across the data set and is used by [chunk_by_id()] to 
#'  form the chunks. Alternatively, `cols` can be multiple column names which 
#'  are pasted together to form a unique `ID` that is used for splitting 
#'  via [chunk_by_cols()].
#' 
#' @param x A list or vector to template the stream; for the `numeric` method, 
#' passing a single number will fill `x` with a sequence of that length.
#' @param locker Passed to [setup_locker()] as `dir`; important to note that the 
#' directory will be unlinked if it exists and is an established locker 
#' directory. 
#' @param format Passed to [format_stream()].
#' @param ... Additional arguments passed to [file_set()].
#' 
#' @return
#' A list with the following elements: 
#' 
#' - `i` the position number
#' - `file` the output file name
#' - `x` the input object.
#' 
#' The list has class `file_stream` as well as `locker_stream` (if `locker` was
#' passed) and a class attribute for the output if `format` was passed.
#' 
#' @examples
#' x <- new_stream(3)
#' x[[1]]
#' 
#' new_stream(2, locker = file.path(tempdir(), "foo"))
#' 
#' df <- data.frame(ID = c(1,2,3,4))
#' x <- new_stream(df, nchunk = 2)
#' x[[2]]
#' 
#' format_is_set(x[[2]])
#' 
#' x <- new_stream(3, format = "fst")
#' format_is_set(x[[2]])
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

#' @inheritParams chunk_data_frame
#' @param cols The name(s) of the column(s) specifying unique IDs to use to 
#' split the `data.frame` into chunks; this could be a unique `ID` or a 
#' combination of columns that when pasted together form a unique ID.
#' @rdname new_stream
#' @export
new_stream.data.frame <- function(x, nchunk, cols = "ID", locker = NULL, 
                                  format = NULL, ...) {
  if(nchunk < 1) {
    stop("`nchunk` must be >= 1.")  
  }
  if(nrow(x) < nchunk) {
    stop("`x` must have >= `nchunk` rows.")  
  }
  if(is.null(cols)) {
    x <- chunk_by_row(x, nchunk = nchunk)
  } else {
    x <- chunk_by_cols(x, nchunk = nchunk, cols = cols)
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
#' @param type The file format type; if `feather` or `parquet` is chosen, then 
#' a check will be made to ensure the `arrow` package is loaded. 
#' @param set_ext If `TRUE`, the existing extension (if it exists) is stripped
#' and a new extension is added based on the value of `type`.
#' @param warn If `TRUE` a warning will be issued in case the output format 
#' is set but there is no directory path associated with the `file` spot in 
#' `x[[1]]`.
#' 
#' @return
#' `x` is returned with a new class attribute reflecting the expected output
#' format (`fst`, `feather` (arrow), `parquet` (arrow), `qs` or `rds`).
#' 
#' @seealso [format_is_set()], [locate_stream()], [ext_stream()], 
#'          [new_stream()], [file_stream()], [file_set()]
#' 
#' @examples
#' fs <- new_stream(2)
#' fs <- format_stream(fs, "fst")
#' fs[[1]]
#' 
#' format_is_set(fs[[1]])  
#'  
#' @export
format_stream <- function(x, type = c("fst", "feather", "parquet", "qs", "rds"), 
                          set_ext = TRUE, warn = FALSE) {
  
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object.")  
  }
  type <- match.arg(type)
  format <- .pkgenv$stream_format_classes[type]
  if(type=="feather" | type=="parquet") require_arrow()
  if(type=="qs") require_qs()
  clx <- class(x)
  cl <- c(format, "list")
  cl <- unique(cl)
  ans <- lapply(x, function(xx) {
    class(xx) <- cl
    xx
  })
  if(isTRUE(set_ext)) {
    ans <- lapply(ans, re_set_ext, ext = paste0(".", type))
  }
  if(dirname(ans[[1]]$file)=='.' & isTRUE(warn)) {
    warning("The format was set, but file name [1] has no directory specified.")  
  }
  class(ans) <- clx
  ans
}

#' Set or change the directory for file_stream objects
#' 
#' Add or update the directory location for items in a `file_stream` object. 
#' If a directory path already exists, it is removed first. 
#' 
#' When `initialize` is set to `TRUE`, the locker space is initialized **or**
#' reset. In order to initialize, `where` must not exist or it must have been 
#' previously set up as locker space. See [setup_locker()] for details.
#' 
#' @param x A `file_stream` object.
#' @param where The new location. 
#' @param initialize If `TRUE`, then the `where` directory is passed to a call
#' to [reset_locker()].
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
locate_stream <- function(x, where, initialize = FALSE) {
  clx <- class(x)
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object.")  
  }
  if(isTRUE(initialize)) {
    reset_locker(where)  
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
