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
  list(i = i, file = file)  
}
stream_add_object <- function(stream, object) {
  stream$x <- object
  stream
}

is.locker_stream <- function(x) inherits(x, "locker_stream")
is.file_stream <- function(x) inherits(x, "file_stream")

#' Generate a sequence of file objects
#' 
#' @param n The number of file names / objects to create.
#' @param where An optional file path.
#' @param prefix A character prefix for the file name.
#' @param pad If `TRUE`, numbers will be padded with zeros.
#' @param sep Separator character.
#' @param ext A file extension, including the dot.
#' @param file_only If `TRUE` then only return an vector of file names.
#' 
#' @return
#' By default a list length `n` of lists length 2; each sublist contains the 
#' integer file number as `i` and the file name as `file`. If `file_only` is 
#' `TRUE`, then a vector of file names.
#' 
#' @examples
#' 
#' x <- file_set(3, where = "foo/bar")
#' length(x)
#' length(x[[1]])
#' x[[2]]$i
#' x[[3]]$file
#' 
#' file_set(2, file_only = TRUE)
#' 
#' @seealso [setup_locker()]
#' 
#' @export
file_set <- function(n, where = NULL, prefix = NULL, pad = TRUE, sep = "-", 
                     ext = "", file_only = FALSE) {
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
  if(isTRUE(file_only)) return(file)
  Map(file, seq_along(file), f = new_file_object, USE.NAMES = FALSE)
}

#' Create a stream of outputs and inputs
#' 
#' By stream we mean a list that pre-specifies the output file names, 
#' replicate numbers and possibly input objects for a simulation. Passing 
#' `dataset` initiates a call to [setup_locker()], which sets up the output 
#' directories. 
#' 
#' @param x A list or vector to template the stream; for the `numeric` method, 
#' passing a single number will fill `x` with a sequence of that length.
#' @param dataset Passed to [setup_locker()]; important to note that the 
#' directory will be unlinked if it exists and is an established locker 
#' directory. 
#' @param format Passed to [format_stream()].
#' @param ... Passed to [file_set()].
#' 
#' @return
#' A list with the following elements: `i` the position number; `file` 
#' the output file name; `x` the input object.
#' 
#' @examples
#' stream_new(3)
#' stream_newm(2, dataset = file.path(tempdir(), "foo"))
#' 
#' df <- data.frame(ID = c(1,2,3,4))
#' data <- chunk_by_id(df, nchunk = 2)
#' stream_new(data)
#' 
#' @seealso [setup_locker()], [file_set()], [internalize_fst()]
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
  stream_new(as.list(x), ...)
}

#' Create a stream of files
#' 
#' Optionally, setup a locker storage space on disk with a specific file 
#' format (e.g. `fst` or `feather`).
#' 
#' @inheritParams format_stream
#' @inheritParams new_stream
#' @param n The number of file names to generate.
#' @param ... Passed to [file_set()].
#' 
#' @examples
#' stream_file(3)
#' 
#' @export
file_stream <- function(n, locker = NULL, format = NULL, where = NULL, ...) {
  if(!n > 0) {
    stop("`n` must be >= 1.")  
  }
  if(!is.null(locker)) {
    where <- locker  
  } 
  ans <- file_set(n = n, file_only = FALSE, where = where, ...)
  if(is.character(locker)) {
    blah <- setup_locker(dir = locker, n = 0)
    class(ans) <- c("locker_stream", class(ans))
  }
  class(ans) <- unique(c("file_stream", class(ans)))
  if(is.character(format)) {
    ans <- format_stream(ans, format)  
  }
  ans
}

#' @export
summary.stream_file <- function(object, ...) {
  x <- object
  a <- paste0("Length: ", length(x))
  format <- class(x[[1]])[1]
  b <- paste0("Format: ", format)
  c <- paste0("Type: ", class(x[[1]]$x)[1])
  d <- paste0("Locker: ", is.locker_stream(x))
  e <- paste0("File[1]: ", x[[1]]$file)
  cat(a, c, b, e, d, sep = "\n")
}

#' Writer functions for stream_file objects
#' 
#' This function will write out objects that have been assigned a format 
#' with either [format_stream()] or the `format` argument to [new_stream()].
#' 
#' The default method writes to `rds` using [saveRDS()] and would be invoked 
#' `write_stream` is called without setting the `format`.
#' 
#' @param x A `file_stream` object.
#' @param data An object to write.
#' @param ... Not used.
#' 
#' @examples
#' ds <- temp_ds("example")
#' 
#' fs <- new_stream(2, dataset = ds, format = "fst")
#' 
#' data <- data.frame(x = rnorm(10))
#' 
#' x <- lapply(fs, write_streame, data = data)
#' 
#' list.files(ds)
#' 
#' reset_locker(ds)
#' 
#' fs <- stream_format(fs, "rds")
#' 
#' x <- lapply(fs, stream_write, data = data)
#' 
#' list.files(ds)
#' 
#' @seealso [format_stream()], [file_stream()]
#' 
#' @export
write_stream <- function(x, ...) UseMethod("write_stream")

#' @rdname write_stream
#' @export
write_stream.default <- function(x, data, dir = NULL, ...) {
  x$file <- write_stream_dir_check(x$file, dir)
  saveRDS(object = data, file = x$file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_fst <- function(x, data, dir = NULL, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  x$file <- write_stream_dir_check(x$file, dir)
  fst::write_fst(x = data, path = x$file)
  return(invisible(x))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_feather <- function(x, data, dir = NULL, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  require_arrow()
  x$file <- write_stream_dir_check(x$file, dir)
  arrow::write_feather(x = data, sink = x$file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_qs <- function(x, data, dir = NULL, ...) {
  require_qs()
  x$file <- write_stream_dir_check(x$file, dir)
  qs::qsave(x = data, file = x$file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_rds <- function(x, data, dir = NULL, ...) {
  x$file <- write_stream_dir_check(x$file, dir)
  saveRDS(object = data, file = x$file)
  return(invisible(NULL))
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
#' 
#' @seealso [locate_stream()], [file_stream()]
#' 
#' @examples
#' fs <- new_stream(2)
#' fs <- format_stream(fs, "fst")
#' fs
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
  if(dirname(ans[[1]]$file)=='.') {
    warning("format was set, but file name [1] has no directory specified.")  
  }
  class(ans) <- clx
  ans
}
#' Re-sets the directory for file_stream objects
#' 
#' @param x A `file_stream` object.
#' @param where The new location. 
#' 
#' @seealso [format_stream()], [file_stream()]
#' 
#' @export
locate_stream <- function(x, where) {
  clx <- class(x)
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object")  
  }
  ans <- lapply(x, re_set_dir, where = where)
  class(ans) <- clx
  ans
}

#' Change the extension on file_stream names
#' 
#' @param x A `file_stream` object.
#' @param ext The new extension. 
#' 
#' @seealso [format_stream()], [file_stream()]
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
