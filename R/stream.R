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
#' The list has class `file_stream` as well as `locer_stream`( if `locker` was
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
#' Re-sets the directory for file_stream objects
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

#' Create a stream of files
#' 
#' Optionally, setup a locker storage space on disk with a specific file 
#' format (e.g. `fst` or `feather`).
#' 
#' Pass `locker` to set up locker space for saving outputs; this also sets
#' the path for output files. If you want to set up the path for output files
#' without setting up `locker` space, pass `where`. 
#' 
#' @inheritParams format_stream
#' @inheritParams new_stream
#' @inheritParams file_set
#' @param n The number of file names to generate.
#' @param ... Passed to [file_set()].
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
#' @param dir An optional directory location to be used if not already in 
#' the `file` spot in `x`.
#' @param ... Not used.
#' 
#' @examples
#' ds <- temp_ds("example")
#' 
#' fs <- new_stream(2, locker = ds, format = "fst")
#' 
#' data <- data.frame(x = rnorm(10))
#' 
#' x <- lapply(fs, write_stream, data = data)
#' 
#' list.files(ds)
#' 
#' reset_locker(ds)
#' 
#' fs <- format_stream(fs, "rds")
#' 
#' x <- lapply(fs, write_stream, data = data)
#' 
#' list.files(ds)
#' 
#' @seealso [format_stream()], [ext_stream()], [locate_stream()], [new_stream()],
#'          [file_stream()]
#' 
#' @export
write_stream <- function(x, ...) UseMethod("write_stream")

#' @rdname write_stream
#' @export
write_stream.default <- function(x, data, dir = NULL, ...) {
  if(!tolower(file_ext(x$file)) %in% c("",  "rds")) {
    stop(
      "the default `write_stream` method requires no file extension or ", 
      "extension of RDS or rds."
    )
  }
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
