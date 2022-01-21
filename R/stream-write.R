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
