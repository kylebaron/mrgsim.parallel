#' Writer functions for stream_file objects
#' 
#' This function will write out objects that have been assigned a format 
#' with either [format_stream()] or the `format` argument to [new_stream()].
#' See examples.
#' 
#' The default method always returns `FALSE`; other methods which get invoked
#' if a `format` was set will return `TRUE`. So, the user can always call 
#' `write_stream()` and check the return value: if `TRUE`, the file was written
#' to disk and the data to not need to be returned; a `FALSE` return value
#' indicates that no format was set and the data should be returned.
#' 
#' Note the write methods can be invoked directly for a specific format 
#' if no `format` was set (see examples). 
#' 
#' @param x A `file_stream` object.
#' @param data An object to write.
#' @param dir An optional directory location to be used if not already in 
#' the `file` spot in `x`.
#' @param ... Not used.
#' 
#' @return 
#' A logical value indicating if the output was written or not. 
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
write_stream.default <- function(x, data, ...) {
  return(invisible(FALSE))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_fst <- function(x, data, dir = NULL, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  file <- write_stream_dir_check(x$file, dir)
  fst::write_fst(x = data, path = file)
  return(invisible(TRUE))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_feather <- function(x, data, dir = NULL, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  require_arrow()
  file <- write_stream_dir_check(x$file, dir)
  arrow::write_feather(x = data, sink = file)
  return(invisible(TRUE))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_qs <- function(x, data, dir = NULL, ...) {
  require_qs()
  file <- write_stream_dir_check(x$file, dir)
  qs::qsave(x = data, file = file)
  return(invisible(TRUE))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_rds <- function(x, data, dir = NULL, ...) {
  file <- write_stream_dir_check(x$file, dir)
  saveRDS(object = data, file = file)
  return(invisible(TRUE))
}
