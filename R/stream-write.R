#' Writer functions for stream_file objects
#' 
#' This function will write out objects that have been assigned a format 
#' with either [format_stream()] or the `format` argument to [new_stream()].
#' See examples.
#' 
#' The default method writes to `rds` using [saveRDS()] only if there is no file
#' extension or the extension is `RDS` or `rds`. This default would be invoked 
#' if `write_stream` is called without setting the `format`.
#' 
#' @param x A `file_stream` object.
#' @param data An object to write.
#' @param dir An optional directory location to be used if not already in 
#' the `file` spot in `x`.
#' @param ... Not used.
#' 
#' @return 
#' `NULL` is returned, invisibly.
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
  if(!is.file_set_item(x)) {
    stop("`x` is not a file_set item.")  
  }
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
  file <- write_stream_dir_check(x$file, dir)
  fst::write_fst(x = data, path = file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_feather <- function(x, data, dir = NULL, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  require_arrow()
  file <- write_stream_dir_check(x$file, dir)
  arrow::write_feather(x = data, sink = file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_qs <- function(x, data, dir = NULL, ...) {
  require_qs()
  file <- write_stream_dir_check(x$file, dir)
  qs::qsave(x = data, file = file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_rds <- function(x, data, dir = NULL, ...) {
  file <- write_stream_dir_check(x$file, dir)
  saveRDS(object = data, file = file)
  return(invisible(NULL))
}
