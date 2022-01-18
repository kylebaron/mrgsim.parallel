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

#' Writer functions for stream_file objects
#' 
#' @param x A `stream_file` object.
#' @param data An object to write.
#' @param ... Not used.
#' 
#' @seealso [stream_format()], [stream_file()]
#' 
#' @export
stream_write <- function(x, ...) UseMethod("stream_write")

#' @rdname stream_write
#' @export
stream_write.default <- function(x, data, ...) {
  saveRDS(object = data, file = x$file)
  return(invisible(NULL))
}

#' @rdname stream_write
#' @export
stream_write.stream_format_fst <- function(x, data, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  fst::write_fst(x = data, path = x$file)
  return(invisible(NULL))
}

#' @rdname stream_write
#' @export
stream_write.stream_format_feather <- function(x, data, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  require_arrow()
  arrow::write_feather(x = data, sink = x$file)
  return(invisible(NULL))
}

#' @rdname stream_write
#' @export
stream_write.stream_format_qs <- function(x, data, ...) {
  require_qs()
  qs::qsave(x = data, file = x$file)
  return(invisible(NULL))
}

#' @rdname stream_write
#' @export
stream_write.stream_format_rds <- function(x, data, ...) {
  saveRDS(object = data, file = x$file)
  return(invisible(NULL))
}

#' Set the format for a stream_file object
#' 
#' @param x A `file_stream` object.
#' @param type The file format type; if `feather` is chosen, then a check will
#' be made to ensure the `arrow` package is loaded. 
#' @param set_ext If `TRUE`, the existing extension (if it exists) is stripped
#' and a new extension is added based on the value of `type`.
#' 
#' @seealso [stream_locate()], [stream_file()]
#' 
#' @export
stream_format <- function(x, type = c("fst", "feather", "qs", "rds"), 
                          set_ext = TRUE) {
  if(!is.stream_file(x)) {
    stop("`x` must be a stream_file object")  
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
  class(ans) <- clx
  ans
}
#' Re-sets the directory for stream_file objects
#' 
#' @param x A `stream_file` object.
#' @param where The new location. 
#' 
#' @seealso [stream_format()], [stream_file()]
#' 
#' @export
stream_locate <- function(x, where) {
  clx <- class(x)
  if(!is.stream_file(x)) {
    stop("`x` must be a stream_file object")  
  }
  ans <- lapply(x, re_set_dir, where = where)
  class(ans) <- clx
  ans
}
