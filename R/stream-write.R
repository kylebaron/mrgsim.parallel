stream_types <- c("fst", "feather", "qs", "rds")
stream_format_classes <- paste0("stream_format_", stream_types)

re_set_ext <- function(x, ext) {
  x$file <- tools::file_path_sans_ext(x$file)
  x$file <- paste0(x$file, ".", ext)
  x
}

re_set_dir <- function(x, where) {
  x$file <- file.path(where, basename(x$file))
  x
}

#' Writer functions for file_stream objects
#' 
#' @param x A `file_stream` object.
#' @param data An object to write.
#' @param ... Not used.
#' 
#' @seealso [format_stream()], [file_stream()], [object_stream()]
#' 
#' @export
write_stream <- function(x, ...) UseMethod("write_stream")

#' @rdname write_stream
#' @export
write_stream.stream_format_fst <- function(x, data, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  fst::write_fst(x = data, path = x$file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_feather <- function(x, data, ...) {
  if(!is.data.frame(data)) stop("`x` must be a data.frame")
  require_arrow()
  arrow::write_feather(x = data, sink = x$file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_qs <- function(x, data, ...) {
  require_qs()
  qs::qsave(x = data, file = x$file)
  return(invisible(NULL))
}

#' @rdname write_stream
#' @export
write_stream.stream_format_rds <- function(x, data, ...) {
  saveRDS(x, file = x$file)
  return(invisible(NULL))
}

#' Set the format for a file_stream object
#' 
#' @param x A `file_stream` object.
#' @param type The file format type; if `feather` is chosen, then a check will
#' be made to ensure the `arrow` package is loaded. 
#' @param set_ext If `TRUE`, the existing extension (if it exists) is stripped
#' and a new extension is added based on the value of `type`.
#' 
#' @seealso [locate_stream()], [file_stream()], [object_stream()] 
#' 
#' @export
format_stream <- function(x, type = c("fst", "feather", "qs", "rds"), 
                          set_ext = TRUE) {
  if(!is.file_stream(x)) {
    stop("`x` must be a file_stream object")  
  }
  type <- match.arg(type)
  format <- "none"
  if(type=="fst") format <- "stream_format_fst"
  if(type=="feather") {
    format <- "stream_format_feather"
    require_arrow()
  }
  if(type=="qs") {
    format <- "stream_format_qs"
    require_qs()
  }
  if(type=="rds") format <- "stream_format_rds"
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
#' Re-sets the directory for file_stream objects
#' 
#' @param x A `file_stream` object.
#' @param where The new location. 
#' 
#' @seealso [format_stream()], [file_stream()], [object_stream()]
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
