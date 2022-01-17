new_file_object <- function(file, i) {
  list(i = i, file = file)  
}
stream_add_object <- function(stream, object) {
  stream$x <- object
  stream
}

#' Generate a sequence of file objects
#' 
#' @param n The number of file names / objects to create.
#' @param tag A character prefix for the file name.
#' @param where An optional file path.
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
#' @seealso [sim_locker()], [setup_locker()]
#' 
#' @export
file_set <- function(n, tag = NULL, where = NULL, pad = TRUE, sep = "-", 
                     ext = "", file_only = FALSE) {
  nn <- seq(n)
  if(isTRUE(pad)) {
    nn <- formatC(nn, width = floor(log10(n)) + 1, flag = "0")    
  }
  file <- paste0(nn, sep, nn[length(nn)], ext)
  if(is.character(tag)) {
    file <- paste0(tag, sep, file)  
  }
  if(is.character(where)) {
    file <- file.path(where, file)  
  }
  if(isTRUE(file_only)) return(file)
  Map(file, seq_along(file), f = new_file_object, USE.NAMES = FALSE)
}

#' Create a stream of outputs and inputs
#' 
#' By stream we mean a list that pre-specifies the list of output file names, 
#' replicate numbers and possibly input objects for a simulation. Use 
#' `file_stream` to only generate file names with indices (see [file_set()]). 
#' Use `object_stream` to create a `file_stream` that contains an input 
#' data structure. Passing `dataset` initiates a call to [setup_locker()], 
#' which sets up the output directories. 
#' 
#' @param objects A list of objects or a vector.
#' @param n The number of streams to create.
#' @param dataset Passed to [setup_locker()]; important to note that the 
#' directory will be unlinked if it exists and is an established locker 
#' directory. 
#' @param ... Passed to [file_set()] (if `dataset` not provided) or 
#' [setup_locker()] (if `dataset` is provided).
#' 
#' @return
#' A list with the following elements: `i` the position number; `file` 
#' the output file name; `x` the input object (for `object_stream` only).
#' 
#' @examples
#' file_stream(n = 3)
#' file_stream(n = 2, dataset = file.path(tempdir(), "foo"))
#' 
#' df <- data.frame(ID = c(1,2,3,4))
#' data <- chunk_by_id(df, nchunk = 2)
#' object_stream(objects = data)
#' 
#' @seealso [setup_locker()], [file_set()], [internalize_fst()]
#' 
#' @export
file_stream <- function(n, dataset = NULL, ...) {
  if(!n > 0) {
    stop("`n` must be >= 1.")  
  }
  if(!is.character(dataset)) {
    ans <- file_set(n = n, file_only = FALSE, ...)
  } else {
    ans <- sim_locker(..., n = n, file_only = FALSE, dir = dataset) 
    class(ans) <- unique(c("locker_stream", class(ans)))
  }
  class(ans) <- unique(c("file_stream", class(ans)))
  ans
}

#' @rdname file_stream
#' @export
object_stream <- function(objects, dataset = NULL, ...) {
  if(!any(is.list(objects), is.vector(objects))) {
    stop("`objects` must be a list or a vector.")
  }
  if(length(objects)==0) {
    stop("`objects` must have length >= 1.")  
  }
  ans <- file_stream(dataset = dataset, n = length(objects), ...)
  cl <- class(ans)
  ans <- Map(ans, objects, f = stream_add_object, USE.NAMES = FALSE)
  class(ans) <- c("object_stream", cl)
  ans
}
