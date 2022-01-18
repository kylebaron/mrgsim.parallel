new_file_object <- function(file, i) {
  list(i = i, file = file)  
}
stream_add_object <- function(stream, object) {
  stream$x <- object
  stream
}

is.stream_locker <- function(x) inherits(x, "stream_locker")
is.stream_file <- function(x) inherits(x, "stream_file")

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
#' By stream we mean a list that pre-specifies the list of output file names, 
#' replicate numbers and possibly input objects for a simulation.  Passing 
#' `dataset` initiates a call to [setup_locker()], which sets up the output 
#' directories. 
#' 
#' @param x A list or vector to template the stream; for the `numeric` method, 
#' passing a single number will fill `x` with a sequence of that length.
#' @param dataset Passed to [setup_locker()]; important to note that the 
#' directory will be unlinked if it exists and is an established locker 
#' directory. 
#' @param format Passed to [stream_format()].
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
stream_new <- function(x, ...) UseMethod("stream_new")

#' @rdname stream_new
#' @export
stream_new.list <- function(x, dataset = NULL, format = NULL, ...) {
  if(length(x)==0) {
    stop("`x` must have length >= 1.")  
  }
  ans <- stream_file(dataset = dataset, n = length(x),  ...)
  cl <- class(ans)
  ans <- Map(ans, x, f = stream_add_object, USE.NAMES = FALSE)
  class(ans) <- cl
  if(is.character(format)) {
    ans <- stream_format(ans, format)  
  }
  ans
}

#' @rdname stream_new
#' @export
stream_new.numeric <- function(x, ...) {
  if(length(x)==1) {
    x <- seq(x)
  } 
  stream_new(as.list(x), ...)
}

#' @rdname stream_new
#' @export
stream_new.character <- function(x, ...) {
  stream_new(as.list(x), ...)
}

#' Create a stream of files
#' 
#' Optionally, setup a locker storage space on disk with a specific file 
#' format (e.g. `fst` or `feather`).
#' 
#' @inheritParams stream_format
#' @inheritParams stream_new
#' @param n The number of file names to generate.
#' @param ... Passed to [file_set()].
#' 
#' @examples
#' stream_file(3)
#' 
#' @export
stream_file <- function(n, dataset = NULL, format = NULL, ...) {
  if(!n > 0) {
    stop("`n` must be >= 1.")  
  }
  ans <- file_set(n = n, file_only = FALSE, where = dataset, ...)
  if(is.character(dataset)) {
    blah <- setup_locker(dir = dataset, n = 0)
    class(ans) <- c("stream_locker", class(ans))
  }
  class(ans) <- unique(c("stream_file", class(ans)))
  if(is.character(format)) {
    ans <- stream_format(ans, format)  
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
  d <- paste0("Locker: ", is.stream_locker(x))
  e <- paste0("File[1]: ", x[[1]]$file)
  cat(a, c, b, e, d, sep = "\n")
}
