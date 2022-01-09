#' Generate a sequence of file objects
#' 
#' @param n the number of file names / objects to create
#' @param tag a character prefix for the file name
#' @param where an optional file path
#' @param pad if `TRUE`, numbers will be padded with zeros
#' @param sep separator character
#' @param ext a file extension, including the dot
#' 
#' @return
#' A list length `n` of lists length 2; each sublist contains the integer 
#' file number as `i` and the file name as `file`. 
#' 
#' @examples
#' 
#' x <- file_set(3, where = "foo/bar")
#' length(x)
#' length(x[[1]])
#' x[[2]]$i
#' x[[3]]$file
#' 
#' @export
file_set <- function(n, tag = NULL, where = NULL, pad = TRUE, sep = "-", ext = "") {
  nn <- seq(n)
  if(isTRUE(pad)) {
    nn <- formatC(nn, width = floor(log10(n))+1, flag = "0")    
  }
  file <- paste0(nn, sep, nn[length(nn)], ext)
  if(is.character(tag)) {
    file <- paste0(tag, sep, file)  
  }
  if(is.character(where)) {
    file <- file.path(where, file)  
  }
  lapply(seq(n), function(i) list(i = i, file = file[i]))
}
