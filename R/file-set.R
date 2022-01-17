new_file_object <- function(file, i) {
  list(i = i, file = file)  
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
