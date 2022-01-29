#' List all output files in a fst file set
#' 
#' Use the function to read all of the `.fst` files that were saved when 
#' `bg_mrgsim_d` was called and `.path` was passed along with `.format = "fst"`.
#' 
#' @param path The (full) directory path to search.
#' 
#' @export
list_fst <- function(path) {
  list.files(
    path, 
    full.names = TRUE, 
    pattern = ".*\\.fst$"
  )
}

#' Get the contents of an fst file set
#' 
#' @inheritParams head_fst
#' @param .as_list Should the results be returned as a list (`TRUE`) or a 
#' tibble (`FALSE`).
#' @param ... Not used.
#' 
#' @seealso [list_fst()], [head_fst()]
#' 
#' @export
internalize_fst <- function(path, .as_list = FALSE, ...) {
  files <- list_fst(path)
  ans <- lapply(files, read_fst)
  if(isTRUE(.as_list)) {
    return(ans)  
  }
  as_tibble(bind_rows(ans), .name_repair = "universal") 
}
#' @export
#' @rdname internalize_fst
get_fst <- internalize_fst

#' Get the head of an fst file set
#' 
#' @param path The directory to search.
#' @param n Number of rows to show.
#' @param i Which output output chunk to show. 
#' 
#' @seealso [get_fst()], [list_fst()]
#' 
#' @export
head_fst <- function(path, n = 5, i = 1) {
  x <- list_fst(path)
  read_fst(x[i], from = 1, to = n)
}
