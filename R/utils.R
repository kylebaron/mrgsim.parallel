#nocov start
#' Functions to enable / disable forking with future
#'
#' @export
fork_ok <- function() {
  message("Setting 'R_FUTURE_FORK_ENABLE'; call 'fork_not_ok()' to undo.")
  Sys.setenv(R_FUTURE_FORK_ENABLE="true")
}
#' @rdname fork_ok
#' @export
fork_not_ok <- function() Sys.setenv(R_FUTURE_FORK_ENABLE="false")

# Wrapper function -----------------------------------------------


wrap_loadso <- function(mod,fun,...) {
  loadso(mod)
  fun(mod,...)
}
#nocov end

#' Chunk a data frame
#'
#' Use [chunk_by_id] to split up a data set by the `ID` column; use
#' [chunk_by_row] split a data set by rows.
#'
#' @param data a data frame
#' @param nchunk number of chunks
#' @param id_col character specifying the column containing the `ID` for
#' chunking
#'
#' @return A list of data frames
#'
#' @examples
#' x <- expand.grid(ID = 1:10, B = rev(1:10))
#'
#' chunk_by_id(x, 3)
#'
#' chunk_by_row(x, nchunk = 4)
#'
#' @name chunk_data_frame
#' @export
chunk_by_id <- function(data,nchunk,id_col = "ID") {
  id <- data[[id_col]]
  ids <- unique(id)
  ntot <- length(ids)
  nper <- ceiling(ntot/nchunk)
  a <- rep(seq(nchunk), each = nper, length.out = ntot)
  ind <- match(id,ids)
  split(data, a[match(id,ids)])
}

#' @rdname chunk_data_frame
#' @export
chunk_by_row <- function(data, nchunk) {
  ntot <- nrow(data)
  nper <- ceiling(ntot/nchunk)
  a <- rep(seq(nchunk), each = nper, length.out = ntot)
  split(data,a)
}

