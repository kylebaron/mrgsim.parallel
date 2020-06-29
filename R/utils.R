.nothing <- function(sims,mod) sims

#nocov start

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
#' @param mark when populated as a character label, adds a column to the 
#' chunked data frames with that name and with value the integer group number
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
chunk_by_id <- function(data,nchunk,id_col="ID",mark=NULL) {
  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame",call.=FALSE)  
  }
  if(!exists(id_col,data)) {
    stop(sprintf("chunking column %s does not exist in data", id_col),call.=FALSE)  
  }
  if(!is.numeric(nchunk)) {
    stop("nchunk must be numeric",call.=FALSE)  
  }
  if(!(nchunk > 0)) {
    stop("nchunk must be greater than zero",call.=FALSE)  
  }
  id <- data[[id_col]]
  ids <- unique(id)
  ntot <- length(ids)
  if(!(nchunk <= ntot)) {
    stop("nchunk must be <= number of IDs",call.=FALSE) 
  }
  nper <- ceiling(ntot/nchunk)
  a <- rep(seq(nchunk), each = nper, length.out = ntot)
  sp <- a[match(id,ids)]
  if(is.character(mark)) {
    data[[mark]] <- sp  
  }
  split.data.frame(data, sp)
}

#' @rdname chunk_data_frame
#' @export
chunk_by_row <- function(data,nchunk,mark=NULL) {
  if(!is.data.frame(data)) {
    stop("data argument must be a data.frame",call.=FALSE)  
  }
  if(!is.numeric(nchunk)) {
    stop("nchunk must be numeric",call.=FALSE)  
  }
  if(!(nchunk > 0)) {
    stop("nchunk must be greater than zero",call.=FALSE)  
  }
  if(!(nchunk <= nrow(data))) {
    stop("nchunk must be <= nrow(data)",call.=FALSE) 
  }
  ntot <- nrow(data)
  nper <- ceiling(ntot/nchunk)
  a <- rep(seq(nchunk), each = nper, length.out = ntot)
  if(is.character(mark)) {
    data[[mark]] <- a    
  }
  split.data.frame(data,a)
}

dat <- function(set = c("data", "idata", "data_big", "idata_big")) {
  set <- match.arg(set)
  file <- paste0(set,".RDS")
  file <- system.file("rmd",file,package = "mrgsim.parallel")
  readRDS(file)
}

mc_able <- function() {
  if(.Platform$OS.type=="windows") return(FALSE)
  return(isTRUE(getOption("mrgsim.parallel.mc.enable",TRUE)))
}
