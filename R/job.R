

#' 
#' 
#' @export
job_mrgsim_d <- function(mod, data, tag, 
                         output_dir = getwd(), 
                         nchunk = 1,
                         chunk_by = "ID", 
                         format = "parquet", 
                         plan = "sequential", 
                         cores = 1, 
                         bg = FALSE, 
                         wait = FALSE,
                         seed = NULL, ...) {
  
  require_arrow()
  format <- match.arg(format)
  
  if(isTRUE(bg)) {
    ans <- bg_mrgsim_d(
      mod = mod,
      data = data, 
      nchunk = nchunk, 
      .format = format, 
      .plan = plan, 
      .cores = cores, 
      .wait = wait, 
      .seed = seed, 
      ...
    )
    return(ans)
  }
  
  path <- NULL  
  Plan <- list(workers = cores)
  
  if(!is.character(tag) || length(tag) > 1) {
    stop("`tag` must be character with length==1.")
  }
  
  if(is.character(plan)) {
    Plan$strategy <- plan
    if(cores==1) Plan$workers <- NULL
  }
  if(is.data.frame(data)) {
    if(nchunk <= 1) {
      data <- list(data)
    } else {
      data <- chunk_by_cols(data, nchunk = nchunk, cols = chunk_by)      
    }
  }
  if(!is.list(data)) {
    stop("`data` didn't resolve to list format.")  
  }
  
  ext <- ifelse(substr(format, 1, 1)=='.', format, paste0(".", format))
  locker_loc <- setup_locker(output_dir, tag = tag)
  output_paths <- file_set(
    n = length(data),
    prefix = "job", 
    ext = ext, 
    where = locker_loc
  )
  
  # if(length(data)==1) {
  #   #func <- bg_mrgsim_d_impl
  #   args <- list(...)
  #   args$mod <- mod
  #   args$data <- data[[1]]
  #   args$output <- output_paths[[1]]
  #   args$.seed <- seed
  #   args$.format <- format
  # } else {
  #   #func <- bg_mrgsim_apply
  #   args <- list()
  #   args$more <- list(mod = mod, ...)
  #   args$output <- output_paths
  #   args$data <- data
  #   args$Plan <- Plan
  #   args$.seed <- seed
  #   args$.format <- format
  # }
  if(is.numeric(seed)) {
    set.seed(seed)  
  }
  do.call(future::plan, Plan)
  ans <- future_mapply(
    FUN = bg_mrgsim_d_impl,
    data = data, 
    output = output_paths, 
    .format = format,
    MoreArgs = list(mod = mod, ...), 
    future.seed = TRUE
  )
  plan(sequential) 
  dirname(ans[[1]])
}
