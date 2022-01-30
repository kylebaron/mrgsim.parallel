#' @importFrom future.apply future_lapply future_mapply
#' @importFrom future plan sequential multicore multisession
#' @importFrom dplyr bind_rows as_tibble tibble
#' @importFrom parallel mclapply mcmapply
#' @importFrom mrgsolve loadso mrgsim_d mrgsim_ei house mrgsim_i mrgsim
#' @importFrom callr r_bg rcmd_safe_env
#' @importFrom fst read_fst write_fst
#' @importFrom tools file_path_sans_ext file_ext
#' @importMethodsFrom mrgsolve as.list
#' @importFrom utils assignInMyNamespace
NULL

#' Simulate with 'mrgsolve' in Parallel
#' 
#' @section Package options:
#' 
#' - `mrgsim.parallel.mc.able`: if `TRUE`, multicore will be used if appropriate.
#' 
#' @name mrgsim.parallel
#' @rdname mrgsim.parallel
NULL

#nocov start
#mc_able <- .Platform$OS.type!="windows"

globalVariables(c("mod", "event"))

.pkgenv <- new.env(parent = emptyenv())
stream_types <- c("fst", "feather", "qs", "rds")
stream_format_classes <- paste0("stream_format_", stream_types)
names(stream_format_classes) <- stream_types
.pkgenv$stream_format_classes <- stream_format_classes
.pkgenv$stream_types <- stream_types
# nocov end
