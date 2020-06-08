#' @importFrom future.apply future_lapply
#' @importFrom dplyr bind_rows
#' @importFrom parallel mclapply
#' @importFrom mrgsolve loadso mrgsim_d mrgsim_ei house mrgsim_i
#' @importFrom utils assignInMyNamespace
NULL

#nocov start
mc_able <- .Platform$OS.type!="windows"

globalVariables(c("mod", "event"))

# nocov end
