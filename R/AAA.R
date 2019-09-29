#' @importFrom future.apply future_lapply
#' @importFrom dplyr bind_rows
#' @importFrom parallel mclapply
#' @importFrom mrgsolve loadso mrgsim_d mrgsim_ei
#' @importFrom utils assignInMyNamespace
NULL

#nocov start
mc_able <- TRUE

.onLoad <- function(libname,pkgname) {
    if(.Platform$OS.type=="windows") {
      assignInMyNamespace("mc_cable", FALSE)
    }
}

globalVariables(c("mod", "event"))

# nocov end
