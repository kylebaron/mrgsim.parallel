# do_bg_mclapply <- function(X, FUN, mc.cores = 1, ...) {
#   lapply(X, FUN, ...)
# }
# 
# bg_mclapply <- function(X, FUN, mc.cores = 1, ..., simplify = FALSE) {
#   args <- list(...)
#   args$X <- X
#   args$FUN <- FUN
#   args$mc.cores <- mc.cores
#   ans <- callr::r_bg(
#     func = do_bg_mclapply,
#     args = args,
#     env = c(callr::rcmd_safe_env()), 
#     package = TRUE
#   )
#   ans
#   #ans$get_result()
# }

