##' @importFrom Rcpp loadModule
.onLoad <- function(libname, pkgname) { # nocov start
    loadModule("stan_fit4model_tmb_mod", what=TRUE)
}
