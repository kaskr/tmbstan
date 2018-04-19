##' Fix model parameters of TMB model object at MLE.
##' @param obj TMB model object.
##' @export
##' @return New TMB model object with zero-length \code{par} component.
##' @details
##' Assuming a model optimization has been carried out, this function
##' constructs a new model object with \code{obj$par} fixed at the best
##' encountered parameter. The resulting object has empty (zero-length)
##' \code{obj$par} but holds the same random effects as the original
##' object. Profiled parameters (see \code{profile} argument of
##' \code{?TMB::MakeADFun}) are also fixed.
fixAtMLE <- function(obj) {
    ## Args to construct copy of 'obj'
    args <- as.list(obj$env)[intersect(names(formals(MakeADFun)), ls(obj$env))]
    ## Determine parameter and full parameter to use
    r <- obj$env$random
    if ( is.null(r) )
        stop("No random effects")
    ## Assumption: Optimization has been carried out by user
    if (is.null(obj$env$last.par.best))
        stop("'last.par.best' not found.")
    parfull <- obj$env$last.par.best
    ## Get random effect indices (excluding profiled parameters)
    if(any(obj$env$profile))
        r <- r[ ! as.logical(obj$env$profile) ]
    ## Get names of random / non-random parameters
    names.random    <- unique(names(parfull[ r]))
    names.nonrandom <- unique(names(parfull[-r]))
    ## Use 'parfull' for new object
    args$parameters <- obj$env$parList(par = parfull)
    ## Fix all non-random parameters
    args$map <- lapply(args$parameters[names.nonrandom],
                       function(x) factor(x * NA))
    ## Find randomeffects character
    args$random <- names.random
    args$regexp <- FALSE
    ## Create new object
    newobj <- do.call("MakeADFun", args)
    newobj
}
