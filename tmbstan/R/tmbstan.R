setClass("tmbstanmodel",
         contains="stanmodel",
         slots = list(par="numeric", fn="function", gr="function",
                      lower="numeric", upper="numeric",ptr="externalptr")
)

tmbstan_model <- function(par, fn, gr, lower=numeric(0), upper=numeric(0)) {
    model_name <- "tmb_generic"
    model_code <- "tmb_generic"
    dso <- new("cxxdso")
    model_cppcode <- readLines(system.file("model.hpp", package="tmbstan"))
    mk_cppmodule <- function(x) stan_fit4model_tmb
    obj <- new("stanmodel",
               model_name = model_name,
               model_code = model_code,
               dso = dso,
               mk_cppmodule = mk_cppmodule,
               model_cpp = list(model_cppname = "dummy",
                                model_cppcode = model_cppcode))
    ans <- new("tmbstanmodel", obj)
    ans@par <- par
    ans@fn <- fn
    ans@gr <- gr
    stopifnot( length(lower) == length(upper) )
    stopifnot( length(lower) == 0 || length(lower) == length(par) )
    ans@lower <- lower
    ans@upper <- upper
    ans
}

setMethod("sampling", "tmbstanmodel",
          function(object, ...) {
              x <- numeric(length(object@par))
              fn <- object@fn
              gr <- object@gr
              R_callf <- quote(fn(x))
              R_callg <- quote(gr(x))
              lower <- object@lower
              upper <- object@upper
              have_bounds <- as.integer(length(lower) > 0)
              ## For names
              if (is.null(names(object@par)))
                  parnames <- rep("par", length(object@par))
              else
                  parnames <- names(object@par)
              shortpar_len <- table(factor(parnames, levels=unique(parnames)))
              shortpar_nam <- names(shortpar_len)
              env <- environment()
              .Call("set_pointers", x, R_callf, R_callg, env, object@ptr, PACKAGE="tmbstan")
              sampling(as(object, "stanmodel"),
                       data = list(N = length(x),
                                   have_bounds=have_bounds,
                                   lower_bound=lower,
                                   upper_bound=upper),...)
          })


## TMB interface to STAN
##
## init: Differs from rstan. If unspecified, the best encountered
## parameter is used (The MLE with corresponding posterior modes if an
## optimization has been carried out - otherwise simply obj$par). We also
## allow to pass a single numeric, or a list of numerics with same length
## as the number of chains. Numeric vectors should have the same length
## as the number of sampled parameters and names are ignored. Parameters
## that do not follow the previous scheme (e.g. characters) are passed on
## to rstan unchanged. If in doubt, use rstan::get_inits to inspect the
## applied initial values.
##
## marginal: Apply the Laplace approximation to 'random' subset of
## parameters ?

tmbstan <- function(obj,
                    lower=numeric(0), upper=numeric(0),
                    ...,
                    marginal=FALSE, silent=TRUE, debug=FALSE) {

    ## Cleanup 'obj' when we exit from this function:
    restore.on.exit <- c("last.par.best",
                         "random.start",
                         "value.best",
                         "last.par",
                         "inner.control",
                         "tracemgc")
    oldvars <- sapply(restore.on.exit, get, envir=obj$env, simplify=FALSE)
    restore.oldvars <- function(){
        for(var in names(oldvars)) assign(var, oldvars[[var]], envir=obj$env)
    }
    on.exit(restore.oldvars())

    if (silent) obj$env$beSilent()
    stopifnot(length(lower) == length(upper))
    if (marginal) {
        par <- obj$env$last.par.best[-obj$env$random]
        fn <- obj$fn
        gr <- obj$gr
        obj$env$random.start <- expression({
            cand <- list(last.par.best[random], last.par[random])
            cand[[which.min(sapply(cand, f0))]]
        })
    } else {
        par <- obj$env$last.par.best
        fn <- obj$env$f
        gr <- function(x) obj$env$f(x, order=1)
        if (length(lower) == length(obj$par) &&
            !is.null(obj$env$random)) {
            ## We allow lower/upper be shorter than the full par
            lower. <- lower; upper. <- upper
            lower <- par * 0 - Inf
            upper <- par * 0 + Inf
            lower[-obj$env$random] <- lower.
            upper[-obj$env$random] <- upper.
        }
    }
    mod <- tmbstan_model(par, fn, gr, lower, upper)
    if ( (!marginal) && (!debug) ) {
        mod@ptr <- obj$env$ADFun$ptr
    }

    ## Args for call to 'sampling'
    args <- list(object = mod, ...)

    ## Initialization of mcmc. Options:
    ##   1. Mode if available (last.par.best)
    ##   2. Sample from gaussian posterior
    ##   3. rstan defaults (rnorm I think ?)
    initSanitizer <- function(x) {
        if (is.list(x)) {
            x <- unlist(x) ## FIXME: We could do better by accounting for list names
            initSanitizer(x)
        }
        else if (is.numeric(x)) {
            if(length(x) != length(par))
                stop("Detected initial parameter of the wrong length.")
            init <- list()
            ## Workaround: rstan doesn't call 'transfom_inits' if
            ## none of the 'get_param_names' are present in the list
            init[[names(par)[1]]] <- numeric()
            ## Set the actual init parameter (Name 'y' is used by the generic model.hpp)
            init$y <- x
            init
        }
        else
            x
    }
    chains <- args$chains
    if (is.null(chains)) chains <- 4 ## rstan default
    if (is.null(args$init)) {
        ## Set our default
        args$init <- list(par)
    }
    if (!is.null(args$init)) {
        ## User specified initializer list - sanitize
        if (is.numeric(args$init))
            args$init <- list(args$init)
        if (is.list(args$init))
            args$init <- lapply(args$init, initSanitizer)
        if (is.list(args$init))
            args$init <- rep(args$init, length.out=chains)
    }

    do.call("sampling", args)
}
