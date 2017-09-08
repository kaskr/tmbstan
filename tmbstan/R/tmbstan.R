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
tmbstan <- function(obj,
                    lower=numeric(0), upper=numeric(0),
                    ...,
                    marginal=FALSE, silent=TRUE) {
    ## FIXME: Remember to restore.on.exit
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
        fn <- function() NULL
        gr <- function() NULL
        if (length(lower) == length(obj$par)) {
            ## We allow lower/upper be shorter than the full par
            lower. <- lower; upper. <- upper
            lower <- par * 0 - Inf
            upper <- par * 0 + Inf
            lower[-obj$env$random] <- lower.
            upper[-obj$env$random] <- upper.
        }
    }
    mod <- tmbstan_model(par, fn, gr, lower, upper)
    if (!marginal) {
        mod@ptr <- obj$env$ADFun$ptr
    }
    ## Initialization of mcmc. Options:
    ##   1. Mode if available (last.par.best)
    ##   2. Sample from gaussian posterior
    ##   3. rstan defaults (rnorm I think ?)
    init <- list()
    init[[names(par)[1]]] <- numeric()  # Workaround: rstan doesn't
                                        # call 'transfom_inits' if
                                        # none of the 'get_param_names'
                                        # are present in the list
    init$y <- par ## The actual init parameter
    fun_init <- function() init
    sampling(mod, init = fun_init, ...)
}
