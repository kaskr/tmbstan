##' @importClassesFrom rstan stanmodel
##' @importFrom methods setClass new
setClass("tmbstanmodel",
         contains="stanmodel",
         slots = list(par="numeric", fn="function", gr="function",
                      lower="numeric", upper="numeric",
                      ptr="externalptr", DLL="character")
)

## ##' @importClassesFrom inline cxxdso
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

##' @importMethodsFrom rstan sampling
##' @importFrom methods selectMethod
##' @useDynLib tmbstan, .registration=TRUE
setMethod("sampling", "tmbstanmodel",
          function(object, data=NULL, ...) {
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
              .Call("set_pointers", x, R_callf, R_callg, env,
                    object@ptr, object@DLL, PACKAGE="tmbstan")
              ## ===============================================================
              ## Parallelization on Windows requires:
              ##   1.  library(tmbstan) loaded on nodes
              ##   2.  DLL loaded on nodes
              ##   3.  'sampling.stanmodel'    bypassed by
              ##       'sampling.tmbstanmodel' on nodes
              ##
              ## (1)
              oldprof <- Sys.getenv("R_PROFILE")
              tmpfile <- tempfile()
              cat("library(tmbstan)\n", file=tmpfile)
              ## (2)
              cat(paste0("dyn.load('",
                         unclass(getLoadedDLLs()[[environment(fn)$DLL]])$path,
                         "')\n"), file=tmpfile, append=TRUE)
              Sys.setenv(R_PROFILE = tmpfile)
              on.exit( { Sys.setenv(R_PROFILE = oldprof); file.remove(tmpfile) } )
              ## (3)
              sampling <- selectMethod("sampling", "stanmodel")
              ## ===============================================================
              sampling(object, ## class="tmbstanmodel" !
                       data = list(N = length(x),
                                   have_bounds=have_bounds,
                                   lower_bound=lower,
                                   upper_bound=upper),...)
          })


##' Draw MCMC samples from a TMB model object using Stan
##' @param obj TMB model object.
##' @param ... Passed to \code{rstan::sampling} with some modifications - see details.
##' @param lower Vector of lower parameter bounds.
##' @param upper Vector of upper parameter bounds.
##' @param laplace Apply the Laplace approximation to \code{random} subset of parameters ? The default disables the Laplace approximation.
##' @param silent Be silent during samling ?
##' @param debug Should not be used.
##' @export
##' @return Object of class \code{stanfit}
##' @details
##' \code{tmbstan} works for models with or without random effects.
##'
##' By default a full Bayesian analysis is carried out, i.e. both
##' parameters and random effects are sampled using MCMC. Models with
##' random effects will thus have the Laplace approximation disabled. It
##' is possible to mix the Laplace approximation with MCMC by setting
##' \code{laplace=TRUE}.
##' All methods provided by the \code{rstan} package can be applied to a
##' fitted object. Get a complete list using
##' \code{methods(class="stanfit")}.
##'
##' Lower and upper bounds can be set using \code{lower} and \code{upper}.
##' The bounds can be specified in one of two ways. Either in short
##' format, i.e. have the same length as \code{obj$par}. Remaining
##' parameters (the random effects) are set as unbounded in this case.
##' Otherwise the bounds must be in long format, i.e. have the same
##' length as the full parameter vector \code{obj$env$par} including the
##' random effects.
##' In both cases \code{-Inf} and \code{Inf} are valid components of
##' \code{lower} and \code{upper} respectively.
##' Note that initial values must be within the specified bounds.
##'
##' The function arguments \code{...} are passed to \code{rstan}'s
##' fitting function, see \code{?rstan::sampling}.
##' A few notable arguments are:
##' \itemize{
##' \item \code{chains} The number of chains.
##' \item \code{iter}   The number of iterations.
##' \item \code{init}   Initial values for the sampler.
##' Behaves like \code{rstan} with some additions:
##' \itemize{
##' \item Default is \code{"random"} - see \code{?stan}.
##' \item Special values \code{0} and \code{"0"} are allowed - see \code{?stan}.
##' \item Additional special characters \code{"par"} and \code{"last.par.best"} are allowed
##'       and will be looked up in the TMB model object.
##'       The value \code{"par"} signifies to start from the defaults of the model object.
##'       If an optimization has been carried out, the intial value \code{"last.par.best"}
##'       will start from the MLE.
##' \item We also allow to pass a single numeric vector, or a list of numeric vectors.
##'       List length must match the number of chains. Vector lengths must match the
##'       number of sampled parameters. Names are currently ignored.
##' \item Parameters
##' that do not follow the previous scheme (e.g. characters) are passed on
##' to \code{rstan} unchanged. If in doubt, use \code{rstan::get_inits} to inspect the
##' applied initial values.
##' }
##' \item \code{seed} Random seed.
##' }
##' @importFrom TMB runExample
##' @examples
##' runExample("simple")
##' fit <- tmbstan(obj, chains=1)
##' class(fit)  ## "stanfit"
##'
##' ## The available methods are
##' methods(class="stanfit")
##'
##' \dontrun{
##' ## Pairs plot
##' pairs(fit, pars=names(obj$par))
##' }
##'
##' ## Trace plot
##' traceplot(fit, pars=names(obj$par), inc_warmup=TRUE)
tmbstan <- function(obj,
                    ...,
                    lower=numeric(0), upper=numeric(0),
                    laplace=FALSE, silent=TRUE, debug=FALSE) {

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
    if (laplace) {
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
    if ( (!laplace) && (!debug) ) {
        mod@ptr <- obj$env$ADFun$ptr
        mod@DLL <- obj$env$DLL
    }

    ## Args for call to 'sampling'
    args <- list(object = mod, ...)

    ## Set rstan default chains
    chains <- args$chains
    if (is.null(chains)) chains <- 4

    ## Handle initial values
    shortParNames <- unique(names(par))
    shortParLengths <- table(factor(names(par), levels=shortParNames))
    specialChars <- c("par", "last.par", "last.par.best")
    initSanitizer <- function(x) {
        if (is.list(x)) {
            ## Permute list
            x <- x[shortParNames]
            names(x) <- shortParNames ## Some names coud be NA otherwise
            ## Check lengths of list components
            spl <- sapply(x, length)
            if (!identical( as.vector(spl), as.vector(shortParLengths) )) {
                cat("Expected parameter lengths:\n")
                print(shortParLengths)
                cat("Got:\n")
                print(spl)
                stop("Wrong length of list component")
            }
            x <- unlist(x)
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
        else if (is.character(x) && (x %in% specialChars) ) {
            x <- get(x, envir=obj$env)
            initSanitizer(x)
        }
        else
            x
    }
    if (is.null(args$init)) {
        ## rstan default
        args$init <- "random"
    } else {
        ## Get
        init <- args$init
        ## From rstan's sampling function:
        if (is.numeric(init) && length(init == 1))
            init <- as.character(init)
        if (is.function(init)) {
            if ("chain_id" %in% names(formals(init)))
                init <- lapply(1:chains, FUN = init)
            else init <- lapply(1:chains, function(id) init())
        }
        ## User specified initializer list - sanitize
        if (is.numeric(init))
            init <- list(init)
        ## E.g. init="last.par.best"
        if (is.character(init) && any(init %in% specialChars))
            init <- as.list(init)
        if (is.list(init))
            init <- lapply(init, initSanitizer)
        if (is.list(init) && ( length(init) != chains ) ) {
            warning("Re-cycling inits to match number of chains")
            init <- rep(init, length.out=chains)
        }
        ## Set
        args$init <- init
    }

    ans <- do.call("sampling", args)
    ans@model_name <- obj$env$DLL
    ans
}
