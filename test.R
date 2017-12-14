######################################################################
##
## Correctness tests against stan
##
######################################################################

library(tmbstan)
verbose <- FALSE
compare <- function(x, y) {
    d <- as.array(x) - as.array(y)
    cat("MAX(ABS(DIFFERENCE)))\n")
    print(max(abs(d)))
    stopifnot(max(abs(d)) < sqrt(.Machine$double.eps))
    cat("PASSED\n")
}
iter <- 2000 ## rstan default
chains <- 4  ## rstan default
seed <- 1    ## Use identical seeds

######################################################################
##
## Example 1: IID normal
##
######################################################################

## Stan
stancode <- '
data {
int<lower=1> N;
}
parameters {
vector[N] y;
}
model {
y ~ normal(0,1);
}
'

mod1 <- stan_model(model_code = stancode, model_name="stan",
                   verbose=verbose)
fit1 <- sampling(mod1, seed=seed, chains=chains, iter=iter,
                 data = list(N = 5) )

## TMB
tmbcode <- '
#include <TMB.hpp>
template<class Type>
Type objective_function<Type>::operator() ()
{
  PARAMETER_VECTOR(y);
  return .5 * (y * y).sum();
}
'
if (!file.exists("tmbcode.cpp"))
    cat(tmbcode, file="tmbcode.cpp")
compile("tmbcode.cpp")
dyn.load(dynlib("tmbcode"))
obj <- MakeADFun(data=list(),
                 parameters=list(y=rep(0,5)),
                 DLL="tmbcode")
fit1. <- tmbstan(obj, seed=seed, chains=chains, iter=iter, init="random")

## Compare output
compare(fit1, fit1.)

######################################################################
##
## Example 1b: Specify inital values for previous example
##
######################################################################

fit1b <- sampling(mod1, seed=seed, chains=1, iter=iter,
                  data = list(N = 5),
                  init = list(list(y=c(-2:2))) )
fit1b. <- tmbstan(obj, seed=seed, chains=1, iter=iter,
                  init=list(list(c(-2:2))) )
compare(fit1b, fit1b.)

######################################################################
##
## Example 2: IID normal constrained finite bounds
##
######################################################################

## Stan
stancode <- '
parameters {
real<lower=-2,upper=0> y1;
real<lower=-1,upper=1> y2;
}
model {
y1 ~ normal(0,1);
y2 ~ normal(0,1);
}
'

mod2 <- stan_model(model_code = stancode, model_name="stan",
                   verbose=verbose)
fit2 <- sampling(mod2, seed=seed, chains=chains, iter=iter,
                 data = list() )

## TMB
obj <- MakeADFun(data=list(),
                 parameters=list(y=rep(0,2)),
                 DLL="tmbcode")
fit2. <- tmbstan(obj, seed=seed, chains=chains, iter=iter,
                 lower=c(-2,-1), upper=c(0,1), init="random")

## Compare output
compare(fit2, fit2.)

######################################################################
##
## Example 2b: Previous in parallel
##
######################################################################
fit2b <- sampling(mod2, seed=1, chains=2, iter=iter,
                  data = list(), cores=2)
fit2b. <- tmbstan(obj, seed=1, chains=2, iter=iter,
                  lower=c(-2,-1), upper=c(0,1), init="random",
                  cores=2)
compare(fit2b, fit2b.)

######################################################################
##
## Example 3: IID normal constrained mix of finite and infinite bounds
##
######################################################################

stancode <- '
parameters {
real<lower=-2,upper=0> y1;
real<lower=-1> y2;
}
model {
y1 ~ normal(0,1);
y2 ~ normal(0,1);
}
'

mod3 <- stan_model(model_code = stancode, model_name="stan",
                   verbose=verbose)
fit3 <- sampling(mod3, seed=seed, chains=chains, iter=iter,
                 data = list() )
## TMB
obj <- MakeADFun(data=list(),
                 parameters=list(y=rep(0,2)),
                 DLL="tmbcode")
fit3. <- tmbstan(obj, seed=seed, chains=chains, iter=iter,
                 lower=c(-2,-1), upper=c(0,Inf), init="random")

## Compare output
compare(fit3, fit3.)


######################################################################
##
## Example 3b: Specify inital values for previous example
##
######################################################################

fit3b <- sampling(mod3, seed=seed, chains=1, iter=iter,
                  data = list(N = 5),
                  init = list(list(y1=-1,y2=0)) )
fit3b. <- tmbstan(obj, seed=seed, chains=1, iter=iter,
                  lower=c(-2,-1), upper=c(0,Inf),
                  init=list(list(c(-1,0))) )
compare(fit3b, fit3b.)

######################################################################
##
## Example 4: Test HMC for previous example
##
######################################################################

fit4 <- sampling(mod3, seed=seed, chains=chains, iter=iter,
                 data = list(),
                 algorithm="HMC")

fit4. <- tmbstan(obj, seed=seed, chains=chains, iter=iter,
                 lower=c(-2,-1), upper=c(0,Inf), init="random",
                 algorithm="HMC")

## Compare output
compare(fit4, fit4.)

######################################################################
##
## Example 5: TMB's simple example implemented in Stan
##
######################################################################

runExample("simple")

data <- obj$env$data
data$A <- as.matrix(data$A)
data$B <- as.matrix(data$B)
data$ncolA <- ncol(data$A)
data$ncolB <- ncol(data$B)
data$nx <- length(data$x)

## Stan
stancode <- '
data {
int nx;
int ncolA;
int ncolB;
vector[nx] x;
matrix[nx,ncolA] A;
matrix[nx,ncolB] B;
}
parameters {
vector[ncolB] u;
vector[ncolA] beta;
real logsdu;
real logsd0;
}
model {
vector[nx] pred;
pred = A * beta + B * u;
u ~ normal(0,exp(logsdu));
x ~ normal(pred,exp(logsd0));
}
'
mod5 <- stan_model(model_code = stancode, model_name="stan",
                   verbose=verbose)

fit5 <- sampling(mod5, seed=1, chains=1, iter=10000,
                 data = data )

fit5. <- tmbstan(obj, seed=1, chains=1, iter=10000, init="random")

tab5  <- summary(fit5, pars=names(obj$par))$c_summary
tab5. <- summary(fit5.,pars=names(obj$par))$c_summary
diff <- (tab5-tab5.)[,c("mean", "sd", "25%", "50%", "75%"),]
print(diff)
stopifnot( max(abs(diff)) < .01 )
