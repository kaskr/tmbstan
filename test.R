######################################################################
##
## Correctness tests against stan
##
######################################################################

library(rstan)
library(tmbstan)
verbose <- TRUE
compare <- function(x, y) {
    d <- as.array(x) - as.array(y)
    cat("MAX(ABS(DIFFERENCE)))\n")
    print(max(abs(d)))
    stopifnot(max(abs(d)) < sqrt(.Machine$double.eps))
    cat("PASSED\n")
}

######################################################################
##
## Example 1: IID normal
##
######################################################################

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
iter <- 50
fit1 <- sampling(mod1, seed=1, chains=1, iter=iter,
                 data = list(N = 5) )

fn <- function(x).5*sum(x*x)
gr <- function(x)x
par <- rep(c(x=0),5)
mod1. <- tmbstan_model(par, fn, gr)
fit1. <- sampling(mod1., seed=1, chains=1, iter=iter)

compare( fit1, fit1.)

######################################################################
##
## Example 2: IID normal constrained finite bounds
##
######################################################################

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
iter <- 50
fit2 <- sampling(mod2, seed=1, chains=1, iter=iter,
                 data = list() )

fn <- function(x).5*sum(x*x)
gr <- function(x)x
par <- rep(c(x=0),2)
mod2. <- tmbstan_model(par, fn, gr, lower=c(-2,-1), upper=c(0,1))
fit2. <- sampling(mod2., seed=1, chains=1, iter=iter)
compare( fit2, fit2.)

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
iter <- 50
fit3 <- sampling(mod3, seed=1, chains=1, iter=iter,
                 data = list() )

fn <- function(x).5*sum(x*x)
gr <- function(x)x
par <- rep(c(x=0),2)
mod3. <- tmbstan_model(par, fn, gr, lower=c(-2,-1), upper=c(0,Inf))
fit3. <- sampling(mod3., seed=1, chains=1, iter=iter)
compare( fit3, fit3.)

######################################################################
##
## Example 4: Test HMC for previous example
##
######################################################################

fit4 <- sampling(mod3, seed=1, chains=1, iter=iter,
                 data = list(),
                 algorithm="HMC")

fit4. <- sampling(mod3., seed=1, chains=1, iter=iter,
                  algorithm="HMC")

compare( fit4, fit4.)
