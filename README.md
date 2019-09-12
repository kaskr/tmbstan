## Package tmbstan

[![Build Status](https://travis-ci.org/kaskr/tmbstan.svg?branch=master)](https://travis-ci.org/kaskr/tmbstan)

MCMC sampling from [TMB](https://github.com/kaskr/adcomp/wiki) (Template Model Builder) model object using Stan. See

Monnahan CC, Kristensen K (2018) No-U-turn sampling for fast Bayesian inference in ADMB and TMB: Introducing the adnuts and tmbstan R packages. PLoS ONE 13(5): e0197954. https://doi.org/10.1371/journal.pone.0197954

## Requirements

TMB version `>= 1.7.12` (on CRAN). Models must be re-compiled using
this new version.

## Install

```r
devtools::install_github("kaskr/tmbstan/tmbstan")
```

## Examples

```r
library(tmbstan)
runExample("simple")

## Run a single chain in serial with defaults
fit <- tmbstan(obj, chains=1)

## Run in parallel with a init function
cores <- parallel::detectCores()-1
options(mc.cores = cores)
init.fn <- function()
  list(u=rnorm(114), beta=rnorm(2), logsdu=runif(1,0,10), logsd0=runif(1,0,1))
fit <- tmbstan(obj, chains=cores, open_progress=FALSE, init=init.fn)

## To explore the fit use shinystan
library(shinystan)
launch_shinystan(fit)

## Can also get ESS and Rhat from rstan::monitor
mon <- monitor(fit)
max(mon$Rhat)
min(mon$Tail_ESS)

## Other methods provided by 'rstan'
class(fit)
methods(class="stanfit")
## Pairs plot of the fixed effects
pairs(fit, pars=names(obj$par))
## Trace plot
traceplot(fit, pars=names(obj$par), inc_warmup=TRUE)

## Can extract marginal posteriors easily
post <- as.matrix(fit)
hist(post[,'u[1]'])                     # random effect
hist(post[,'logsd0'])                   # fixed effect

## What if you want a posterior for derived quantities in the report? Just
## loop through each posterior sample (row) and call the report function
## which returns a list. The last column is the log-posterior density (lp__)
## and needs to be dropped
obj$report(post[1,-ncol(post)])         # sd0 is only element
sd0 <- rep(NA, len=nrow(post))
for(i in 1:nrow(post)){
  r <- obj$report(post[i,-ncol(post)])
  sd0[i] <- r$sd0
}
hist(sd0)

## It is also possible to use the Laplace approximation to integrate the
## random effects while using NUTS to integrate the fixed effects.
## ****This is generally not recommended****
init.fn <- function()
  list(beta=rnorm(2), logsdu=runif(1,0,10), logsd0=runif(1,0,1))
fit <- tmbstan(obj, chains=cores, open_progress=FALSE,
               init=init.fn, laplace=TRUE)

## There are no posterior samples for the random effects because they are
## integrated out by the LA. See Monnahan and Kristensen (2019) for discussion
## of why this would be worth doing. Typically it will be slower and less
## accurate than laplace=FALSE (the default).
names(as.data.frame(fit))
```
