## Package tmbstan

[![Build Status](https://travis-ci.org/kaskr/tmbstan.svg?branch=master)](https://travis-ci.org/kaskr/tmbstan)

MCMC sampling from [TMB](https://github.com/kaskr/adcomp/wiki) (Template Model Builder) model object using STAN. See

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
fit <- tmbstan(obj, chains=1)

## Methods provided by 'rstan'
class(fit)
methods(class="stanfit")

## Pairs plot
pairs(fit, pars=names(obj$par))

## Trace plot
traceplot(fit, pars=names(obj$par), inc_warmup=TRUE)
```
