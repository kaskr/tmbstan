## Package tmbstan

[![Build Status](https://travis-ci.org/kaskr/tmbstan.svg?branch=master)](https://travis-ci.org/kaskr/tmbstan)

MCMC sampling from TMB model object using STAN.

## Install

```r
devtools::install_github("kaskr/tmbstan", subdir="tmbstan")
```

The development version of TMB is required

```r
devtools::install_github("kaskr/adcomp", subdir="TMB")
```

Shared object files must be re-compiled using the new version.

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
