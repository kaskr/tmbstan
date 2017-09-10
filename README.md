## Package tmbstan

Plugin custom function and gradient in STAN.

## Install

```r
devtools::install_github("kaskr/tmbstan", subdir="tmbstan")
```

## Examples

```r
## Example 1
library(tmbstan)
fn <- function(x).5*sum(x*x)
gr <- function(x)x
par <- rep(c(x=0),5)
mod1 <- tmbstan_model(par,fn,gr)
fit1 <- sampling(mod1, seed=1)

## Example 2
fn <- function(x)sum(abs(x))
gr <- function(x)sign(x)
par <- rep(0,5)
mod2 <- tmbstan_model(par,fn,gr)
fit2 <- sampling(mod2, seed=1)

## Example 3 (Simple tmb interface to stan)
library(tmbstan)
library(TMB)
TMB::runExample("simple")
system.time(fit <- tmbstan(obj))
```
