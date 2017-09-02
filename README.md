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

## Example 3
library(TMB)
TMB::runExample("simple")
obj <- MakeADFun(data=list(x=x, B=B, A=A),
    parameters=list(u=u*0, beta=beta*0, logsdu=1, logsd0=1),
    random=NULL, DLL="simple", silent=TRUE)
mod3 <- tmbstan_model(obj$par, obj$fn, obj$gr)
system.time(fit3 <- sampling(mod3, seed=1))

## Example 4
## (Speed up previous by not repeating the forward sweep)
f <- function(x)obj$env$f(x)
g <- function(x)obj$env$f(x,order=1,doforward=0)
mod4 <- tmbstan_model(obj$par, f, g)
system.time(fit4 <- sampling(mod4, seed=1))
```
