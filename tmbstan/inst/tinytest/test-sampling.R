library(tinytest)
library(RTMB)
library(tmbstan)

######################################################################
## Sample gamma and check the mean
######################################################################
n <- 10
f <- function(x) -sum(dgamma(x, 2, 1/(1:n), log=TRUE))
obj <- MakeADFun(f, 1:n)
s <- tmbstan(obj, lower=rep(0, n), upper=rep(Inf, n),
             seed=1, chains=1, iter=1e4)
mu <- 2 * (1:n)
muhat <- get_posterior_mean(s)[1:n]
expect_equal(mu, muhat, tol=0.2, info="Sample independent gamma")

######################################################################
## Sample chick weight random regression
######################################################################
## Data
data(ChickWeight)
## Parameters and random effects
parameters <- list(
    mua=0,          ## Mean slope
    sda=1,          ## Std of slopes
    mub=0,          ## Mean intercept
    sdb=1,          ## Std of intercepts
    sdeps=1,        ## Residual Std
    a=rep(0, 50),   ## Random slope by chick
    b=rep(0, 50)    ## Random intercept by chick
)
## Negative log likelihood
f <- function(parms) {
    getAll(ChickWeight, parms, warn=FALSE)
    ## Optional (enables extra RTMB features)
    weight <- OBS(weight)
    ## Initialize joint negative log likelihood
    nll <- 0
    ## Random slopes
    nll <- nll - sum(dnorm(a, mean=mua, sd=sda, log=TRUE))
    ## Random intercepts
    nll <- nll - sum(dnorm(b, mean=mub, sd=sdb, log=TRUE))
    ## Data
    predWeight <- a[Chick] * Time + b[Chick]
    nll <- nll - sum(dnorm(weight, predWeight, sd=sdeps, log=TRUE))
    ## Get predicted weight uncertainties
    ADREPORT(predWeight)
    ## Return
    nll
}
obj <- MakeADFun(f, parameters, random=c("a", "b"))
out <- tmbstan(obj, chains=1, seed=1)
## Result from running with iter=1e5:
expected <- c(8.46596098980211, 3.6052470808301, 29.0456997822677, 11.0220591012495,  12.9226467890375)
ans <- get_posterior_mean(out)[1:5]
expect_equal(ans, expected, tol=0.1, info="Sample random regression")
