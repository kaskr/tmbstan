## Auto generate generic model template for tmbstan
outfile <- "tmbstan/src/include/model.hpp"
cpyfile <- "tmbstan/inst/model.hpp"

library(rstan)

## ?stan_model
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

mod <- stan_model(model_code = stancode, model_name="tmb", verbose=TRUE, obfuscate_model_name = FALSE)

cat("#define STAN__SERVICES__COMMAND_HPP\n#include <rstan/rstaninc.hpp>\n#include \"custom_func.hpp\"\n\n",
    file=outfile)

cat(mod@model_cpp$model_cppcode, file=outfile, append=TRUE)

## Modify
pattern <- "lp_accum__.add(normal_log<propto__>(y, 0, 1));"
replace <- "lp_accum__.add(custom_func(y));"
mod <- gsub(pattern, replace, readLines(outfile), fixed=TRUE)
writeLines(mod, outfile)

## Need a copy in 'inst' folder
file.copy(outfile, cpyfile)
