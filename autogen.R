## Auto generate generic model template for tmbstan
outfile <- "tmbstan/src/include/model.hpp"
modfile <- "tmbstan/src/Modules.cpp"
cpyfile <- "tmbstan/inst/model.hpp"

library(rstan)

## ?stan_model
stancode <- '
data {
int<lower=1> N;
int<lower=0,upper=1> have_bounds;
vector[N*have_bounds] lower_bound;
vector[N*have_bounds] upper_bound;
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

## Modify objective
mod <- readLines(outfile)
searchReplace <- function(pattern, replace) {
    i <- grep(pattern, mod, fixed=TRUE)
    i <- setdiff(i, grep("====== Custom Edit Begin", mod))
    stopifnot( length(i) >= 1 )
    if(length(i) > 1) warning("More than one match; Using first")
    i <- i[1]
    mod[i] <<- paste0("\n// ====== Custom Edit Begin",
                      replace,
                      "// ====== Custom Edit End\n")
    NULL
}
pattern <- "lp_accum__.add(normal_log<propto__>(y, 0, 1));"
replace <- "
lp_accum__.add(custom_func(y));
"
searchReplace(pattern, replace)

## Handle bounds
pattern <- "writer__.vector_unconstrain(y);"
replace <- "
if (!have_bounds) {
  writer__.vector_unconstrain(y);
} else {
  for (int j1__ = 0U; j1__ < N; ++j1__)
    writer__.scalar_lub_unconstrain(lower_bound(j1__), upper_bound(j1__), y(j1__));
}
"
searchReplace(pattern, replace)
pattern <- "y = in__.vector_constrain(N,lp__);"
replace <- "
{
  if(!have_bounds) {
    y = in__.vector_constrain(N, lp__);
  } else {
    y.resize(N);
    for (int j1__ = 0U; j1__ < N; ++j1__)
      y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__), lp__);
  }
}
"
searchReplace(pattern, replace)
pattern <- "y = in__.vector_constrain(N);"
replace <- gsub(",[ ]*lp__","",replace) ## Remove lp__ from previous
searchReplace(pattern, replace)
pattern <- "vector_d y = in__.vector_constrain(N);"
replace <- "
vector_d y;
if(!have_bounds) {
  y = in__.vector_constrain(N);
} else {
  y.resize(N);
  for (int j1__ = 0U; j1__ < N; ++j1__)
    y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__));
}
"
searchReplace(pattern, replace)
## Handle parameter names
pattern <- "names__.resize(0);"
replace <- "
SEXP shortpar_nam = Rf_findVar(Rf_install(\"shortpar_nam\"), R_env);
names__ = Rcpp::as<std::vector<std::string> >(shortpar_nam);
return;
"
searchReplace(pattern, replace)
pattern <- "dimss__.resize(0);"
replace <- "
SEXP shortpar_len = Rf_findVar(Rf_install(\"shortpar_len\"), R_env);
for(int i=0; i<LENGTH(shortpar_len); i++) {
  std::vector<size_t> dims__;
  dims__.resize(0);
  dims__.push_back(INTEGER(shortpar_len)[i]);
  dimss__.push_back(dims__);
}
return;
"
searchReplace(pattern, replace)

## Write
writeLines(mod, outfile)

## Need a copy in 'inst' folder
file.copy(outfile, cpyfile, overwrite=TRUE)

## Write 'module code'
code <- rstan:::get_Rcpp_module_def_code("model_tmb")
cat("#include <Rcpp.h>\nusing namespace Rcpp;\n#include \"include/model.hpp\"\n",
    file=modfile)
cat(code, file=modfile, append=TRUE)
cat("\n", file=modfile, append=TRUE)
