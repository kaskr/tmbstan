## Auto generate generic model template for tmbstan
outfile <- "src/include/model.hpp"
modfile <- "src/Modules.cpp"
cpyfile <- "inst/model.hpp"

## ?stan_model
stan_file <- "inst/model.stan"
mod <- rstan::stanc(stan_file, model_name="tmb", verbose=TRUE, obfuscate_model_name = FALSE)

cat("#define STAN__SERVICES__COMMAND_HPP\n#include <rstan/rstaninc.hpp>\n#include \"custom_func.hpp\"\n\n",
    file=outfile)

cat(mod$cppcode, file=outfile, append=TRUE)

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
pattern <- "lp_accum__.add(stan::math::std_normal_lpdf<propto__>(y));"
replace <- "
lp_accum__.add(custom_func::custom_func(y));
"
searchReplace(pattern, replace)

## Handle parameter names
pattern <- "names__ = std::vector<std::string>{\"y\"}"
replace <- "
SEXP shortpar_nam = R_getVar(Rf_install(\"shortpar_nam\"), custom_func::R_env, static_cast<Rboolean>(0));
names__ = Rcpp::as<std::vector<std::string> >(shortpar_nam);
return;
"
searchReplace(pattern, replace)

pattern <- "dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{static_cast<"
replace <- "
SEXP shortpar_len = R_getVar(Rf_install(\"shortpar_len\"), custom_func::R_env, static_cast<Rboolean>(0));
for(int i=0; i<LENGTH(shortpar_len); i++) {
  std::vector<size_t> dims__;
  dims__.resize(0);
  int len_i = INTEGER(shortpar_len)[i];
  if (len_i > 1) dims__.push_back(len_i);
  dimss__.push_back(dims__);
}
return;
"
searchReplace(pattern, replace)

# Part of the dimss__ declaration trails over to a second line under 2.31, cleanup:
mod <- gsub("\\b\\s*size_t>\\(N\\)}};", "", mod)

## Write
writeLines(mod, outfile)

## Need a copy in 'inst' folder
file.copy(outfile, cpyfile, overwrite=TRUE)
