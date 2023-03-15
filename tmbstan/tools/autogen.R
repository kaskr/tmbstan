## Auto generate generic model template for tmbstan
outfile <- "src/include/model.hpp"
modfile <- "src/Modules.cpp"
cpyfile <- "inst/model.hpp"

## ?stan_model
if (utils::packageVersion("rstan") < 2.26) {
  stan_file <- "inst/model.stan"
} else {
  stan_file <- "inst/model226.stan"
}
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
if (utils::packageVersion("rstan") < 2.26) {
  pattern <- "lp_accum__.add(normal_log<propto__>(y, 0, 1));"
} else if (utils::packageVersion("StanHeaders") >= 2.31) {
  pattern <- "lp_accum__.add(stan::math::normal_lpdf<propto__>(y, 0, 1));"
} else {
  pattern <- "lp_accum__.add(normal_lpdf<propto__>(y, 0, 1));"
}
replace <- "
lp_accum__.add(custom_func::custom_func(y));
"
searchReplace(pattern, replace)

if (utils::packageVersion("rstan") <= 2.21) {
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
  pattern <- "y = in__.vector_constrain(N, lp__);"
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
  pattern <- "Eigen::Matrix<double, Eigen::Dynamic, 1> y = in__.vector_constrain(N);"
  replace <- paste0("\nEigen::Matrix<double, Eigen::Dynamic, 1> y;\n", gsub(",[ ]*lp__","",replace)) ## Remove lp__ from previous
  searchReplace(pattern, replace)
  pattern <- "y = in__.vector_constrain(N);"
  replace <- "
  if(!have_bounds) {
  y = in__.vector_constrain(N);
  } else {
  y.resize(N);
  for (int j1__ = 0U; j1__ < N; ++j1__)
    y(j1__) = in__.scalar_lub_constrain(lower_bound(j1__), upper_bound(j1__));
  }
  "
  searchReplace(pattern, replace)
}


## Handle parameter names
if (utils::packageVersion("rstan") < 2.26) {
  pattern <- "names__.resize(0);"
} else if (utils::packageVersion("StanHeaders") >= 2.31) {
  pattern <- "names__ = std::vector<std::string>{\"y\"}"
} else {
  pattern <- "names__.clear()"
}
replace <- "
SEXP shortpar_nam = Rf_findVar(Rf_install(\"shortpar_nam\"), custom_func::R_env);
names__ = Rcpp::as<std::vector<std::string> >(shortpar_nam);
return;
"
searchReplace(pattern, replace)

if (utils::packageVersion("rstan") < 2.26) {
  pattern <- "dimss__.resize(0);"
} else if (utils::packageVersion("StanHeaders") >= 2.31) {
  pattern <- "dimss__ = std::vector<std::vector<size_t>>{std::vector<size_t>{static_cast<"
} else {
  pattern <- "dimss__.clear();"
}

replace <- "
SEXP shortpar_len = Rf_findVar(Rf_install(\"shortpar_len\"), custom_func::R_env);
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
if (utils::packageVersion("StanHeaders") >= 2.31) {
  mod <- gsub("\\b\\s*size_t>\\(N\\)}};", "", mod)
}

## Write
writeLines(mod, outfile)

## Need a copy in 'inst' folder
file.copy(outfile, cpyfile, overwrite=TRUE)


