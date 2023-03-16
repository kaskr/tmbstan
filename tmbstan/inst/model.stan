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
