functions {
  vector make_bounds(vector bound_in, int N, int is_upper) {
    if (num_elements(bound_in) == 0) {
      real bound = (is_upper == 1 ? positive_infinity() : negative_infinity());
      return rep_vector(bound, N);
    } else {
      return bound_in;
    }
  }
}
data {
  int<lower=1> N;
  int<lower=0,upper=1> have_bounds;
  vector[N*have_bounds] lower_bound;
  vector[N*have_bounds] upper_bound;
}
parameters {
  vector<lower=make_bounds(lower_bound, N, 0),
          upper=make_bounds(upper_bound, N, 1)>[N] y;
}
model {
  y ~ normal(0,1);
}
