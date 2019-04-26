data {
  int<lower = 0> N;                        // number of data points
  int<lower = 0, upper = 1> optimal[N];    // predictor optimal
  int<lower = 0, upper = 1> motivated[N];  // predictor motivated
  int<lower = 0, upper = 1> dist_far[N];   // predictor dist_far
  int<lower = 0, upper = 1> correct[N];   // dependent variable
}
parameters {
  real c;      // intercept
  real b_f;    // dist_far
  real b_m;    // motivated
  real b_o;    // optimal
  real b_mf;   // motivated_far
  real b_of;   // optimal_far
}
model {
  // priors
  c ~ normal(0,1);
  b_f ~ normal(0,1);
  b_m ~ normal(0,1);
  b_o ~ normal(0,1);
  b_mf ~ normal(0,1);
  b_of ~ normal(0,1);

  // likelihood
  for(n in 1:N)
      correct[n] ~ bernoulli_logit(2 * fmax(0.5, b_f * dist_far[n] + 
                                             b_m * motivated[n] +
                                             b_o * optimal[n] +
                                             b_mf * motivated[n] * dist_far[n] +
					     b_of * optimal[n] * dist_far[n] + 
                                             c) - 1);
}
