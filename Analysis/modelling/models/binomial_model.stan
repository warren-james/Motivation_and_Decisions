data {
  int<lower = 1> N;              // No. data points 
  int<lower = 1> K;              // No. predictors
  int y[N];                      // Dependant variable
  matrix[N,K] X;                 // Predictors

} parameters {
  real alpha;       // intercept
  vector[K] beta;  // coef of predictors

} transformed parameters {
  vector[N] eta;
  eta = alpha + X * beta;

} model {
  // priors 
  alpha ~ normal(0,1);

  // likelihood 
  y ~ bernoulli_logit(2 * fmax(0, eta) - 1);
}
