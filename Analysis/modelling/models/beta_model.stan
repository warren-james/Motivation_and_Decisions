data {
  int<lower = 1> N;                      // No. data points 
  int<lower = 1> K;                      // No. predictors
  vector<lower = 0, upper = 1>[N] y;     // Dependant variable
  matrix[N,K] X;                         // Predictors
} parameters {
  vector[K] beta;                     // coefs for mu
  vector[K] gamma;                    // coefs for mu
} transformed parameters {
  vector<lower = 0, upper = 1>[N] mu;   // transformed linear predictor for mean
  vector<lower = 0>[N] phi;             // transformed linear predictor for spread
  vector<lower = 0>[N] A;               // parameter for beat dist 
  vector<lower = 0>[N] B;               // parameter for beat dist

  for (i in 1:N){
    mu[i] = inv_logit(X[i,] * beta);
    phi[i] = exp(X[i,] * gamma);
  }
  A = mu .* phi;
  B = (1.0 - mu) .* phi;
} model {
  // priors 
  beta[1] ~ normal(0, 0.5);
  beta[2] ~ normal(0, 0.5);

  gamma[1] ~ normal(0, 1);
  gamma[2] ~ normal(0, 1);

  // likelihood 
  y ~ beta(A, B);
}
