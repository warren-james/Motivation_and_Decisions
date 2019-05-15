data {

  int<lower = 1> N;                // No. data points 
  int<lower = 1> K;                // No. predictors
  int<lower = 0, upper = 1> y[N];  // dependent variable
  matrix[N,K] X;                   // Matrix of predictors
 
} parameters {

  vector[K] beta ;  // coefs for mu

} transformed parameters {

  vector<lower = 0, upper = 1>[N] mu; // transformed predictor for mu

  for(i in 1:N){
    mu[i] = inv_logit(X[i,] * beta);
  }  

} model {

  for(n in 1:N){
    y[n] ~ bernoulli_logit(mu);
  }

}
