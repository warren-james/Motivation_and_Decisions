data {

  int<lower = 1> N;                   // No. data points 
  int<lower = 1> K;                   // No. predictors
  int<lower = 0, upper = 1> y[N];     // dependent variable
  int<lower = 0> S;                   // No. Subjects
  int<lower = 0, upper = S> subj[N];  // Subj ID 
  matrix[N,K] X;                      // Matrix of predictors
 
} parameters {
  vector[S] subj_int;  // rand intercepts by participant
  vector[K] beta ;     // matrix of coefs

} transformed parameters {
  
  // varying intercepts
  // real alpha
  
  // to be estimated
  real mu;
  
  for(n in 1:N){
    mu = X[n,] * beta + subj_int[subj[n]];  
  }

} model {

  y ~ bernoulli_logit(mu);
  //y ~ bernoulli_logit(X * beta);
}
