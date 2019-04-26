data {

  int<lower=1> N;               // number of data points

  int<lower=1> K;               // number of predictors

  vector<lower=0,upper=1>[N] y; // dependant variable
 
 matrix[N,K] X;                // predictors


  // priors 
  //vector[K] scale_beta;
  //vector[K] scale_gamma;

} parameters {


  vector[K] beta;  //coefs for mu

  vector[K] gamma; // coefs for phi


}

 transformed parameters {


  vector<lower=0,upper=1>[N] mu;    // transformed linear predictor for mean of beta distribution

  vector<lower=0>[N] phi;           // transformed linear predictor for precision of beta distribution

  vector<lower=0>[N] A;             // parameter for beta distn

  vector<lower=0>[N] B;             // parameter for beta distn


  
  for (i in 1:N) {

    mu[i]  = inv_logit(X[i,] * beta);
   
 phi[i] = exp(X[i,] * gamma);

  }



  A = mu .* phi;

  B = (1.0 - mu) .* phi;


}
 
model {

  // priors

  // beta ~ normal(0, scale_beta);


  // gamma ~ normal(0, scale_gamma);


  // likelihood

  y ~ beta(A, B);

}
