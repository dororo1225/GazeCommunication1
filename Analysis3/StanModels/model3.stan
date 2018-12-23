data{
  int N;                                  // number of EC sessions
  int N_pair;                             // number of infant-mother pairs 
  int<lower=0> N_bout[N];                 // numeber of EC bouts within EC session
  int<lower=0> Y[N];                      // number of infant-led EC bouts within EC session
  vector[N] X1;                           // age in months
//  vector<lower=0, upper=1>[N] X2;         // walker or not
  vector[N] X3;                           // average distance within EC session
  vector[N] X4;                           // (average distance within EC session)^2
  int<lower=1, upper=N_pair> ID_pair[N];  // infant-mother pair id 
}

parameters{
  real beta0;                  // y_intercept;
  real beta1;                  // effect of age in months
//  real beta2;                  // effect of walking
  real beta3;                  // effect of distance
  real beta4;                  // effect of distance^2
  vector[N] r_session;         // random intercepts for EC sessions
  vector[N_pair] r_pair;       // random intercepts for infant-mother pairs
  real<lower=0> sigma_session; // hyper paramter
  real<lower=0> sigma_pair;    // hyper paramter
}

model{
    Y ~ binomial_logit(N_bout, beta0 + beta1 * X1 + beta3 * X3 + beta4 * X4 + r_pair[ID_pair] + r_session); 
    
    // random effects
    r_session ~ normal(0, sigma_session);
    r_pair ~ normal(0, sigma_pair);
    
    // weak informaation prior distribution
    sigma_pair ~ student_t(4, 0, 1);
}

generated quantities{
  vector[N] log_lik; //log-likelihood to calc WAIC

  for (j in 1:N){
    log_lik[j] = binomial_logit_lpmf(Y[j] | N_bout[j], beta0 + beta1 * X1[j] + beta3 * X3[j] + beta4 * X4[j] + r_pair[ID_pair[j]] + r_session[j]);
  }
}
