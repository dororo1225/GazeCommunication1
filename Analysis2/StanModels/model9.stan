data{
  int N;                                        // number of rows in "df" 
  int N_pair;                                   // number of infant-mother pairs 
  int N_session;                                // number of EC sessions
  int<lower=0> Y[N];                            // number of EC bouts within EC session
  vector[N] X1;                                 // age in months
  vector<lower=0, upper=1>[N] X2;               // walker or not
  vector[N] X3;                                 // average distance within EC session
  vector[N] X4;                                 // (average distance within EC session)^2
  vector<lower=0, upper=1>[N] X5;               // infant-led EC bout or not
  vector[N] X6;                                 // infant-led EC bout * age in months
//  vector<lower=0, upper=1>[N] X7;               // infant-led EC bout * walker
//  vector[N] X8;                                 // infant-led EC bout * distance
//  vector[N] X9;                                 // infant-led EC bout * (distance)^2
  int<lower=1, upper=N_pair> ID_pair[N];        // infant-mother pair id 
  int<lower=1, upper=N_session> ID_session[N];  // EC session id
}

parameters{
  real beta0;                  // y_intercept;
  real beta1;                  // effect of age in months
  real beta2;                  // effect of walking
  real beta3;                  // effect of distance
  real beta4;                  // effect of distance^2
  real beta5;                  // effect of infant-lea EC bout
  real beta6;                  // effect of infant-led EC bout * age in months
//  real beta7;                  // effect of infant-led EC bout * walking
//  real beta8;                  // effect of infant-led EC bout * distance
//  real beta9;                  // effect of Infant-led EC bout * distance^2
  vector[N_session] r_session; // random intercepts for EC session 
  vector[N_pair] r_pair_int;   // random intercepts for infant-mother pairs
  vector[N_pair] r_pair_slp;   // random slopes for infant-mother pairs
  real<lower=0> sigma_session; // hyper paramter for EC session
  real<lower=0> sigma_pair;    // hyper paramter for infant-mother pairs
}

model{
    Y ~ poisson_log(beta0 + beta1 * X1 + beta2 * X2 + beta3 * X3 +  beta4 * X4 + (beta5 + r_pair_slp[ID_pair]) .* X5 + beta6 * X6 + r_pair_int[ID_pair] + r_session[ID_session]); 
    
    // random effects
    r_session ~ normal(0, sigma_session);
    r_pair_int ~ normal(0, sigma_pair);
    r_pair_slp ~ normal(0, sigma_pair);

    // weak informaation prior distribution
    sigma_pair ~ student_t(4, 0, 1);
}

generated quantities{
  vector[N] log_lik; //log-likelihood to calc WAIC

  for (j in 1:N){
    log_lik[j] = poisson_log_lpmf(Y[j] | beta0 + beta1 * X1[j] + beta2 * X2[j] + beta3 * X3[j] + beta4 * X4[j] + (beta5 + r_pair_slp[ID_pair[j]]) * X5[j] + beta6 * X6[j] + r_pair_int[ID_pair[j]] + r_session[ID_session[j]]);
  }
  
}
