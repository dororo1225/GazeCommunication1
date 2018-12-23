data{
  int N;       
  int K;       
  vector[N] y; 
}

parameters{
  simplex[K] pi;
  positive_ordered[K] mu;
  vector<lower=0>[K] shape;
}

model{
  real logprob[K];
  
  for (i in 1:N){
    for (j in 1:K){
      logprob[j] = log(pi[j]) + gamma_lpdf(y[i]|shape[j], (shape[j] / mu[j]));
    }
    target += log_sum_exp(logprob);
  }
}

generated quantities{
  vector<lower=0>[K] rate;
  vector[N] log_lik;
  real logprob[K];
  
  for (i in 1:N){
    for (j in 1:K){
      logprob[j] = log(pi[j]) + gamma_lpdf(y[i]|shape[j], (shape[j] / mu[j]));
    }
    log_lik[i] = log_sum_exp(logprob);
  }

  for (j in 1:K){
    rate[j] = shape[j] / mu[j];
  }
}
