data{
  int N;
  vector[N] y;
}

parameters{
  real<lower=0> mu;
  real<lower=0> shape;
}

model{
  for (i in 1:N){
    y[i] ~ gamma(shape, (shape / mu));
  }
}

generated quantities{
  real<lower=0> rate;
  vector[N] log_lik;

  rate = shape / mu;

  for (i in 1:N){
    log_lik[i] = gamma_lpdf(y[i]|shape, (shape / mu));
  }
}

