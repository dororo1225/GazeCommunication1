#### MODEL COMPARISON IN ANALYSIS 1
library(tidyverse)
library(rstan)
library(loo)
library(ggmcmc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
read_csv("Data_processed.csv", col_types = "cdiiiidcdi") %>% 
  mutate(ID_pair = as.numeric(as.factor(Name))) -> df

# prepare data list for stan model
data <- list(N = nrow(df),
             N_pair = length(unique(df$Name)),
             Y = df$N_Bout,
             X1 = df$AgeinMonths,
             X2 = if_else(df$LocomotorStatus == "walker", 1, 0),
             X3 = df$avg_Distance,
             X4 = (df$avg_Distance)^2,
             ID_pair = df$ID_pair)

# model fitting
## model1.stan
form1 <- c("Age + Walk + Dist + Dist^2")
model1 <- stan_model(file = 'StanModels/model1.stan')
fit1 <- sampling(model1,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model1.csv",
                 seed = 12345)
summary(fit1, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3)

## model2.stan
form2 <- c("Age + Walk + Dist")
model2 <- stan_model(file = 'StanModels/model2.stan')
fit2 <- sampling(model2,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model2.csv",
                 seed = 12345)
summary(fit2, pars = c("beta0", "beta1", "beta2", "beta3", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3)

## model3.stan
form3 <- c("Age + Dist + Dist^2")
model3 <- stan_model(file = 'StanModels/model3.stan')
fit3 <- sampling(model3,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model3.csv",
                 seed = 12345)
summary(fit3, pars = c("beta0", "beta1", "beta3", "beta4", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3)

## model4.stan
form4 <- c("Walk + Dist + Dist^2")
model4 <- stan_model(file = 'StanModels/model4.stan')
fit4 <- sampling(model4,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model4.csv",
                 seed = 12345)
summary(fit4, pars = c("beta0", "beta2", "beta3", "beta4", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3)

## model5.stan
form5 <- c("Age + Walk")
model5 <- stan_model(file = 'StanModels/model5.stan')
fit5 <- sampling(model5,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model5.csv",
                 seed = 12345)
summary(fit5, pars = c("beta0", "beta1", "beta2", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3)

## model6.stan
form6 <- c("Age + Dist")
model6 <- stan_model(file = 'StanModels/model6.stan')
fit6 <- sampling(model6,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model6.csv",
                 seed = 12345)
summary(fit6, pars = c("beta0", "beta1", "beta3", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3) 

## model7.stan
form7 <- c("Walk + Dist")
model7 <- stan_model(file = 'StanModels/model7.stan')
fit7 <- sampling(model7,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model7.csv",
                 seed = 12345)
summary(fit7, pars = c("beta0", "beta2", "beta3", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3)

## model8.stan
form8 <- c("Dist + Dist^2")
model8 <- stan_model(file = 'StanModels/model8.stan')
fit8 <- sampling(model8,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 sample_file = "StanResults/model8.csv",
                 seed = 12345)
summary(fit8, pars = c("beta0", "beta3", "beta4", "phi", "sigma_pair"))$summary %>%
  signif(digits = 3) 

# model comparison
## calculate WAIC
loglik1 <- extract(fit1)$log_lik
loglik2 <- extract(fit2)$log_lik
loglik3 <- extract(fit3)$log_lik
loglik4 <- extract(fit4)$log_lik
loglik5 <- extract(fit5)$log_lik
loglik6 <- extract(fit6)$log_lik
loglik7 <- extract(fit7)$log_lik
loglik8 <- extract(fit8)$log_lik
waic1 <- waic(loglik1)$estimate[3]
waic2 <- waic(loglik2)$estimate[3]
waic3 <- waic(loglik3)$estimate[3]
waic4 <- waic(loglik4)$estimate[3]
waic5 <- waic(loglik5)$estimate[3]
waic6 <- waic(loglik6)$estimate[3]
waic7 <- waic(loglik7)$estimate[3]
waic8 <- waic(loglik8)$estimate[3]

## model selection
data_frame(model = rep("model", times = 8)) %>% 
  mutate(model = str_c(model, row_number()),
         formula = c(form1, form2, form3, form4, form5, form6, form7, form8),
         waic = c(waic1, waic2, waic3, waic4, waic5, waic6, waic7, waic8)) %>% 
  arrange(waic)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}