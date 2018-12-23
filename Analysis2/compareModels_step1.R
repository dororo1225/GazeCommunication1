##### MODEL COMPARISON IN ANALYSIS 2 (Step 1) #####
library(tidyverse)
library(rstan)
library(loo)
library(ggmcmc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
read_csv("Data_processed.csv", col_types = "cdiiiidcdi") %>% 
  mutate(ID_pair = as.numeric(as.factor(Name)),
         ID_session = row_number()) %>% 
  gather(Initiator, count, N_Infant, N_Mother) %>% 
  mutate(Initiator = str_to_lower(str_sub(Initiator, start = 3, end = -1))) %>% 
  arrange(Name, AgeinMonths, ID_session) -> df

# prepare data list for stan model
data <- list(N = nrow(df),
             N_pair = length(unique(df$Name)),
             N_session = length(unique(df$ID_session)),
             Y = df$count,
             X1 = df$AgeinMonths,
             X2 = if_else(df$LocomotorStatus == "walker", 1, 0),
             X3 = df$avg_Distance,
             X4 = (df$avg_Distance)^2,
             X5 = if_else(df$Initiator == "infant", 1, 0),
             X6 = if_else(df$Initiator == "infant", 1, 0) * df$AgeinMonths,
             X7 = if_else(df$Initiator == "infant", 1, 0) * if_else(df$LocomotorStatus == "walker", 1, 0),
             X8 = if_else(df$Initiator == "infant", 1, 0) * df$avg_Distance,
             X9 = if_else(df$Initiator == "infant", 1, 0) * (df$avg_Distance)^2,
             ID_pair = df$ID_pair,
             ID_session = df$ID_session)

## model fitting (Step1)
### model1.stan
form1 <- c("(1 + First)(1 + Age + Walk + Dist + Dist^2)")
model1 <- stan_model(file = 'StanModels/model1.stan')
fit1 <- sampling(model1,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model1.csv",
                 seed = 12345)
summary(fit1, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "beta9", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model2.stan
form2 <- c("Dist^2 + (1 + First)(1 + Age + Walk + Dist)")
model2 <- stan_model(file = 'StanModels/model2.stan')
fit2 <- sampling(model2,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model2.csv",
                 seed = 12345)
summary(fit2, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model3.stan
form3 <- c("Walk + (1 + First)(1 + Age + Dist + Dist^2)")
model3 <- stan_model(file = 'StanModels/model3.stan')
fit3 <- sampling(model3,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model3.csv",
                 seed = 12345)
summary(fit3, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta8", "beta9", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model4.stan
form4 <- c("Age + (1 + First)(1 + Walk + Dist + Dist^2)")
model4 <- stan_model(file = 'StanModels/model4.stan')
fit4 <- sampling(model4,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model4.csv",
                 seed = 12345)
summary(fit4, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta7", "beta8", "beta9", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

# model comparison
## caliculate WAIC
loglik1 <- extract(fit1)$log_lik
loglik2 <- extract(fit2)$log_lik
loglik3 <- extract(fit3)$log_lik
loglik4 <- extract(fit4)$log_lik

waic1 <- waic(loglik1)$estimate[3]
waic2 <- waic(loglik2)$estimate[3]
waic3 <- waic(loglik3)$estimate[3]
waic4 <- waic(loglik4)$estimate[3]

## model selection
data_frame(model = rep("model", times = 4)) %>% 
  mutate(model = str_c(model, row_number()),
         formula = c(form1, form2, form3, form4),
         waic = c(waic1, waic2, waic3, waic4)) %>% 
  arrange(waic)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}