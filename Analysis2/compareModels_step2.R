##### MODEL COMPARISON IN ANALYSIS 2 (Step 2) #####
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

## model fitting (Step2)
### model5.stan
form5 <- c("Dist^2 + Dist + (1 + First)(1 + Age + Walk)")
model5 <- stan_model(file = 'StanModels/model1.stan')
fit5 <- sampling(model5,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 # refresh = 0,
                 sample_file = "StanResults/model5.csv",
                 seed = 12345)
summary(fit5, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta7", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model6.stan
form6 <- c("Dist^2 + Walk + (1 + First)(1 + Age + Dist)")
model6 <- stan_model(file = 'StanModels/model6.stan')
fit6 <- sampling(model6,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model6.csv",
                 seed = 12345)
summary(fit6, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model7.stan
form7 <- c("Dist^2 + Age + (1 + First)(1 + Walk + Dist)")
model7 <- stan_model(file = 'StanModels/model7.stan')
fit7 <- sampling(model7,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model7.csv",
                 seed = 12345)
summary(fit7, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta7", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model8.stan
form8 <- c("(1 + First)(1 + Age + Walk + Dist)")
model8 <- stan_model(file = 'StanModels/model8.stan')
fit8 <- sampling(model8,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 refresh = 0,
                 sample_file = "StanResults/model8.csv",
                 seed = 12345)
summary(fit8, pars = c("beta0", "beta1", "beta2", "beta3", "beta5", "beta6", "beta7", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

# model comparison
## caliculate WAIC
loglik5 <- extract(fit5)$log_lik
loglik6 <- extract(fit6)$log_lik
loglik7 <- extract(fit7)$log_lik
loglik8 <- extract(fit8)$log_lik

waic5 <- waic(loglik5)$estimate[3]
waic6 <- waic(loglik6)$estimate[3]
waic7 <- waic(loglik7)$estimate[3]
waic8 <- waic(loglik8)$estimate[3]

## model selected at Step1
form2 <- c("Dist^2 + (1 + First)(1 + Age + Walk + Dist)")
fit2 <- read_stan_csv(csvfiles = sprintf("StanResults/model2_%s.csv", 1:4))
loglik2 <- extract(fit2)$log_lik
waic2 <- waic(loglik2)$estimate[3]

## model selection
data_frame(model = rep("model", times = 4)) %>% 
  mutate(model = str_c(model, row_number() + 4),
         formula = c(form5, form6, form7, form8),
         waic = c(waic5, waic6, waic7, waic8)) %>% 
  add_row(model = "model2",
          formula = form2,
          waic = waic2) %>% 
  arrange(waic)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}