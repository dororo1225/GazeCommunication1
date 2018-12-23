##### MODEL COMPARISON IN ANALYSIS 2 (Step 3) #####
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

## model fitting (Step3)
### model9.stan
form9 <- c("Dist^2 + Walk + Dist + (1 + First)(1 + Age)")
model9 <- stan_model(file = 'StanModels/model1.stan')
fit9 <- sampling(model9,
                 data = data,
                 chains = 4,
                 iter = 6000,
                 warmup = 1000,
                 thin = 2,
                 # refresh = 0,
                 sample_file = "StanResults/model9.csv",
                 seed = 12345)
summary(fit9, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model10.stan
form10 <- c("Dist^2 + Walk + Age + (1 + First)(1 + Dist)")
model10 <- stan_model(file = 'StanModels/model10.stan')
fit10 <- sampling(model10,
                  data = data,
                  chains = 4,
                  iter = 6000,
                  warmup = 1000,
                  thin = 2,
                  refresh = 0,
                  sample_file = "StanResults/model10.csv",
                  seed = 12345)
summary(fit10, pars = c("beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)


### model11.stan
form11 <- c("Walk + (1 + First)(1 + Age + Dist)")
model11 <- stan_model(file = 'StanModels/model11.stan')
fit11 <- sampling(model11,
                  data = data,
                  chains = 4,
                  iter = 6000,
                  warmup = 1000,
                  thin = 2,
                  refresh = 0,
                  sample_file = "StanResults/model11.csv",
                  seed = 12345)
summary(fit11, pars = c("beta0", "beta1", "beta2", "beta3", "beta5", "beta6", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model12.stan
form12 <- c("Dist^2 + (1 + First)(1 + Age + Dist)")
model12 <- stan_model(file = 'StanModels/model12.stan')
fit12 <- sampling(model12,
                  data = data,
                  chains = 4,
                  iter = 6000,
                  warmup = 1000,
                  thin = 2,
                  refresh = 0,
                  sample_file = "StanResults/model12.csv",
                  seed = 12345)
summary(fit12, pars = c("beta0", "beta1", "beta3", "beta4", "beta5", "beta6", "beta8", "sigma_session", "sigma_pair"))$summary %>% 
  signif(digits = 3)

# model comparison
## caliculate WAIC
loglik9 <- extract(fit9)$log_lik
loglik10 <- extract(fit10)$log_lik
loglik11 <- extract(fit11)$log_lik
loglik12 <- extract(fit12)$log_lik

waic9 <- waic(loglik9)$estimate[3]
waic10 <- waic(loglik10)$estimate[3]
waic11 <- waic(loglik11)$estimate[3]
waic12 <- waic(loglik12)$estimate[3]

## model selected at Step2
form6 <- c("Dist^2 + Walk + (1 + First)(1 + Age + Dist)")
fit6 <- read_stan_csv(csvfiles = sprintf("StanResults/model6_%s.csv", 1:4))
loglik6 <- extract(fit6)$log_lik
waic6 <- waic(loglik6)$estimate[3]

## model selection
data_frame(model = rep("model", times = 4)) %>% 
  mutate(model = str_c(model, row_number() + 8),
         formula = c(form9, form10, form11, form12),
         waic = c(waic9, waic10, waic11, waic12)) %>% 
  add_row(model = "model6",
          formula = form6,
          waic = waic6) %>% 
  arrange(waic)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}