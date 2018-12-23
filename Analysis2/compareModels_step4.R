##### MODEL COMPARISON IN ANALYSIS 2 (Step 4) #####
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

## model fitting (Step4)
### model13.stan
form13 <- c("Dist^2 + Dist + (1 + First)(1 + Age)")
model13 <- stan_model(file = 'StanModels/model1.stan')
fit13 <- sampling(model13,
                  data = data,
                  chains = 4,
                  iter = 6000,
                  warmup = 1000,
                  thin = 2,
                  # refresh = 0,
                  sample_file = "StanResults/model13.csv",
                  seed = 12345)
summary(fit13, pars = c("beta0", "beta1", "beta3", "beta4", "beta5", "beta6", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model14.stan
form14 <- c("Dist^2 + Age + (1 + First)(1 + Dist)")
model14 <- stan_model(file = 'StanModels/model14.stan')
fit14 <- sampling(model14,
                  data = data,
                  chains = 4,
                  iter = 6000,
                  warmup = 1000,
                  thin = 2,
                  refresh = 0,
                  sample_file = "StanResults/model14.csv",
                  seed = 12345)
summary(fit14, pars = c("beta0", "beta1", "beta3", "beta4", "beta5", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

### model15.stan
form15 <- c("(1 + First)(1 + Age + Dist)")
model15 <- stan_model(file = 'StanModels/model15.stan')
fit15 <- sampling(model15,
                  data = data,
                  chains = 4,
                  iter = 6000,
                  warmup = 1000,
                  thin = 2,
                  refresh = 0,
                  sample_file = "StanResults/model15.csv",
                  seed = 12345)
summary(fit15, pars = c("beta0", "beta1", "beta3", "beta5", "beta6", "beta8", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3)

# model comparison
## caliculate WAIC
loglik13 <- extract(fit13)$log_lik
loglik14 <- extract(fit14)$log_lik
loglik15 <- extract(fit15)$log_lik

waic13 <- waic(loglik13)$estimate[3]
waic14 <- waic(loglik14)$estimate[3]
waic15 <- waic(loglik15)$estimate[3]

## model selected at Step3
form12 <- c("Dist^2 + (1 + First)(1 + Age + Dist) ")
fit12 <- read_stan_csv(csvfiles = sprintf("StanResults/model12_%s.csv", 1:4))
loglik12 <- extract(fit12)$log_lik
waic12 <- waic(loglik12)$estimate[3]

## model selection
data_frame(model = rep("model", times = 3)) %>% 
  mutate(model = str_c(model, row_number() + 12),
         formula = c(form13, form14, form15),
         waic = c(waic13, waic14, waic15)) %>% 
  add_row(model = "model12",
          formula = form12,
          waic = waic12) %>% 
  arrange(waic)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}