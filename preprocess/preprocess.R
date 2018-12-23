#### PREPROCESS RAWDATA
library(tidyverse)
library(rstan)
library(ggmcmc)
library(loo)
library(here)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load rawdata
df <- read_csv("Data_raw.csv", col_types = "cdcciiiiccdc")

# calculate inter-eye-cotact-bout interval
df %>% 
  group_by(Video) %>% 
  mutate(EC_start_next = lead(EC_start)) %>% 
  ungroup() %>% 
  mutate(interval = (EC_start_next - EC_end) / 1000) %>% 
  filter(!is.na(interval)) -> df_interval

# prepare data list for stan model
data <- list(N = nrow(df_interval),
             K = 2,
             y = df_interval$interval)

# fit mixed-Gamma distribution model to data
## model fitting
model1 <- stan_model(file = "StanModels/model1.stan")
fit1 <- sampling(model1,
                 data = data,
                 chains = 4,
                 iter = 5500,
                 warmup = 500,
                 thin = 2,
                 sample_file = "StanResults/model1.csv",
                 seed = 12345)

## posterior distribution of each parameter 
summary(fit1, pars = c("mu", "shape", "rate", "pi"))$summary %>% 
  signif(digits = 3)

# fit simple Gamma distribution model to data
## model fitting
model2 <- stan_model(file = "StanModels/model2.stan")
fit2 <- sampling(model2,
                 data = data,
                 chains = 4,
                 iter = 5500,
                 warmup = 500,
                 thin = 2,
                 sample_file = "StanResults/model2.csv",
                 seed = 12345)

## posterior distribution of each parameter 
summary(fit2, pars = c("mu", "shape", "rate"))$summary %>%
  signif(digits = 3)

# model comparison
## mixed-Gamma distribution model
log_lik1 <- extract_log_lik(fit1)
waic1 <- waic(log_lik1)
print(waic1 , digits = 4)

## simple Gamma distribution model
log_lik2 <- extract_log_lik(fit2)
waic2 <- waic(log_lik2)
print(waic2 , digits = 4)

# define the threshold of inter-eye-contact-bout interval
ggs(fit1) %>% 
  filter(str_detect(.$Parameter, pattern = "shape") | str_detect(.$Parameter, pattern = "rate") | str_detect(.$Parameter, pattern = "pi")) %>% 
  group_by(Parameter) %>% 
  summarise(EAP = mean(value)) %>% 
  mutate(id = str_extract(.$Parameter, pattern = "[[:digit:]]+"),
         Parameter = str_extract(.$Parameter, pattern = "[[:alpha:]]+")) %>% 
  spread(Parameter, EAP) -> df_fit1

threshold <- function(s1, r1, p1, s2, r2, p2, lwr, upr){
  optimize(function(x) (p1 * dgamma(x, s1, r1) - p2 * dgamma(x, s2, r2))^2, interval = c(lwr, upr))
}

thr <- threshold(s1 = df_fit1$shape[1],
                 r1 = df_fit1$rate[1],
                 p1 = df_fit1$pi[1],
                 s2 = df_fit1$shape[2],
                 r2 = df_fit1$rate[2],
                 p2 = df_fit1$pi[2],
                 lwr = df_fit1$shape[1] / df_fit1$rate[1],
                 upr = df_fit1$shape[2] / df_fit1$rate[2])$minimum

# define EC session with the threshold
df %>% 
  filter(Constrain_infant != "constrained", Posture_mother == "sit") %>%
  group_by(Video) %>% 
  mutate(EC_start_next = lead(EC_start),
         interval = (EC_start_next - EC_end) / 1000,
         NextECisOver = if_else(interval > thr, 1, 0), 
         NewSession = lag(NextECisOver), 
         NewSession = if_else(is.na(NewSession), 1, NewSession)) %>% 
  ungroup() %>% 
  group_by(Observation) %>% 
  mutate(SessionID = cumsum(NewSession)) %>%  
  ungroup() %>%   
  select(Name, AgeinMonths, Observation, Video, SessionID, BoutID, interval, Video, Initiator, Distance, Constrain_infant, Posture_mother) -> df_session

# remove data based on some condition
df_session %>% 
  filter(!is.na(Distance) , !is.na(Initiator), Initiator != "same") %>% 
  group_by(Name, AgeinMonths, SessionID) %>%
  summarise(N_Infant = sum(Initiator == "infant"),
            N_Mother = sum(Initiator == "mother"),
            N_Bout = n(),
            avg_Distance = mean(Distance)) %>%
  ungroup() %>%
  mutate(avg_Distance_scale = as.numeric(scale(avg_Distance, center = TRUE, scale = TRUE))) %>% 
  filter(avg_Distance_scale > -3, avg_Distance_scale < 3) %>%
  select(- avg_Distance_scale) -> df_removed

# merge locomotion data
df_locomotion <- read_csv("Data_locomotion.csv", col_types = "cdcdi") 
df_removed %>% 
  left_join(df_locomotion, by = c("Name", "AgeinMonths")) -> df_processed
df_processed %>% 
  write_csv("Data_processed.csv")

filepaths <- str_c(str_sub(here(), start = 1, end = -12), c("Analysis1", "Analysis2", "Analysis3"), "Data_processed.csv", sep = "/")
write_csv(df_processed, filepaths[1]) 
write_csv(df_processed, filepaths[2]) 
write_csv(df_processed, filepaths[3]) 

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}