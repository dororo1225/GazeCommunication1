#### RESULT OF MODEL COMPARISON
library(tidyverse)
library(rstan)
library(loo)
library(ggmcmc)
library(knitr)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# model formulas
form1 <- c("Age + Walk + Dist + Dist^2")
form2 <- c("Age + Walk + Dist")
form3 <- c("Age + Dist + Dist^2")
form4 <- c("Walk + Dist + Dist^2")
form5 <- c("Age + Walk")
form6 <- c("Age + Dist")
form7 <- c("Walk + Dist")
form8 <- c("Dist + Dist^2")

# waic of each model
df_waic <- data_frame(model = character(),
                      formula = character(),
                      waic = numeric(),
                      SE = numeric())

for (i in 1:8){
  assign(paste("fit", i, sep = ""), read_stan_csv(csvfiles = sprintf(paste("StanResults/model", i, "_%s.csv", sep = ""), 1:4)))
  assign(paste("loglik", i, sep = ""), extract(get(paste("fit", i, sep = "")))$log_lik)
  assign(paste("waic", i, sep = ""), waic(get(paste("loglik", i, sep = "")))$estimate[3])
  assign(paste("SE", i, sep = ""), waic(get(paste("loglik", i, sep = "")))$estimate[6])
  df_waic %>% 
    add_row(model = paste("model", i, sep = ""),
            formula = get(paste("form", i, sep = "")),
            waic = get(paste("waic", i, sep = "")),
            SE = get(paste("SE", i, sep = ""))) -> df_waic
}

df_waic %>% 
  arrange(waic) %>% 
  mutate(delta_waic = waic - first(waic)) %>% 
  kable()

# posterior distribution of each parameter in top 3 models (table S3)
data_frame(model = character(),
           posterior = character(),
           waic = numeric(),
           beta0 = numeric(),
           beta1 = numeric(),
           beta2 = numeric(),
           beta3 = numeric(),
           beta4 = numeric()) -> df_posterior

for (i in 1:8){
  ggs(get(paste("fit", i, sep = ""))) %>% 
    filter(str_detect(Parameter, pattern = "beta")) %>% 
    group_by(Parameter) %>% 
    summarise(EAP = mean(value),
              SD = sd(value)) -> df_fit
  
  df_fit %>% 
    select(-SD) %>% 
    spread(Parameter, EAP) %>% 
    mutate(posterior = "mean") -> df_fit_mean
  
  df_fit %>% 
    select(-EAP) %>% 
    spread(Parameter, SD) %>% 
    mutate(posterior = "SD") -> df_fit_SD
  
  bind_rows(df_fit_mean, df_fit_SD) %>%
    mutate(model = paste("model", i, sep = ""),
           waic = get(paste("waic", i, sep = ""))) %>% 
    select(model, posterior, waic, starts_with("beta")) %>% 
    bind_rows(df_posterior, .) -> df_posterior 
}

df_posterior %>% 
  arrange(waic) %>% 
  head(6) %>% 
  kable()

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}