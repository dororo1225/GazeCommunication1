#### CREATE GRAPHS
library(tidyverse)
library(rstan)
library(loo)
library(ggmcmc)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# load data
read_csv("Data_processed.csv", col_types = "cdiiiidcdi") %>% 
  mutate(ID_pair = as.numeric(as.factor(Name))) -> df

read_csv("Data_locomotion.csv", col_types = "cdcdi") %>% 
  mutate(ID_pair = as.numeric(as.factor(Name))) -> df_locomotion

# prepare data list for stan model
expand.grid(avg_Distance = seq(min(df$avg_Distance), max(df$avg_Distance), length = 40),
            Name = unique(df$Name),
            AgeinMonths = unique(df$AgeinMonths),
            stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  left_join(df_locomotion, by = c("Name", "AgeinMonths")) %>% 
  mutate(id = row_number()) -> df_predict

data <- list(N = nrow(df),
             N_pair = length(unique(df$Name)),
             N_bout = df$N_Bout,
             Y = df$N_Infant,
             X1 = df$AgeinMonths,
             X3 = df$avg_Distance,
             ID_pair = df$ID_pair,
             N_predict = nrow(df_predict),
             X1_predict = df_predict$AgeinMonths,
             X3_predict = df_predict$avg_Distance,
             ID_pair_predict = df_predict$ID_pair)

# model fitting
## model_selected.stan
form <- c("Age + Dist")
model <- stan_model(file = 'StanModels/model_selected.stan')
fit <- sampling(model,
                data = data,
                chains = 4,
                iter = 6000,
                warmup = 1000,
                thin = 2,
                refresh = 0,
                sample_file = "StanResults/model_selected.csv",
                seed = 12345)
summary(fit, pars = c("beta0", "beta1", "beta3", "sigma_session", "sigma_pair"))$summary %>%
  signif(digits = 3) %>% kable()
fit <- read_stan_csv(csvfiles = sprintf("StanResults/model_selected_%s.csv", 1:4))

# figure S5
ggs(fit) %>%
  filter(str_detect(.$Parameter, pattern = "q")) %>%
  group_by(Parameter) %>%
  summarise(EAP = mean(value),
            q_975 = quantile(value, probs = 0.975),
            q_025 = quantile(value, probs = 0.025)) %>%
  mutate(id = as.numeric(str_extract(.$Parameter, pattern = "[[:digit:]]+"))) %>%
  left_join(df_predict, by = "id") -> df_fit

df_fit %>%
  ggplot(aes(x = avg_Distance)) +
  geom_ribbon(aes(ymax = q_975, ymin = q_025), fill = "grey75") +
  geom_line(aes(y = EAP, color = AgeinMonths), lwd = 1.2) +
  geom_point(data = df, aes(y = N_Infant / N_Bout, color = AgeinMonths, size = N_Bout), alpha = 0.8) +
  facet_grid(Name~AgeinMonths) +
  labs(x = "distance (m)", y = "proportion", color = "age in months", size = "Num EC Bout") +
  guides(size = FALSE) +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5), limits = c(0, 2.2)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18))
ggsave(width = 16.4, height = 9.5, file = "figS5.pdf", dpi = 300)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}