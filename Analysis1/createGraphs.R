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
             N_predict = nrow(df_predict),
             Y = df$N_Bout,
             X1 = df$AgeinMonths,
             X3 = df$avg_Distance,
             X4 = (df$avg_Distance)^2,
             ID_pair = df$ID_pair,
             X1_predict = df_predict$AgeinMonths,
             X3_predict = df_predict$avg_Distance,
             X4_predict = (df_predict$avg_Distance)^2,
             ID_pair_predict = df_predict$ID_pair)

# model fitting
## model_selected.stan
form <- c("Age + Dist + Dist^2")
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
summary(fit, pars = c("beta0", "beta1", "beta3", "beta4", "phi", "sigma_pair", "D"))$summary %>%
  signif(digits = 3)
fit <- read_stan_csv(csvfiles = sprintf("StanResults/model_selected_%s.csv", 1:4))

# figure 2(a)
set.seed(1234)
ggs(fit) %>%
  filter(str_detect(Parameter, pattern = "beta") | Parameter == "r_pair.2") %>%
  spread(Parameter, value) %>%
  sample_n(100) -> df_sample

ggs(fit) %>%
  filter(str_detect(Parameter, pattern = "beta") | Parameter == "r_pair.2") %>%
  spread(Parameter, value) %>%
  summarise_all(mean) -> df_EAP

drawPV <- function(int, ri, age, dist, distS, x){exp(int + ri + age * 11.5 + dist * x + distS * x^2)}  

col <- "#21908CFF"
lap_ps <- 0.5
lwd_ps <- 0.5

data_frame(avg_Distance = c(0.1, 2.1)) %>% 
  ggplot(aes(x = avg_Distance)) +
  mapply(
    function(a, b, c, d, e) stat_function(fun = drawPV, args = list(int = a, ri = b, age = c, dist = d, distS = e),
                                          alpha = lap_ps, lwd = lwd_ps, color = "#27AD81FF"),
    df_sample$beta0, df_sample$r_pair.2, df_sample$beta1, df_sample$beta3, df_sample$beta4 
  ) +
  stat_function(fun = drawPV, args = list(int = df_EAP$beta0, ri = df_EAP$r_pair.2, age = df_EAP$beta1, dist = df_EAP$beta3, distS = df_EAP$beta4),
                lwd = 1.2, color = col) +
  geom_point(data = filter(df, Name == "B", AgeinMonths == 11.5), aes(y = N_Bout), alpha = 0.8, size = 2, fill = col, shape = 21) +
  labs(x = "distance (m)", y = "count") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5), limits = c(0, 2.2)) +
  theme_classic() +
  theme(plot.tag = element_text(size = 15, face = "italic"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1))
ggsave(width = 5, height = 4.5, file = "fig2a.pdf", dpi = 300)

# figure S3
ggs(fit) %>%
  filter(str_detect(.$Parameter, pattern = "lambda")) %>%
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
  geom_point(data = df, aes(y = N_Bout, color = AgeinMonths), alpha = 0.8, size = 1.5) +
  facet_grid(Name~AgeinMonths) +
  labs(x = "distance (m)", y = "count", color = "age in months") +
  scale_color_viridis_c() +
  scale_fill_viridis_c() +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5), limits = c(0, 2.2)) +
  coord_cartesian(ylim = c(0, 22)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18))
ggsave(width = 16.4, height = 9.5, file = "figS3.pdf", dpi = 300)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}