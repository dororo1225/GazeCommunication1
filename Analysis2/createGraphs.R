#### CREATE GRAPHS
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

read_csv("Data_locomotion.csv", col_types = "cdcdi") %>% 
  mutate(ID_pair = as.numeric(as.factor(Name))) -> df_locomotion

# prepare data list for stan model
expand.grid(avg_Distance = seq(min(df$avg_Distance), max(df$avg_Distance), length = 40),
            Name = unique(df$Name),
            AgeinMonths = unique(df$AgeinMonths),
            Initiator = unique(df$Initiator),
            stringsAsFactors = FALSE) %>%
  as_tibble() %>%
  left_join(df_locomotion, by = c("Name", "AgeinMonths")) %>% 
  arrange(Name, AgeinMonths, avg_Distance) %>% 
  mutate(id = row_number()) -> df_predict

data <- list(N = nrow(df),
             N_pair = length(unique(df$Name)),
             N_session = length(unique(df$ID_session)),
             Y = df$count,
             X1 = df$AgeinMonths,
             X3 = df$avg_Distance,
             X4 = (df$avg_Distance)^2,
             X5 = if_else(df$Initiator == "infant", 1, 0),
             X6 = if_else(df$Initiator == "infant", 1, 0) * df$AgeinMonths,
             X8 = if_else(df$Initiator == "infant", 1, 0) * df$avg_Distance,
             ID_pair = df$ID_pair,
             ID_session = df$ID_session,
             N_predict = nrow(df_predict),
             X1_predict = df_predict$AgeinMonths,
             X3_predict = df_predict$avg_Distance,
             X4_predict = (df_predict$avg_Distance)^2,
             X5_predict = if_else(df_predict$Initiator == "infant", 1, 0),
             X6_predict = if_else(df_predict$Initiator == "infant", 1, 0) * df_predict$AgeinMonths,
             X8_predict = if_else(df_predict$Initiator == "infant", 1, 0) * df_predict$avg_Distance,
             ID_pair_predict = df_predict$ID_pair)

# model fitting
## model_selected.stan
form <- c("Dist^2 + (1 + First)(1 + Age + Dist)")
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
fit <- read_stan_csv(csvfiles = sprintf("StanResults/model_selected_%s.csv", 1:4))
summary(fit, pars = c("beta0", "beta1", "beta3", "beta4", "beta5", "beta6", "beta8", "sigma_session", "sigma_pair",
                      "D_infant", "D_mother", "deltaD", "age_infant", "distance_infant"))$summary %>%
  signif(digits = 3)


# prepare parameters to draw pridicted values
df %>%
  mutate(Initiator = if_else(Initiator == "mother", "parent", Initiator),
         Initiator = str_c(str_to_lower(Initiator), "-led EC")) -> df_fig
col_i <- "#3B528BFF"
col_pvi <- "#472D7BFF"
col_m <- "#AADC32FF"
col_pvm <- "#29AF7FFF"
lap_ps <- 0.5
lwd_ps <- 0.5

# figure 2(b)
set.seed(1234)
ggs(fit) %>%
  filter(str_detect(Parameter, pattern = "beta") | Parameter == "r_pair_int.2"| Parameter == "r_pair_slp.2") %>%
  spread(Parameter, value) %>%
  sample_n(100) -> df_sample

ggs(fit) %>%
  filter(str_detect(Parameter, pattern = "beta") | Parameter == "r_pair_int.2"| Parameter == "r_pair_slp.2") %>%
  spread(Parameter, value) %>%
  summarise_all(mean) -> df_EAP

drawPV_m <- function(int, ri1, age, dist, distS, x){
  exp(int + ri1 + age * 11.5 + dist * x + distS * x^2 )
}  

drawPV_i <- function(int, ri1, age, dist, distS, ri2, iec, iecA, iecD, x){
  exp(int + ri1 + iec + ri2 + (age + iecA) * 11.5 + (dist + iecD) * x + distS * x^2 )
}  

data_frame(avg_Distance = c(1.2, 1.2, 1.2, 1.2),
           count = c(0, 0, 0, 0),
           Initiator = rep(c("infant-led EC", "parent-led EC"), each = 2)) -> d_l

data_frame(avg_Distance = c(0.1, 2.1)) %>% 
  ggplot(aes(x = avg_Distance)) +
  geom_line(data = d_l, aes(x = avg_Distance, y = count, group = Initiator, color = Initiator), lwd = 1) +
  mapply(
    function(a, b, c, d, e) stat_function(fun = drawPV_m, args = list(int = a, ri1 = b, age = c, dist = d, distS = e),
                                          alpha = lap_ps, lwd = lwd_ps, color = col_m),
    df_sample$beta0, df_sample$r_pair_int.2, df_sample$beta1, df_sample$beta3, df_sample$beta4 
  ) +
  stat_function(fun = drawPV_m, args = list(int = df_EAP$beta0,
                                            ri1 = df_EAP$r_pair_int.2,
                                            age = df_EAP$beta1,
                                            dist = df_EAP$beta3,
                                            distS = df_EAP$beta4),
                lwd = 1.2, color = col_pvm) +
  mapply(
    function(a, b, c, d, e, f, g, h, i) stat_function(fun = drawPV_i,
                                                      args = list(int = a,
                                                                  ri1 = b,
                                                                  age = c,
                                                                  dist = d,
                                                                  distS = e,
                                                                  ri2 = f,
                                                                  iec = g,
                                                                  iecA = h,
                                                                  iecD = i),
                                                      alpha = lap_ps, lwd = lwd_ps, color = col_i),
    df_sample$beta0, df_sample$r_pair_int.2, df_sample$beta1, df_sample$beta3, df_sample$beta4,
    df_sample$r_pair_slp.2, df_sample$beta5, df_sample$beta6, df_sample$beta8
  ) +
  stat_function(fun = drawPV_i, args = list(int = df_EAP$beta0,
                                            ri1 = df_EAP$r_pair_int.2,
                                            age = df_EAP$beta1,
                                            dist = df_EAP$beta3,
                                            distS = df_EAP$beta4,
                                            ri2 = df_EAP$r_pair_slp.2,
                                            iec = df_EAP$beta5,
                                            iecA = df_EAP$beta6,
                                            iecD = df_EAP$beta8),
                lwd = 1.2, color = col_pvi) +
  geom_point(data = df_fig %>% 
               filter(Name == "B", AgeinMonths == 11.5), 
             aes(y = count, fill = Initiator, group = Initiator), alpha = 0.8, size = 2, shape = 21) +
  labs(x = "distance (m)", y = "count") +
  guides(fill = guide_legend(title = NULL), color = guide_legend(title = NULL)) +
  scale_color_manual(values = c(col_pvi, col_pvm)) +
  scale_fill_manual(values = c(col_i, col_m)) +
  scale_y_continuous(breaks = seq(0, 8, by = 2)) +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5), limits = c(0, 2.2)) +
  theme_classic() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15, color = "black"),
        axis.line = element_line(size = 1),
        axis.ticks = element_line(size = 1, color = "black"),
        legend.text = element_text(size = 15),
        legend.position = c(0.95, 0.95), legend.justification = c(1, 1),
        legend.background = element_rect(color = "black", size = 0.5))
ggsave(width = 5, height = 4.5, file = "fig2b.pdf", dpi = 300)

# figure S4
ggs(fit) %>%
  filter(str_detect(.$Parameter, pattern = "lambda")) %>%
  group_by(Parameter) %>%
  summarise(EAP = mean(value),
            q_975 = quantile(value, probs = 0.975),
            q_025 = quantile(value, probs = 0.025)) %>%
  mutate(id = as.numeric(str_extract(.$Parameter, pattern = "[[:digit:]]+"))) %>%
  left_join(df_predict, by = "id") %>%
  mutate(Initiator = str_c(str_to_lower(Initiator), "-led EC"),
         Initiator = str_replace(Initiator, pattern = "mother", replacement = "parent")) -> df_fit

df_fit %>%
  ggplot(aes(x = avg_Distance)) +
  geom_ribbon(aes(ymax = q_975, ymin = q_025, fill = Initiator), alpha = 0.4) +
  geom_line(aes(y = EAP, color = Initiator, group = Initiator), lwd = 1) +
  geom_point(data = df_fig, aes(y = count, color = Initiator), alpha = 0.5) +
  facet_grid(Name~AgeinMonths) +
  labs(x = "distance (m)", y = "count") +
  scale_x_continuous(breaks = seq(0, 2, by = 0.5), limits = c(0, 2.2)) +
  scale_color_manual(values = c(col_pvi, col_pvm)) +
  scale_fill_manual(values = c(col_i, col_m)) +
  coord_cartesian(ylim = c(0, 15)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(color = "black"),
        strip.text = element_text(face = "bold", size = 18),
        legend.title = element_text(face = "bold", size = 18))
ggsave(width = 16.4, height = 9.5, file = "figS4.pdf", dpi = 300)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}