#### CREATE FIGURES
library(tidyverse)
library(rstan)
library(ggmcmc)
library(loo)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

# figure S2
## load rawdata
df <- read_csv("Data_raw.csv", col_types = "cdcciiiiccdc")

## caliculate inter-eye-cotact-bout interval
df %>% 
  group_by(Video) %>% 
  mutate(EC_start_next = lead(EC_start)) %>% 
  ungroup() %>% 
  mutate(interval = (EC_start_next - EC_end) / 1000) %>% 
  filter(!is.na(interval)) -> df_interval

## define the threshold of inter-eye-contact-bout interval
fit1 <- read_stan_csv(csvfiles = sprintf("StanResults/model1_%s.csv", 1:4))
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

## define functions to draw figure
dmixgamma <- function(x, lambda, a1, a2, b1, b2){
  y <- lambda * dgamma(x, a1, b1) + (1 - lambda) * dgamma(x, a2, b2)
  return(y)
}

dgamma_part <- function(x, lambda, a, b){
  y <- lambda * dgamma(x, a, b)
  return(y)
}

## draw figure
df_interval %>% 
  ggplot(aes(x = interval)) +
  geom_histogram(aes(y = ..density..), binwidth = 10, fill = "white", color = "grey25", lwd = 0.5) +
  stat_function(fun = dmixgamma,
                args = list(lambda = df_fit1$pi[1],
                            a1 = df_fit1$shape[1],
                            a2 = df_fit1$shape[2],
                            b1 = df_fit1$rate[1],
                            b2 = df_fit1$rate[2]),
                lwd = 1, color = "#000004FF") +
  stat_function(fun = dgamma_part,
                args = list(lambda = df_fit1$pi[1],
                            a = df_fit1$shape[1],
                            b = df_fit1$rate[1]),
                lwd = 1, color = "#451077FF") +
  stat_function(fun = dgamma_part,
                args = list(lambda = df_fit1$pi[2],
                            a = df_fit1$shape[2],
                            b = df_fit1$rate[2]),
                lwd = 1, color = "#CD4071FF") +
  annotate("segment", x = thr, xend = thr, y = 0, yend = 0.014, size = 0.5, color = "grey25", lty = 2) +
  annotate("text", x = 275, y = 0.024, color = "#451077FF", fontface = "bold", size = 5,
           label = sprintf("%.2f * Gamma(shape=%.1f  rate=%.3f)", df_fit1$pi[1], df_fit1$shape[1], df_fit1$rate[1])) +
  annotate("text", x = 275, y = 0.022, color = "#CD4071FF", fontface = "bold", size = 5,
           label = sprintf("%.2f *Gamma(shape=%.1f  rate=%.3f)", df_fit1$pi[2], df_fit1$shape[2], df_fit1$rate[2])) +
  annotate("text", x = thr, y = 0.015, color = "black", size = 5,
           label = sprintf("%.1fs", thr)) +
  labs(x = "inter-eye-contact-bout interval (s)") +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0)) +
  coord_cartesian(xlim = c(0, 420), ylim = c(0, 0.025)) +
  theme_classic() +
  theme(axis.title = element_text(size = 18, face = "bold"),
        axis.text = element_text(color = "black", size = 12),
        axis.ticks = element_line(size = 0.8, color = "black"),
        axis.line = element_line(size = 0.8, color = "black"))
ggsave("figS2.pdf", width = 5, height = 4, dpi = 300)

# figure S1
## load locomotion data
df_locomotion <- read_csv("Data_locomotion.csv", col_types = "cdcdi") 

## draw figue
df_locomotion %>% 
  ggplot(aes(x = AgeinMonths, y = forcats::fct_rev(Name))) +
  geom_tile(aes(fill = Percent_walk), color = "grey50", lwd = 0.8) +
  geom_point(aes(color = LocomotorStatus), size = 2) +
  scale_fill_viridis_c(option = "A") +
  scale_color_manual(values = c("white", "black")) +
  scale_x_continuous(breaks = seq(10, 15.5, by = 0.5), expand = c(0, 0)) +
  scale_y_discrete(expand = c(0, 0)) +
  labs(x = "age in months", y = "participant", fill = "% Walk", color = "Locomotor\nStatus") +
  guides(fill = guide_legend(reverse = TRUE), color = FALSE) +
  theme_classic() +
  theme(axis.title = element_text(face = "bold", size = 18),
        axis.text = element_text(size = 18, color = "black"),
        axis.ticks = element_blank(),
        axis.line = element_line(size = 0.8, color = "grey50"),
        legend.title = element_text(face = "bold", size = 18),
        legend.text = element_text(size = 15))
ggsave(width = 10, height = 4, file = "figS1.pdf", dpi = 300)

# Session Information
devtools::session_info() %>% {
  print(.$platform)
  .$packages %>% dplyr::filter(`*` == "*") %>% 
    knitr::kable(format = "markdown")
}
