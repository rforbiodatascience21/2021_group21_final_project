# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggridges")
library("ggplot2")
library("ggridges")
library("broom")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean_num <- read_tsv(file = "data/03_data_clean_num.tsv.gz")

model_data <- data_clean_num %>% 
  pivot_longer(!status, 
               names_to = "measurements", 
               values_to = "values") %>% 
  group_by(measurements) %>% 
  nest %>% 
  ungroup %>%
  mutate(model = map(data, ~ glm(status ~ values,
                                 data = .,
                                 family = binomial(link = "logit")))) %>% 
  mutate(model_tidy = map(model, ~tidy(., conf.int = TRUE))) %>%
  unnest(model_tidy) %>%
  filter(term == "values") %>% 
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant")) %>% 
  mutate(neg_log10_p = -log10(p.value))

# Visualize data ----------------------------------------------------------

# Manhattan plot

Manhattan_plot <- model_data %>% 
  ggplot(aes(x = reorder(measurements, -neg_log10_p),
             y = neg_log10_p,
             colour = identified_as)) + 
  geom_point() +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed") +
  scale_color_manual(values = c("#E7B800", "#00AFBB")) +
  theme_classic(base_family = "Avenir",
                base_size = 14) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(legend.position = "bottom", 
        plot.title = element_text(hjust = 0.5), 
        legend.title = element_blank()) +
  labs(title = "Manhattan plot on numeric attributes", x = "Parameters",
       y = "Minus log10(p)")

# Estimate confidence plot from the linear model

estimate_plot <- model_data %>% 
  ggplot(aes(x = estimate,
             y = reorder(measurements, -estimate),
             colour = identified_as)) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.2)) +
  theme_classic(base_family = "Avenir",
                base_size = 14) +
  theme(plot.title = element_text(hjust = 0.5),
        axis.text.y = element_text("Parameters"),
        legend.position = "bottom",
        legend.title = element_blank()) + 
  scale_color_manual(values = c("#E7B800", "#00AFBB")) +
  labs(title = "Estimate confidence plot", y = "")

# Write data --------------------------------------------------------------
save(model_data, file = "results/GLMmodel_data.RData")
ggsave(Manhattan_plot, file = "results/05_plot_Manhattan.png")
ggsave(estimate_plot, file = "results/05_plot_estimate.png")
