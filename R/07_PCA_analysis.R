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

# Wrangle data ------------------------------------------------------------
data_clean_num <- data_clean_num %>% 
  mutate(status = as_factor(status)) %>% 
  mutate(historyCardio = as_factor(historyCardio)) %>%
  mutate(electroCardioG = as_factor(electroCardioG)) %>%
  mutate(performance = as_factor(performance)) %>%
  mutate(stage = as_factor(stage)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

# we run the PCA and plot the results

dropCol <- c("performance", "historyCardio", "boneMetastase", "stage", "electroCardioG")

pca_data <- data_clean_num %>% 
  select(-one_of(dropCol)) %>%
  select(!status) %>%
  prcomp(scale = TRUE)

plot <- pca_data %>% augment(data_clean_num) %>%
  mutate(status = factor(status)) %>%
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             color = status)) + 
  geom_point(size = 2) + 
  xlab("PC1") +
  ylab("PC2") + 
  theme_classic(base_family = "Avenir", base_size = 8) +
  theme(legend.position = "bottom")
plot
# Write data --------------------------------------------------------------
ggsave(plot, file = "results/07_PCA.png")