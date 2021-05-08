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
data_clean_pca <- read_tsv(file = "data/03_data_clean_pca.tsv.gz")

# Wrangle data ------------------------------------------------------------
data_clean_pca <- data_clean_pca %>% 
  mutate(status = as_factor(status)) %>% 
  mutate(historyCardio = as_factor(historyCardio)) %>%
  mutate(electroCardioG = as_factor(electroCardioG)) %>%
  mutate(performance = as_factor(performance)) %>%
  mutate(stage = as_factor(stage)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

# we run the PCA and plot the results
pca_columns <- data_clean_pca %>%
  select(status, everything())

pca_data <- pca_columns %>% 
  select(!status) %>%
  prcomp(scale = TRUE)

plot <- pca_data %>% augment(pca_columns) %>%
  mutate(status = factor(status)) %>%
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             color = status)) + 
  geom_point(size = 2) + 
  theme_classic(base_family = "Avenir", base_size = 8) +
  theme(legend.position = "bottom")
plot
# Write data --------------------------------------------------------------
ggsave(plot, file = "results/07_PCA.png")