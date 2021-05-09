# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggridges")
library("ggplot2")
library("broom")
library("factoextra")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean_num <- read_tsv(file = "data/03_data_clean_num.tsv.gz")

# Wrangle data ------------------------------------------------------------

# Run a PCA with just the continuous attributes

dropCol <- c("performance", "historyCardio", "boneMetastase", "stage", "electroCardioG")

pca1_data <- data_clean_num %>% 
  select(-one_of(dropCol)) %>%
  select(!status) %>%
  prcomp(scale = TRUE)

fviz_eig(pca1_data)

PCA1 <- pca1_data %>% augment(data_clean_num) %>%
  mutate(status = factor(status)) %>%
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             color = status)) + 
  geom_point(size = 2) + 
  xlab("PC1") +
  ylab("PC2") + 
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

PCA1_contribution <- fviz_pca_var(pca1_data,
             col.var = "contrib", # Color by contributions to the PC
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE     # Avoid text overlapping
)

# Run a PCA with all variables

pca2_data <- data_clean_num %>% 
  select(!status) %>%
  prcomp(scale = TRUE)

PCA2 <- pca2_data %>% augment(data_clean_num) %>%
  mutate(status = factor(status)) %>%
  ggplot(aes(x = .fittedPC1,
             y = .fittedPC2,
             color = status)) + 
  geom_point(size = 2) + 
  xlab("PC1") +
  ylab("PC2") + 
  theme_classic(base_size = 14) +
  theme(legend.position = "bottom")

# Write data --------------------------------------------------------------
ggsave(PCA1, file = "results/07_PCA_continuous.png")
ggsave(PCA1_contribution, file = "results/07_PCA_contribution.png")
ggsave(PCA2, file = "results/07_PCA_all_variables.png")

