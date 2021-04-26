# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggridges")
library("ggplot2")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean_aug <- read_tsv(file = "data/03_data_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
data_clean_aug %>% ...


# Model data
data_clean_aug %>% ...


# Visualise data ----------------------------------------------------------
data_clean_aug %>% ...

data_clean_aug %>%
  ggplot( aes(y=stage, x=tumorSize,  fill=stage, group = stage)) +
  geom_density_ridges(alpha=0.6) +
  theme_ridges() +
  theme(
    legend.position="none",
    panel.spacing = unit(0.1, "lines"),
    strip.text.x = element_text(size = 8)) +
  xlab("tumor Size") +
  ylab("Cancer Stages")


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)