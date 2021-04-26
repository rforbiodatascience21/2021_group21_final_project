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
  ggplot( aes(y=stage, x=tumorSize,  fill=stage)) +
  geom_density_ridges(alpha=0.7) +
  theme_ridges() +
  xlab("Tumor Size (cm2)") +
  ylab("Cancer Stages") + 
  scale_fill_manual(values=c ("#E69F00", "#611DEB"))


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)