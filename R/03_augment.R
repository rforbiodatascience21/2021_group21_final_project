# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------
data_clean_aug <- data_clean %>% 
  mutate(status = case_when(status == "alive" ~ 0,
                            status == "dead - prostatic ca" ~ 1,
                            status == "dead - heart or vascular" ~ 2,
                            status == "dead - cerebrovascular" ~ 3,
                            status == "dead - pulmonary embolus" ~ 4,
                            status == "dead - other ca" ~ 5,
                            status == "dead - respiratory disease" ~ 6,
                            status == "dead - other specific non-ca" ~ 7,
                            status == "dead - unspecified non-ca" ~ 8,
                            status == "dead - unknown cause" ~ 9))



# Write data --------------------------------------------------------------
write_tsv(x = data_clean_aug,
          file = "data/03_data_clean_aug.tsv.gz")