# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
my_data_raw <- read_csv(file = "data/_raw/prostateCancerData.csv")


# Wrangle data ------------------------------------------------------------
my_data <- my_data_raw # %>% ...


# Write data --------------------------------------------------------------
write_tsv(x = my_data,
          file = "data/01_my_data.tsv")


