# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_raw <- read_csv(file = "data/_raw/prostateCancerData.csv")

# Write data --------------------------------------------------------------
write_tsv(x = my_data_raw,
          file = "data/01_raw_data.tsv.gz")