# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_raw <- read_csv(file = "data/_raw/prostateCancerData.csv")

# Data is split into General information about the patient and tests and cancer-related data
# (this is done for academical purposes regarding join)

patient_raw_data <- my_data_raw %>%
  select(patno, status, age, wt, pf, hx, bm)

cancer_raw_data <- my_data_raw %>%
  select(-status, -age, -wt, -pf, -hx, -bm)

# Write data --------------------------------------------------------------
write_tsv(x = patient_raw_data,
          file = "data/01_patient_raw_data.tsv.gz")
write_tsv(x = cancer_raw_data,
          file = "data/01_cancer_raw_data.tsv.gz")