# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
my_data_raw <- read_csv(file = "data/_raw/prostateCancerData.csv")

# Data is split following the study differentiation
# (this is done for academical purposes regarding join)

patient_raw_data <- my_data_raw %>%
  select(patno, stage, rx, dtime, status, sdate)

pretreatment_raw_data <- my_data_raw %>%
  select(-stage, -rx, -dtime, -status, -sdate)

# Write data --------------------------------------------------------------
write_tsv(x = patient_raw_data,
          file = "data/01_patient_raw_data.tsv.gz")
write_tsv(x = pretreatment_raw_data,
          file = "data/01_pretreatment_raw_data.tsv.gz")
