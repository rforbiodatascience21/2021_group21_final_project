# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
raw_data <- read_tsv(file = "data/01_raw_data.tsv.gz")


# Wrangle data ------------------------------------------------------------

raw_data <- raw_data %>% 
  rename(patientID  = patno, treatment = rx, monthFollowUp = dtime, weightIndex = wt, historyCardio = hx, performance = pf, systolicBP = sbp, diastolicBP = dbp , electroCardioG = ekg  , hemoglobin = hg ,  tumorSize = sz , SGindex = sg , acidPhosphatase =  ap , boneMetastase = bm )

glimpse(raw_data)

raw_data %>%
  summarise(n = n_distinct(patientID))  # 502 rows, 502 unique patients, so no duplicates in patients

NA_values <- sum(is.na(raw_data)) # 27 NA values

data_clean <- raw_data %>% drop_na()

# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv.gz")