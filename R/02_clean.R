# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
raw_data <- read_tsv(file = "data/01_my_data.tsv.gz")


# Wrangle data ------------------------------------------------------------

raw_data %>% 
  rename(patno = patientID, rx = treatment, dtime = monthFollowUp, wt = weightIndex, hx = historyCardio, sbp = systolicBP, dbp = diastolicBP, ekg = electroCardioG, hg= hemoglobin, sz= tumorSize, sg = SGindex, ap = acidPhosphatase, bm = boneMetastase)

glimpse(raw_data)

NA_values ->sum(is.na(raw_data)) # 27 NA values

raw_data %>% drop_na()

raw_data %>%
  summarise(n = n_distinct(patno))  # 502 rows, 502 unique patients, so no duplicates in patients

data_clean <- raw_data %>%

# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv")