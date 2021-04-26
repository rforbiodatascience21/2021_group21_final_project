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
<<<<<<< HEAD

data %>% 
  rename(patno = patientID, rx = treatment, dtime = monthFollowUp, wt = weightIndex, hx = historyCardio, sbp = systolicBP, dbp = diastolicBP, ekg = electroCardioG, hg= hemoglobin, sz= tumorSize, sg = SGindex, ap = acidPhosphatase, bm = boneMetastase)


glimpse(data)

=======

glimpse(raw_data)
>>>>>>> 6653292e395d24fa0fad032f0ecb20ac3634d7a3

NA_values ->sum(is.na(raw_data)) # 27 NA values

raw_data %>%
  summarise(n = n_distinct(patno))  # 502 rows, 502 unique patients, so no duplicates in patients

data_clean <- data %>%

# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv")