# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("readr")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_csv("data/_raw/prostateCancerData.csv")


# Wrangle data ------------------------------------------------------------

glimpse(data)

sum(is.na(data)) # 27 NA values

data %>%
  summarise(n = n_distinct(patno))  # 502 rows, 502 unique patients, so no duplicates in patients



data_clean <- data # %>% ...

# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv")