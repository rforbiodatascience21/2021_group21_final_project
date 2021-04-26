# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")

# Wrangle data ------------------------------------------------------------

data_clean <- data_clean %>% separate(status,
                                          c("status","reasonDeath"),
                                          " - ") %>% 
  mutate(dose = case_when(str_detect(treatment, pattern = "estrogen") ~ str_sub(treatment, start = 1, end = 3),
                          str_detect(treatment, pattern = "placebo") ~ "0")) %>%
  mutate(treatment = case_when (str_detect(treatment, pattern = "placebo") ~ "placebo",
                                str_detect(treatment, pattern = "estrogen") ~ "estrogen")) %>%
  mutate(dose = as.numeric(dose))

data_clean <- data_clean %>% 
  mutate(reasonDeathNum = case_when(reasonDeath == "prostatic ca" ~ 2,
                          reasonDeath == "cerebrovascular" ~ 1,
                          reasonDeath == "heart or vascular" ~ 1,
                          reasonDeath == "other ca" ~ 0,
                          reasonDeath == "other specific non-ca" ~ 0,
                          reasonDeath == "pulmonary embolus" ~ 1,
                          reasonDeath == "respiratory disease" ~ 0,
                          reasonDeath == "unspecified non-ca" ~ 0))
  
glimpse(data_clean)



  

data_clean$dose

# Write data --------------------------------------------------------------
write_tsv(x = data_clean_aug,
          file = "data/03_data_clean_aug.tsv.gz")