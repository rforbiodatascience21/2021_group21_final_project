# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")


# Wrangle data ------------------------------------------------------------

#creation  of our augmented data
data_clean_aug <- data_clean %>% 
  mutate(status = str_replace(status, 
                              pattern = "alive", 
                              replacement = "alive - not dead")) %>%
  separate(status,
           c("status","reasonDeath"),
           " - ") %>%
  mutate(dose = case_when(str_detect(treatment, 
                                     pattern = "estrogen") ~ str_sub(treatment, 
                                                                     start = 1, 
                                                                     end = 3),
                          str_detect(treatment, 
                                     pattern = "placebo") ~ "0")) %>%
  relocate(dose, .after = stage) %>%
  mutate(treatment = case_when (str_detect(treatment, pattern = "placebo") ~ "placebo",
                                str_detect(treatment, pattern = "estrogen") ~ "estrogen")) %>%
  mutate(reasonDeathNum = case_when(reasonDeath == "prostatic ca" ~ 2,
                                    reasonDeath == "cerebrovascular" ~ 1,
                                    reasonDeath == "heart or vascular" ~ 1,
                                    reasonDeath == "other ca" ~ 0,
                                    reasonDeath == "other specific non-ca" ~ 0,
                                    reasonDeath == "unknown cause" ~ 0,
                                    reasonDeath == "pulmonary embolus" ~ 1,
                                    reasonDeath == "respiratory disease" ~ 0,
                                    reasonDeath == "unspecified non-ca" ~ 0,
                                    reasonDeath == "not dead" ~ 0)) %>%
  mutate(reasonDeath = case_when(reasonDeath == "other specific non-ca" ~ "other non-ca",
                                 reasonDeath == "unspecified non-ca" ~ "other non-ca",
                                 TRUE ~ reasonDeath))

# Data set with all attributes double

dropCol <- c("patientID", "reasonDeath", "treatment", "reasonDeathNum")

data_clean_num <- data_clean_aug %>%
  select(-one_of(dropCol)) %>%
  mutate (status = case_when(status == "alive"  ~ 0,
                             status == "dead"  ~ 1)) %>%
  mutate(electroCardioG = case_when(electroCardioG == "normal" ~ 0,
                                    electroCardioG == "benign" ~ 1,
                                    electroCardioG == "rhythmic disturb & electrolyte ch" ~ 2,
                                    electroCardioG == "heart block or conduction def" ~ 3,
                                    electroCardioG == "heart strain" ~ 4,
                                    electroCardioG == "old MI" ~ 5,
                                    electroCardioG == "recent MI" ~ 6)) %>%
  mutate(performance = case_when(performance == "normal activity" ~ 0, # we will make this value go from 0 to 10, being the total time
                                 performance == "in bed < 50% daytime" ~ 2.5, # in bed, and thus for <50% and > 50%, the mid-points 2.5
                                 performance == "in bed > 50% daytime" ~ 7.5, # and 7.5 will be used, respectively
                                 performance == "confined to bed" ~ 10))

# Write data --------------------------------------------------------------
write_tsv(x = data_clean_aug,
          file = "data/03_data_clean_aug.tsv.gz")

write_tsv(x = data_clean_num,
          file = "data/03_data_clean_num.tsv.gz")