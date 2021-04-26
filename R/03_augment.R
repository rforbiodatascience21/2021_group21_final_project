# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")


# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean <- read_tsv(file = "data/02_data_clean.tsv.gz")

# Wrangle data ------------------------------------------------------------

data_clean_aug <- data_clean %>% separate(status,
                                          c("status","reasonDeath"),
                                          " - ") %>% 
  mutate(dose = case_when(str_detect(treatment, pattern = "estrogen") ~ str_sub(treatment, start = 1, end = 3),
                          str_detect(treatment, pattern = "placebo") ~ "0")) %>%
  mutate(treatment = case_when (str_detect(treatment, pattern = "placebo") ~ "placebo",
                                str_detect(treatment, pattern = "estrogen") ~ "estrogen")) %>%
  mutate(dose = as.numeric(dose)) %>%
  mutate(reasonDeathNum = case_when(reasonDeath == "prostatic ca" ~ 2,
                          reasonDeath == "cerebrovascular" ~ 1,
                          reasonDeath == "heart or vascular" ~ 1,
                          reasonDeath == "other ca" ~ 0,
                          reasonDeath == "other specific non-ca" ~ 0,
                          reasonDeath == "pulmonary embolus" ~ 1,
                          reasonDeath == "respiratory disease" ~ 0,
                          reasonDeath == "unspecified non-ca" ~ 0))
  
glimpse(data_clean)

data_clean_pca <- data_clean %>%
  mutate (status = case_when(status == "alive"  ~ 0,
                               status == "dead"  ~ 1)) %>%
  mutate(status = case_when(status == "alive" ~ 0,
                            status == "dead - prostatic ca" ~ 1,
                            status == "dead - heart or vascular" ~ 2,
                            status == "dead - cerebrovascular" ~ 3,
                            status == "dead - pulmonary embolus" ~ 4,
                            status == "dead - other ca" ~ 5,
                            status == "dead - respiratory disease" ~ 6,
                            status == "dead - other specific non-ca" ~ 7,
                            status == "dead - unspecified non-ca" ~ 8,
                            status == "dead - unknown cause" ~ 9)) %>%
  mutate(treatment = case_when(str_detect(treatment, pattern = "estrogen") ~ as.numeric(str_sub(treatment, start = 1, end = 3)),
                               str_detect(treatment, pattern = "placebo") ~ 0)) %>%
  mutate(electroCardioG = case_when(electroCardioG == "normal" ~ 0,
                                    electroCardioG == "benign" ~ 1,
                                    electroCardioG == "rhytmic disturb & electrolyte ch" ~ 2,
                                    electroCardioG == "heart block or conduction def" ~ 3,
                                    electroCardioG == "heart strain" ~ 4,
                                    electroCardioG == "old MI" ~ 5,
                                    electroCardioG == "recent MI" ~ 6))

# Write data --------------------------------------------------------------
write_tsv(x = data_clean_aug,
          file = "data/03_data_clean_aug.tsv.gz")

write_tsv(x = data_clean_pca,
          file = "data/03_data_clean_pca.tsv.gz")