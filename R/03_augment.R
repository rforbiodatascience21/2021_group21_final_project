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
  mutate(reasonDeath = replace_na(reasonDeath, "not dead")) %>%
  mutate(dose = case_when(str_detect(treatment, pattern = "estrogen") ~ str_sub(treatment, start = 1, end = 3),
                          str_detect(treatment, pattern = "placebo") ~ "0")) %>%
  mutate(dose = as.numeric(dose)) %>%
  relocate(dose, .after = stage) %>%
  mutate(treatment = case_when (str_detect(treatment, pattern = "placebo") ~ "placebo",
                                str_detect(treatment, pattern = "estrogen") ~ "estrogen")) %>%
  mutate(reasonDeathNum = case_when(reasonDeath == "prostatic ca" ~ 2,
                          reasonDeath == "cerebrovascular" ~ 1,
                          reasonDeath == "heart or vascular" ~ 1,
                          reasonDeath == "other ca" ~ 0,
                          reasonDeath == "other specific non-ca" ~ 0,
                          reasonDeath == "pulmonary embolus" ~ 1,
                          reasonDeath == "respiratory disease" ~ 0,
                          reasonDeath == "unspecified non-ca" ~ 0,
                          reasonDeath == "not dead" ~ 0)) %>%
  mutate(reasonDeath = case_when( reasonDeath == "other specific non-ca" ~ "other non-ca",
                                reasonDeath == "unspecified non-ca" ~ "other non-ca",
                                TRUE ~ reasonDeath))

glimpse(data_clean_aug)

data_clean_pca <- select(data_clean_aug, -c(treatment, reasonDeath)) %>%
  mutate (status = case_when(status == "alive"  ~ 0,
                             status == "dead"  ~ 1)) %>%
  relocate(reasonDeathNum, .after = status) %>%
  mutate(electroCardioG = case_when(electroCardioG == "normal" ~ 0,
                                    electroCardioG == "benign" ~ 1,
                                    electroCardioG == "rhytmic disturb & electrolyte ch" ~ 2,
                                    electroCardioG == "heart block or conduction def" ~ 3,
                                    electroCardioG == "heart strain" ~ 4,
                                    electroCardioG == "old MI" ~ 5,
                                    electroCardioG == "recent MI" ~ 6)) %>%
  mutate(performance = case_when(performance == "normal activity" ~ 0,
                                 performance == "in bed < 50% daytime" ~ 1,
                                 performance == "in bed > 50% daytime" ~ 2,
                                 performance == "confined to bed" ~ 3))



glimpse(data_clean_pca)

# Write data --------------------------------------------------------------
write_tsv(x = data_clean_aug,
          file = "data/03_data_clean_aug.tsv.gz")

write_tsv(x = data_clean_pca,
          file = "data/03_data_clean_pca.tsv.gz")