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
#renaming columns
raw_data <- raw_data %>% 
  rename(patientID  = patno, 
         treatment = rx, 
         monthFollowUp = dtime, 
         weightIndex = wt, 
         historyCardio = hx, 
         performance = pf, 
         systolicBP = sbp, 
         diastolicBP = dbp , 
         electroCardioG = ekg, 
         hemoglobin = hg,  
         tumorSize = sz, SGindex = sg, 
         acidPhosphatase =  ap, 
         boneMetastase = bm )

#checking the data
glimpse(raw_data)

raw_data %>%
  summarise(n = n_distinct(patientID))  # 502 rows, 502 unique patients, so no duplicates in patients

NA_values <- sum(is.na(raw_data)) # 27 NA values

# for age, weightIndex, tumorSize and SGIndex we filled the NAs with the mean
meanAge <- raw_data %>% drop_na() %>% summarise(m = mean(age))
meanWI <- raw_data %>% drop_na() %>% summarise(m = mean(weightIndex))
meanTS <- raw_data %>% drop_na() %>% summarise(m = mean(tumorSize))
meanSGI <- raw_data %>% drop_na() %>% summarise(m = mean(SGindex))
raw_data <- raw_data %>% replace_na(list(age = meanAge, 
                                         weightIndex = meanWI,
                                         tumorSize = meanTS,
                                         SGindex = meanSGI))

NA_values <- sum(is.na(raw_data)) # 8 values

#for electroCardioG we decided to drop the NaN
data_clean <- raw_data %>% drop_na() 


# Write data --------------------------------------------------------------
write_tsv(x = data_clean,
          file = "data/02_data_clean.tsv.gz")