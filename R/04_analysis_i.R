# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggridges")
library("ggplot2")
library("ggridges")
library("broom")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean_aug <- read_tsv(file = "data/03_data_clean_aug.tsv.gz")
data_clean_pca <- read_tsv(file = "data/03_data_clean_pca.tsv.gz")

# Wrangle data ------------------------------------------------------------
data_clean_aug <- data_clean_aug %>% 
  mutate(treatment =as_factor(treatment)) %>%
  mutate(status =as_factor(status)) %>% 
  mutate(electroCardioG =as_factor(electroCardioG)) %>%
  mutate(performance =as_factor(performance)) %>%
  mutate(stage =as_factor(stage)) %>% 
  mutate(reasonDeathNum = as_factor(reasonDeathNum)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

# Model data

# we could add some simple command such as

#gravier_data %>%
#group_by(outcome) %>%
#  summarise(n = n())

# to get quick info about distribution in our dataset in case it is relevant

# one measurement fitting model
fit <- data_clean_pca %>% 
  glm(status ~ hemoglobin,
      data = .,
      family = binomial(link = "logit"))

fit %>%
  #summary
  tidy
# all measures at once
pca_data <- data_clean_pca %>%
  pivot_longer(!status, names_to = "measurements", values_to = "values")

pca_data <- pca_data %>%
  group_by(measurements) %>%
  tidyr::nest() %>%
  ungroup()

pca_data <- pca_data %>%
  mutate(model = map(data, ~ glm(status ~ values, 
                               data = .,
                               family = binomial(link = "logit"))))
# Visualize data ----------------------------------------------------------
data_clean_aug %>% ...

# Plot 0 - very basic, see the count of stage 3 and 4, we may exclude this
ggplot(data_clean_aug, aes(stage, ..count..)) + 
  geom_bar(alpha=0.7,aes(fill = stage), position = "dodge")+
  scale_fill_manual(values=c ("#edae49", "#66a182"))


# Plot 1 - tumor Size vs cancer Stage
data_clean_aug %>%
  ggplot( aes(y=stage, x=tumorSize,  fill=stage)) +
  geom_density_ridges(alpha=0.7) +
  theme_ridges() +
  xlab("Tumor Size (cm2)") +
  ylab("Cancer Stage") + 
  scale_fill_manual(values=c("#00AFBB", "#E7B800"))


# Plot 2 - Reason of Death Count, grouped by stage
data_clean_aug %>% 
  filter(!is.na(reasonDeath) & 
           reasonDeath != "unknown cause" & 
           reasonDeath != "other ca" & 
           reasonDeath != "other specific non-ca" &
           reasonDeath != "unspecified non-ca") %>% 
  count(reasonDeath, stage) %>% 
  ggplot(aes(x = reorder(reasonDeath, n, sum), y = n, fill = stage)) + 
  geom_col() +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust=0.5, size=11))+ 
  xlab("Reason of Death") +
  ylab("Count") + 
  scale_fill_manual(values=c( "#00AFBB", "#E7B800"))


# Write data --------------------------------------------------------------
write_tsv(...)
ggsave(...)