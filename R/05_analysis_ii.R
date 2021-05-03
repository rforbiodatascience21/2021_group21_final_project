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
data_clean_pca <- read_tsv(file = "data/03_data_clean_pca.tsv.gz")

# try to do something to insert also the target attribute by using function names() that gives column names of a dataframe
model_data <- data_clean_pca %>% # original data size (494x16)
  pivot_longer(!status, names_to = "measurements", values_to = "values") %>% # create a pivot_longer structure (7410x3) -> (status, measurements, values)
  group_by(measurements) %>% # as we have all rows now in the same column in the pivot_longer, it is grouped by measurements -> original columns
  nest %>% # this function puts together each value for each measurement with the respective status (measurements, data), where data is a tibble with the value of the measurement and status for each row
  ungroup %>%
  mutate(model = map(data, ~ glm(status ~ values, # we apply a General Liner Model for each combination of attributes with respect to status
                                 data = .,
                                 family = binomial(link = "logit")))) %>% # binomial and logit are used as status is binary
  mutate(model_tidy = map(model, ~tidy(., conf.int = TRUE))) %>% # tidy gives the results of the fitting in a nice form in a tibble
  unnest(model_tidy) %>% # the tibble we just created is unnested, showing all results
  filter(str_detect(term, "values")) %>% # get rid of the intercepts and just keep the linear part
  mutate(identified_as = case_when(p.value < 0.05 ~ "Significant",
                                   TRUE ~ "Non-significant")) %>% # mark each fitting depending on its performance with p value
  mutate(neg_log10_p = -log10(p.value)) # create a column required for the Manhattan plot

# Visualize data ----------------------------------------------------------

# Manhattan plot

Manhattan_plot <- model_data %>% 
  ggplot(aes(x = reorder(measurements, -neg_log10_p),
             y = neg_log10_p,
             colour = identified_as)) + 
  geom_point() +
  geom_hline(yintercept = -log10(0.05),
             linetype = "dashed") +
  theme_classic(base_family = "Avenir",
                base_size = 8) +
  theme(axis.text.x=element_text(angle=45,hjust=1)) +
  theme(legend.position = "bottom") +
  labs(x = "Parameters",
       y = "Minus log10(p)")

estimate_plot <- model_data %>% 
  ggplot(aes(x = estimate,
             y = reorder(measurements, -estimate),
             colour = identified_as)) +
  geom_vline(xintercept = 0,
             linetype = "dashed") +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low,
                     xmax = conf.high,
                     height = 0.2)) +
  theme_classic(base_family = "Avenir",
                base_size = 8) +
  theme(axis.text.y = element_text("Parameters"),
        legend.position = "bottom") + 
  labs(y = "")

# Write data --------------------------------------------------------------
save(model_data, file = "results/GLMmodel_data.RData")
ggsave(Manhattan_plot, file = "results/04_plot_Manhattan.png")
ggsave(estimate_plot, file = "results/04_plot_estimate.png")