# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library(ggplot2)
library(tidyverse)

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")

# Load data ---------------------------------------------------------------
#We use the same as for PCA since we need parameters to be numeric
data_kmeans <- read_tsv(file = "data/03_data_clean_num.tsv.gz")

## kmean Clustering -------------------------------------------------------
#prepare a table withput y (status alive/death)
data_kmeans_noLabel <- data_kmeans %>% 
  select(-status)

#retire dose, diastolic and sistolic, acid Phosphatase since 
#they were found to not have goow correlation to status
dropCol <- c("diastolicBP","dose","systolicBP","acidPhosphatase")

data_kmeans_noLabel <- data_kmeans_noLabel %>% 
  select(-one_of(dropCol))

#apply kmeans model with k=2 (status alive/death)
kClust <- kmeans(data_kmeans_noLabel, 
                 centers = 2)

#summary(kClust)

#compare with original status to check performance
#create a "confussion" matrix
kmean_aug <- augment(kClust, data_kmeans) %>% 
  rename(cluster = .cluster) %>% 
  select(status, cluster) %>% 
  count(status, cluster) %>% 
  mutate(status = case_when(status == 1 ~ "Dead",
                            status == 0 ~ "Alive")) %>% 
  mutate(percentage = n*100/sum(n))

#plotting as a confussion matrix
plot1 <- ggplot(kmean_aug, aes(x = status, y = cluster)) +
  geom_tile(aes(fill = n), color = "white") +
  geom_text(aes(label = n), vjust = -1) +
  geom_text(aes(label = str_c(round(percentage, 1), "%")), vjust = 1) +
  scale_fill_gradient(low = "white", high = "#00AFBB") +
  theme_minimal() +
  ggtitle("Confussion matrix for k-means clustering") +
  xlab("Status") +
  ylab("predicted cluster") 

# Write data --------------------------------------------------------------
ggsave(plot1, file = "results/06_plot_kmean.png")