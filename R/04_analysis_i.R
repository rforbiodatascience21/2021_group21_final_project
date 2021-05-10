# Clear workspace ---------------------------------------------------------
rm(list = ls())

# Load libraries ----------------------------------------------------------
library("tidyverse")
library("ggridges")
library("ggplot2")
library("ggridges")
library("broom")
library("patchwork")

# Define functions --------------------------------------------------------
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data_clean_aug <- read_tsv(file = "data/03_data_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------
data_clean_aug <- data_clean_aug %>% 
  mutate(treatment = as_factor(treatment)) %>%
  mutate(status = as_factor(status)) %>% 
  mutate(electroCardioG = as_factor(electroCardioG)) %>%
  mutate(performance = as_factor(performance)) %>%
  mutate(stage = as_factor(stage)) %>% 
  mutate(reasonDeathNum = as_factor(reasonDeathNum)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

# Visualize data ----------------------------------------------------------

#Plot 0 - very basic, see the count of stage 3 and 4, we may exclude this
plot0 <- data_clean_aug %>%
  ggplot(aes(stage, ..count..)) + 
  geom_bar(alpha = 0.7, 
           aes(fill = stage), 
           position = "dodge") +
  labs(x = "Cancer stage", 
       y = "Total patients") +
  ggtitle("Total patients depending on cancer stage") +
  geom_text(stat = 'count', 
            aes(label = ..count..), 
            vjust = -0.5) +
  ylim(c(0,300)) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.title = element_text(size = 11, 
                                  hjust = 0.5, 
                                  vjust = 0.5))


#Plot 1 - tumor Size vs cancer Stage
plot1 <- data_clean_aug %>%
  ggplot(aes(y = stage, x = tumorSize, fill = stage)) +
  geom_density_ridges(alpha=0.7) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  ggtitle("Tumor Size Density With Cancer Stages") +
  xlab("Tumor Size (cm2)") +
  ylab("Cancer Stage") + 
  theme_minimal()


#Plot 2 - Reason of Death Count, grouped by stage
plot2 <- data_clean_aug %>% 
  filter(reasonDeath != "not dead" & 
         reasonDeath != "unknown cause" & 
         reasonDeath != "other ca" & 
         reasonDeath != "other non-ca") %>% 
  count(reasonDeath, stage) %>% 
  ggplot(aes(x = reorder(reasonDeath, n, sum), 
             y = n, 
             fill = stage)) + 
  geom_col() +
  scale_fill_manual(values = c( "#00AFBB", "#E7B800")) +
  ggtitle("Death Reason - Grouped By Cancer Stage") +
  xlab("Reason of Death") +
  ylab("Total patients") +   
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 25, 
                                   vjust = 0.8, 
                                   hjust = 0.5, 
                                   size = 11))


#Plot 3 - stage vs acid phosphates
p1 <- data_clean_aug %>% 
  ggplot(aes(x =stage, 
             y = log(acidPhosphatase), 
             fill = stage)) +
  geom_violin() +
  stat_summary(fun = mean, 
               fun.min = mean, 
               fun.max = mean,
               geom = "crossbar", 
               width = 0.25,
               position = position_dodge(width = .25)) +
  labs(x = "Cancer stage", 
       y = "Log of acid phosphatase", 
       color = "Cancer stage") +
  ggtitle("Violin Plot of Acid Phosphatases and SG Index") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() +
  theme(legend.position = "none")


p2 <-  data_clean_aug %>% 
  ggplot(aes(x = stage, 
             y = SGindex, 
             fill = stage)) +
  geom_violin() +
  stat_summary(fun = mean, 
               fun.min = mean, 
               fun.max = mean,
               geom = "crossbar", 
               width = 0.25,
               position = position_dodge(width = .25)) +
  labs(x = "Cancer stage", 
       y = "SG Index", 
       color = "Cancer stage") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() +
  theme(legend.position = "none")


plot3 <- p1 + p2


#plot 4 - survival depending on treatment and by stage
plot4 <- data_clean_aug %>%
  filter(reasonDeath != "unknown cause" & 
         reasonDeath != "other ca" & 
         reasonDeath != "other non-ca") %>% 
  count(dose, status, stage) %>% 
  mutate(stage = case_when(stage == 3 ~ "Stage 3",
                           stage == 4 ~ "Stage 4")) %>% 
  ggplot(aes(x = as_factor(dose), 
             y = n, 
             fill = status)) +
  geom_col(position = "dodge") +
  labs(x= "Dose", 
       y="Total patients", 
       color="Status") +
  ggtitle("Survival Per Treatment - vs Cancer Stage") +
  scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal() +
  facet_wrap(~stage)

#Plot 5 - Reason of death per dose
dataPlot5 <- data_clean_aug %>% 
  filter(reasonDeath != "not dead" &
           reasonDeath != "unknown cause") %>% 
  group_by(reasonDeath, dose) %>% 
  summarise(percentage = n()) %>% 
  group_by(dose) %>% 
  mutate(percentage = percentage/sum(percentage)*100)

#Prepare labels
dose.labs <- c("Placebo", "Estrogen 0.2mg", "Estrogen 1mg", "Estrogen 5mg")
names(dose.labs) <- c("0", "0.2", "1", "5" )

#Make the plot
plot5 <-  dataPlot5 %>%
  ggplot(aes(x = "", y = percentage, fill = reasonDeath)) +
  geom_bar(stat = "identity", width = 1, color = "white" ) +
  coord_polar("y", start = 0) +
  geom_text(size = 2, 
            aes(x = 1.65, label = str_c(round(percentage), "%")), 
            position = position_stack(vjust = 0.5)) +
  theme_void() + # remove background, grid, numeric labels
  labs(title = "Cause of death per treatment") +
  facet_wrap(~ dose, 
             nrow = 1, 
             labeller = labeller(dose = dose.labs)) +
  theme(plot.title = element_text(hjust = 0.5, vjust = 5), 
        legend.position = "bottom", 
        legend.title = element_blank(),
        legend.key.size = unit(0.5, 'cm'),
        legend.text = element_text(size=8)) +
  scale_fill_manual(
    values = c("#ff8c00", "#E7B800", "#9999FF", "#ffc0cb", "#00AFBB", "#68bb59", "#346299")) 



#Plot 6 - %alive versus treatment doses by age
#Make groups of ages
dataPlot6 <- data_clean_aug %>% 
  mutate(ageGroup = case_when(age >= 45 & age < 55 ~ "[45-54]",
                              age >= 55 & age < 65 ~ "[55-64]",
                              age >= 65 & age < 75 ~ "[65-74]",
                              age >= 75 & age < 85 ~ "[75-84]",
                              age >= 85 & age < 95 ~ "[85-94]"))

#Get table: dose + ageGroup + ppl_no
dataPlot6 <- dataPlot6 %>% 
  group_by(dose, ageGroup) %>% 
  summarize(ppl_no = n())  

#Add percentage
#Total of patients per dose
patients_per_dose <- dataPlot6 %>% 
  group_by(dose) %>%
  summarise(ppl_total = sum(ppl_no)) 

#Put data all together
dataPlot6 <- full_join(dataPlot6, patients_per_dose, by = "dose")

#Compute percentage
dataPlot6 <- dataPlot6 %>% group_by(dose) %>% 
  mutate(percentage = round((ppl_no/ppl_total)*100, 1))

#Make the plot
plot6 <- dataPlot6 %>% 
  ggplot(aes(x = ageGroup, y = percentage, fill = ageGroup)) +
  geom_bar(stat = "identity",position = "dodge") +
  facet_wrap(~ dose, nrow = 1, 
             labeller = labeller(dose = dose.labs), 
             strip.position = "bottom") +
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(size = 10),
        strip.placement = "outside",
        strip.background = element_blank(),
        strip.text = element_text(size = 8),
        plot.title = element_text(hjust = 0.5, size = 11),
        legend.title = element_text(size = 9)) +
  scale_y_continuous(expand = c(0,0), limit = c(0,59)) +
  scale_x_discrete(breaks = NULL) +
  scale_fill_manual(
    values = c("#68bb59", "#ffc0cb","#E7B800", "#00AFBB", "#ff8c00"))+
  labs(title = "Survival rate based on treatment per age group", 
       y = "Percentage of alive patients", fill = "Age Group")

#Plot 7 - Bone metastase vs cancer stage
dataPlot7 <- data_clean_aug %>% 
  select(stage,boneMetastase) %>% 
  mutate(boneMetastase = case_when(boneMetastase == 0 ~ "No",
                                   boneMetastase == 1 ~"Yes")) %>% 
  count(stage,boneMetastase) %>% 
  mutate(percentage = round(n*100/sum(n), 1)) 

plot7 <- dataPlot7 %>%
  ggplot(aes(x = stage, y = boneMetastase)) +
  geom_tile(aes(fill = n), color = "white") +
  geom_text (aes(label = n), vjust = -1) +
  geom_text(aes(label = str_c(round(percentage, 1), "%")), 
            vjust = 1) +
  scale_fill_gradient(low = "white", high = "#00AFBB") +
  theme_minimal() +
  ggtitle("Bone metastases vs cancer stage") +
  xlab("Cancer stage") +
  ylab("Bone metastases") 

# Write data --------------------------------------------------------------
ggsave(plot0, file = "results/04_plot_0.png")
ggsave(plot1, file = "results/04_plot_1.png")
ggsave(plot2, file = "results/04_plot_2.png")
ggsave(plot3, file = "results/04_plot_3.png")
ggsave(plot4, file = "results/04_plot_4.png")
ggsave(plot5, file = "results/04_plot_5.png")
ggsave(plot6, file = "results/04_plot_6.png")
ggsave(plot7, file = "results/04_plot_7(not used on presentation).png")