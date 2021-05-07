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
  mutate(treatment = as_factor(treatment)) %>%
  mutate(status = as_factor(status)) %>% 
  mutate(electroCardioG = as_factor(electroCardioG)) %>%
  mutate(performance = as_factor(performance)) %>%
  mutate(stage = as_factor(stage)) %>% 
  mutate(reasonDeathNum = as_factor(reasonDeathNum)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

# Model data
model_data = model_logit(data_clean_pca)

# we could add some simple command such as

#gravier_data %>%
#group_by(outcome) %>%
#  summarise(n = n())

# to get quick info about distribution in our dataset in case it is relevant

# Manhattan plot

model_data %>% 
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

model_data %>% 
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

# we run the PCA and plot the results
PCA(data_clean_pca)

# Visualize data ----------------------------------------------------------

#Plot 0 - very basic, see the count of stage 3 and 4, we may exclude this
plot_0 <- data_clean_aug %>%
ggplot(aes(stage, ..count..)) + 
  geom_bar(alpha=0.7,aes(fill = stage), position = "dodge")+
  scale_fill_manual(values=c ("#edae49", "#66a182"))


#Plot 1 - tumor Size vs cancer Stage
plot_1 <- data_clean_aug %>%
  ggplot( aes(y=stage, x=tumorSize,  fill=stage)) +
  geom_density_ridges(alpha=0.7) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  ggtitle("Tumor Size Density With Cancer Stages") +
  xlab("Tumor Size (cm2)") +
  ylab("Cancer Stage") + 
  theme_ridges()


#Plot 2 - Reason of Death Count, grouped by stage
plot_2 <- data_clean_aug %>% 
  filter(!is.na(reasonDeath) & 
           reasonDeath != "unknown cause" & 
           reasonDeath != "other ca" & 
           reasonDeath != "other specific non-ca" &
           reasonDeath != "unspecified non-ca") %>% 
  count(reasonDeath, stage) %>% 
  ggplot(aes(x = reorder(reasonDeath, n, sum), y = n, fill = stage)) + 
  geom_col() +
  scale_fill_manual(values=c( "#00AFBB", "#E7B800")) +
  ggtitle("Count Of Death Reasons - Grouped By Cancer Stage") +
  xlab("Reason of Death") +
  ylab("Count") +   
  theme_ridges() +
  theme(axis.text.x = element_text(angle = 25, vjust = 0.8, hjust=0.5, size=11))


#Plot 3 - stage vs acid phosphates
plot_3 <- data_clean_aug %>% 
          ggplot(aes(x=stage, y=log(acidPhosphatase), fill= stage)) +
                     geom_violin() +
                     stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                                   geom = "crossbar", 
                                   width = 0.25,
                                   position = position_dodge(width = .25)) +
                     ggtitle("Log of acid Phosphatase depending on cancer stage") + 
                     labs(x= "Cancer stage", y="Log of acid phosphatase", color="Cancer stage") +
                     theme(legend.position = "none")



#Plot 4 - Reason of death per dose - (in process)
#Get distribution of reason of death by treatment dose
distribution_death_reason <- data_clean_aug %>% 
  filter(reasonDeath!="not dead") %>% 
  group_by(reasonDeath,dose) %>% 
  summarise(percentage=n()) %>% 
  group_by(dose) %>% 
  mutate(percentage=percentage/sum(percentage)*100)

#Plot
dose.labs <- c("Placebo", "Estrogen 0.2mg", "Estrogen 1mg", "Estrogen 5mg")
names(dose.labs) <- c("0", "0.2", "1", "5" )

plot_4 <- ggplot(distribution_death_reason, aes(x="", y=percentage, fill=reasonDeath)) +
  geom_bar(stat="identity", width=1, color="white" ) +
  coord_polar("y", start=0) +
  geom_text(aes(x = 1.6, label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5))+
  theme_void()+ # remove background, grid, numeric labels
  #scale_fill_brewer(palette = "Dark2") +
  labs(title = "Cause of death per treatment")+
  facet_wrap(~ dose, nrow = 1, labeller = labeller(dose = dose.labs)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom", 
        legend.title = element_blank())


# Write data --------------------------------------------------------------
ggsave(plot_0, file = "results/04_plot_0.png")
ggsave(plot_1, file = "results/04_plot_1.png")
ggsave(plot_2, file = "results/04_plot_2.png")
ggsave(plot_3, file = "results/04_plot_3.png")
