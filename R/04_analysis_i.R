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



# Visualize data ----------------------------------------------------------

#Plot 0 - very basic, see the count of stage 3 and 4, we may exclude this
plot0 <- data_clean_aug %>%
ggplot(aes(stage, ..count..)) + 
  geom_bar(alpha=0.7,aes(fill = stage), position = "dodge")+
  scale_fill_manual(values=c ("#edae49", "#66a182"))


#Plot 1 - tumor Size vs cancer Stage
plot1 <- data_clean_aug %>%
  ggplot( aes(y=stage, x=tumorSize,  fill=stage)) +
  geom_density_ridges(alpha=0.7) +
  scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
  ggtitle("Tumor Size Density With Cancer Stages") +
  xlab("Tumor Size (cm2)") +
  ylab("Cancer Stage") + 
  theme_ridges()


#Plot 2 - Reason of Death Count, grouped by stage
plot2 <- data_clean_aug %>% 
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
p1 <- data_clean_aug %>% 
          ggplot(aes(x=stage, y=log(acidPhosphatase), fill= stage)) +
                     geom_violin() +
                     stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                                   geom = "crossbar", 
                                   width = 0.25,
                                   position = position_dodge(width = .25)) +
                     labs(x= "Cancer stage", y="Log of acid phosphatase", color="Cancer stage") +
                     scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
                     theme(legend.position = "none")

p2 <-  data_clean_aug %>% 
                      ggplot(aes(x=stage, y=SGindex, fill= stage)) +
                      geom_violin() +
                      stat_summary(fun = mean, fun.min = mean, fun.max = mean,
                                   geom = "crossbar", 
                                   width = 0.25,
                                   position = position_dodge(width = .25)) +
                      labs(x= "Cancer stage", y="SG Index", color="Cancer stage") +
                      scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
                      theme(legend.position = "none")


plot3 <- p1 + p2


#Plot 4 - Reason of death per dose - (in process)
#Get distribution of reason of death by treatment dose
dataPlot4 <- data_clean_aug %>% 
  filter(reasonDeath!="not dead" &
         reasonDeath!="unknown cause") %>% 
  group_by(reasonDeath,dose) %>% 
  summarise(percentage=n()) %>% 
  group_by(dose) %>% 
  mutate(percentage=percentage/sum(percentage)*100)

#Make the plot
dose.labs <- c("Placebo", "Estrogen 0.2mg", "Estrogen 1mg", "Estrogen 5mg")
names(dose.labs) <- c("0", "0.2", "1", "5" )

plot4 <- ggplot(dataPlot4, aes(x="", y=percentage, fill=reasonDeath)) +
  geom_bar(stat="identity", width=1, color="white" ) +
  coord_polar("y", start=0) +
  geom_text(size=2, aes(x = 1.6, label = paste0(round(percentage), "%")), 
            position = position_stack(vjust = 0.5))+
  theme_void()+ # remove background, grid, numeric labels
  #scale_fill_brewer(palette = "Dark2") +
  labs(title = "Cause of death per treatment")+
  facet_wrap(~ dose, nrow = 1, labeller = labeller(dose = dose.labs)) +
  theme(plot.title = element_text(hjust = 0.5), 
        legend.position = "bottom", 
        legend.title = element_blank())


#Plot 5 - %alive versus treatment doses by age
#Make groups of ages
dataPlot5 <- data_clean_aug %>% mutate(
  # Create groups based on tumor size
  ageGroup = case_when(
    age >= 45 & age < 55 ~ "[45-54]",
    age >= 55 & age < 65 ~ "[55-64]",
    age >= 65 & age < 75 ~ "[65-74]",
    age >= 75 & age < 85 ~ "[75-84]",
    age >= 85 & age < 95 ~ "[85-94]"))

#Get table: dose + ageGroup + ppl_no
dataPlot5 <- dataPlot5 %>% 
  group_by(dose, ageGroup) %>% 
  dplyr::summarize(ppl_no = n())  

#Add percentage
#-----Total of patients per dose
patients_per_dose <- dataPlot5 %>% group_by(dose) %>%
  dplyr::summarise(ppl_total = sum(ppl_no)) 

#-----Put data all together
dataPlot5 <- full_join(dataPlot5, patients_per_dose, by = "dose")

#-----Compute percentage
dataPlot5 <- dataPlot5 %>% group_by(dose) %>% 
  mutate(percentage=round((ppl_no/ppl_total)*100, 1))

#Make the plot

plot5 <- ggplot(dataPlot5, aes(x=ageGroup, y=percentage, fill = ageGroup)) +
  geom_bar(stat="identity",position="dodge")+
  facet_wrap(~ dose, nrow = 1, labeller = labeller(dose = dose.labs),strip.position = "bottom")+
  theme(panel.background = element_blank(), 
        axis.line = element_line(colour = "black"),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        legend.position = "none",
        strip.placement = "outside",
        strip.background = element_blank(),
        plot.title = element_text(hjust = 0.5)) + 
  scale_y_continuous(expand = c(0,0), limit = c(0,59))+
  scale_x_discrete(breaks = NULL)+
  geom_text(aes(label=ageGroup, colour=ageGroup), 
            vjust=0.5, 
            hjust=0.5, 
            size=2,
            nudge_y= 1) +
  labs(title= "Survival rate based on treatment per age group", y = "Percentage of alive patients")

#plot 6
dataPlot6 <- data_clean_aug %>% 
  select(stage,boneMetastase) %>% 
  mutate(boneMetastase = case_when(boneMetastase == 0 ~ "No",
                                   boneMetastase == 1 ~"Yes")) %>% 
  count(stage,boneMetastase) %>% 
  mutate(percentage = round(n*100/sum(n),1)) 

plot6 <- ggplot(dataPlot6, aes(x=stage,y=boneMetastase)) +
  geom_tile(aes(fill = n), color = "white") +
  geom_text (aes(label=n), vjust = -1) +
  geom_text(aes(label = paste0(round(percentage,1),"%")), vjust = 1) +
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
