# Clear workspace ---------------------------------------------------------
rm(list = ls())


# Load libraries ----------------------------------------------------------
library(shiny)
library(dplyr)
library(shinythemes)
library(ggplot2)
library(tidyverse)
library(reshape2)
library(rsconnect)

# Define functions --------------------------------------------------------
source(file = "../R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "../data/03_data_clean_aug.tsv.gz")

# Wrangle data ------------------------------------------------------------
data <- data %>% 
  mutate(treatment = as_factor(treatment)) %>%
  mutate(status = as_factor(status)) %>% 
  mutate(electroCardioG = as_factor(electroCardioG)) %>%
  mutate(performance = as_factor(performance)) %>%
  mutate(stage = as_factor(stage)) %>% 
  mutate(reasonDeathNum = as_factor(reasonDeathNum)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

#add string values instead of 0 and 1 for bone metastases
data <- data %>% 
  mutate(boneMetastase = case_when(boneMetastase == 0 ~ "No",
                                   boneMetastase == 1 ~ "Yes"))


# User Interface
ui <- fluidPage(theme = shinytheme("united"),
                
                titlePanel(" Data explorer - Prostate cancer clinical data "),
                sidebarLayout(
                  sidebarPanel(
                    
                    selectInput("category", 
                                label = "Category", 
                                choices=c("Cancer Stage", "Treatment Dose", "Performance","Bone Metastases"),
                                selected= "Cancer Stage"),
                    
                    selectInput("density", 
                                label = "Density", 
                                choices=c("Age", "Weight Index", "Tumor Size", "Systolic Blood Pressure",
                                          "Diastolic Blood Pressure","SG Index"),
                                selected= "Age"),

                  ),
                  
                  mainPanel(
                    
                    plotOutput(outputId = "plot"),
                    tags$style(type="text/css",
                               ".shiny-output-error { visibility: hidden; }",
                               ".shiny-output-error:before { visibility: hidden; }"
                    )
                  )
                ),
)

# Server
server <- function(input, output, session) {
  observe({
    
  })
  
  output$plot <- renderPlot({
    
  pairs <- tribble(~key, ~val,
                   "Cancer Stage", "stage",
                   "Weight Index", "weightIndex",
                   "Tumor Size", "tumorSize",
                   "Treatment Dose", "dose",
                   "Age", "age",
                   "Performance", "performance",
                   "Systolic Blood Pressure", "systolicBP",
                   "Diastolic Blood Pressure", "diastolicBP",
                   "Bone Metastases", "boneMetastase",
                   "SG Index","SGindex")
    
    pairs <- spread(pairs, key, val)
    
    df <- shiny_df( first(pairs %>% select(input$density)) ,first(pairs %>% select(input$category)), data)
    
    if (input$category == "Treatment Dose") {
      dose_df <- data %>%
        mutate(dose = case_when (dose == 0.0 ~ "Placebo",
                                 dose == 0.2 ~ "Estrogen 0.2 mg",
                                 dose == 1 ~ "Estrogen 1 mg",
                                 dose == 5 ~ "Estrogen 5 mg"))
      df <- shiny_df( first(pairs %>% select(input$density) ) ,first(pairs %>% select(input$category)),dose_df)
      df <- arrange(df, category)
    }
    
    if (input$category == "Performance") {
      perf_df <- data %>%
                 filter(performance!="confined to bed")
      df <- shiny_df( first(pairs %>% select(input$density) ) ,first(pairs %>% select(input$category)),perf_df)
      df <- arrange(df, category)
    } 
    
    df %>%
      ggplot( aes(y=category, x=density, fill=category)) +
      geom_density_ridges(alpha=0.7) +
      ggtitle(str_c(input$density, " Density Plot Categorized by ", input$category)) +
      xlab(str_c("Density - ",input$density)) +
      ylab(str_c("Category - ",input$category)) + 
      theme_minimal()
  })
}


# Create Shiny App
shinyApp(ui = ui, server = server)


# Publish at Shiny App
