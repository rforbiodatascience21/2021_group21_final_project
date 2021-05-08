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
# source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/03_data_clean_aug.tsv.gz")

# Wrangle data ------------------------------------------------------------
data <- data %>% 
  mutate(treatment = as_factor(treatment)) %>%
  mutate(status = as_factor(status)) %>% 
  mutate(electroCardioG = as_factor(electroCardioG)) %>%
  mutate(performance = as_factor(performance)) %>%
  mutate(stage = as_factor(stage)) %>% 
  mutate(reasonDeathNum = as_factor(reasonDeathNum)) %>% 
  mutate(boneMetastase = as_factor(boneMetastase))

# User Interface
ui <- fluidPage(theme = shinytheme("united"),
                
                titlePanel(" Title Here "),
                sidebarLayout(
                  sidebarPanel(
                    selectInput("column", 
                                label = "Density", 
                                choices=c("Age", "Weight Index", "Tumor Size"),
                                selected= "Cancer Stage"),
                    
                  ),
                  
                  mainPanel(
                    
                    plotOutput(outputId = "plot"),
                    tags$h5(" Title xxx \n NOT COMPLETE - ON GOING "),
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
    
    if (input$column == "Age") {
      df <- data %>% 
            select(stage, age)
      df <- rename(df, col = age)
    }
    if (input$column == "Weight Index") {
      df <- data %>% 
            select(stage, weightIndex)
      df <- rename(df, col = weightIndex)
    }
    if (input$column == "Tumor Size") {
      df <- data %>% 
        select(stage, tumorSize)
      df <- rename(df, col = tumorSize)
    }
    
    df %>%
      ggplot( aes(y=stage, x=col,  fill=stage)) +
      geom_density_ridges(alpha=0.7) +
      scale_fill_manual(values=c("#00AFBB", "#E7B800")) +
      ggtitle("Tumor Size Density With Cancer Stages") +
      xlab("Tumor Size (cm2)") +
      ylab("Cancer Stage") + 
      theme_ridges()
  })
}


# Create Shiny App
shinyApp(ui = ui, server = server)


# Publish at Shiny App
