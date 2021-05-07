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
source(file = "R/99_project_functions.R")


# Load data ---------------------------------------------------------------
data <- read_tsv(file = "data/03_data_clean_aug.tsv.gz")


# Wrangle data ------------------------------------------------------------


# User Interface
ui <- fluidPage(theme = shinytheme("united"),
                titlePanel(" Title Here"),
                sidebarLayout(
                  sidebarPanel(
                  .....
                  ),
                  
                  mainPanel(
                    
                    ....
                    
                  )
                  
                ),
)

# Server
server <- function(input, output, session) {
  observe({
    
..
  })
  
  output$plot <- renderPlot({
    
    .....
  })
}


# Create Shiny App
shinyApp(ui = ui, server = server)


# Publish at Shiny App