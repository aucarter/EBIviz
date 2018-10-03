# Setup
library(shiny);library(data.table); library(ggplot2); library(RColorBrewer); library(plotly); library(shinythemes)

# Paths
data.dir <- "data/prepped/"
data.path <- paste0(data.dir, "mort.csv")

# Data
dt <- fread(data.path)

# Define UI for application that plots random distributions 
shinyUI(
  fluidPage(
  # theme = shinythemes::shinytheme(theme = "lumen"),
  
  # Application title
  headerPanel("EBI viz"),
  
  sidebarPanel(
    # Location
    selectInput(inputId = "cloc", 
                label = "Location", 
                choices = sort(unique(bound.dt$location)),
                selected = "Nepal"),
    # Age
    selectInput(inputId = "cage", 
                label = "Age group", 
                choices = sort(unique(bound.dt$age)),
                selected = "Under 5"),
    
    # Cause
    selectInput(inputId = "ccause", 
                label = "Cause", 
                choices = c("All causes (per 1000 live births)", sort(unique(bound.dt$cause))),
                selected = "All causes (per 1000 live births)"),
    
    # Year range
    sliderInput(inputId = "range", 
                label = "Year Range:",
                min = 1990, max = 2015,
                step = 5,
                value = c(2000,2015),
                sep = ""),
    
    # 5-year average bar
    checkboxInput(inputId = "bar", 
                  label = "5-year Avg. (Bar Chart)",
                  value = FALSE),
    
    # Download
    downloadButton("Download"),
    width = 3
  ),
  

  
  # Show a plot of the generated distribution
  mainPanel(
    plotOutput("plot")
  )
))