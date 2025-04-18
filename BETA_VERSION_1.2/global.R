# Packages Used
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(shinycssloaders)
library(RColorBrewer)
library(plotly)
library(dplyr)
library(ggplot2)
library(tippy)
library(rhandsontable)


### Next ###
# Set Default Values for all input selectors
# Add Column headers for rhandsontables (Management Plot 1 and Management Plot 2)
# Leachable N outputs


# Read in CSV Files
crops <- distinct(read.csv("N_Content_expanded.csv"))
fert <- read.csv("Ammonia_Lost.csv")
soil <- read.csv("denitrification.csv")


soil_drain <- soil %>% select(Soil.Drainage.Classification) %>% slice(1:5)


source('ui.R', local = TRUE)
source('Server.R', local = TRUE)


# Run the application
shinyApp(ui = UI, server = SERVER)