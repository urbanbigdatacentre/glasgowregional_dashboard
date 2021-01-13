library(shiny)
library(shinyjs)
library(shinythemes)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets) #for ui elements
library(tidyverse) #for manipulating data
library(leaflet) #for drawing graph
library(sp) # mnipulating spatial data
library(rgdal) #for reading in shapefile
library(sf)
library(rmarkdown)
library(knitr)

glasgow_map_regions <- readRDS("data/glasgow_regions.rds") # shapefile for glasgow city region areas
indicators_data <- readRDS("data/indicators_data.rds") # read in indicators data

#making a table of the latest data for the summary page as indicators differ in year for latest data
#table of most recent years
most_recent_years <- indicators_data %>% group_by(Indicator,Region) %>% summarise(Year = max(Year))
#join latest years with original table to get value for that year
latest_data <- inner_join(most_recent_years,indicators_data, by = c("Region","Indicator","Year"))
#get unique list for data sources
source_data <- unique(indicators_data[,c('Indicator','Source')])
#calculate % changes for 1 year, 3, 5 and 10 year periods where appropriate
change_periods <- data.frame()
change_periods 
  
#list for the drop-down menus    
glasgow_regions <- c("East Dunbartonshire", "East Renfrewshire", "Glasgow City", "Inverclyde",
                    "North Lanarkshire", "Renfrewshire", "South Lanarkshire", "West Dunbartonshire")
uk_regions <- c("Cardiff Capital Region", "Edinburgh and South East Scotland City Region", "Glasgow City Region",
                "Greater Manchester", "Liverpool City Region", "North of Tyne", "Sheffield City Region", 
                "West Midlands", "West of England", "West Yorkshire")

indicators <- c(unique(indicators_data$Indicator))
indicators_cleaned <- c(unique(indicators_data$Indicator[!grepl("[A-Z] :",indicators_data$Indicator)]))

comparators <- c("Scotland", "Scottish City Region Top Quartile Averages", "City Region Top Quartile Averages", "United Kingdom")