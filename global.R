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
library(plotly) #for interactive graphs

###### Main datasets
glasgow_map_regions <- readRDS("data/glasgow_regions.rds") # shapefile for glasgow city region areas
indicators_data <- readRDS("data/indicators_data.rds") # read in indicators data
#indicator_definitions <- readRDS("data/indicator_definitions.rds") #read in definitions for indicators

###### Calculations for app
#making a table of the latest data for the summary page as indicators differ in year for latest data
#table of most recent years
most_recent_years <- indicators_data %>% group_by(Indicator,Region) %>% summarise(recent_year = max(Year), Years_available = n())
#join latest years with original table to get value for that year
latest_data <- inner_join(most_recent_years,indicators_data, by = c("Region","Indicator","recent_year"="Year")) %>% rename(Year = recent_year)
#get unique list for data sources
source_data <- unique(indicators_data[,c('Indicator','Source')])
#calculate % changes for 1 year, 3, 5 and 10 year periods where appropriate
change_data <- inner_join(indicators_data, most_recent_years, by = c("Region","Indicator")) %>% mutate_at(vars(Year,recent_year), ~as.numeric(.))
change_data <- change_data %>% mutate(year_diff = recent_year - Year) %>% select(-Source)
#one_year_diff <- latest_data_3 %>% filter(Years_available>1 & (year_diff==0|year_diff==1)) %>% arrange(Region,Indicator,Year)
#three_year_diff <- latest_data_3 %>% filter(Years_available>3 & (year_diff==0|year_diff==3)) %>% arrange(Region,Indicator,Year)
#five_year_diff <- latest_data_3 %>% filter(Years_available>5 & (year_diff==0|year_diff==5)) %>% arrange(Region,Indicator,Year)
#ten_year_diff <- latest_data_3 %>% filter(Years_available>10 & (year_diff==0|year_diff==10)) %>% arrange(Region,Indicator,Year)
#calculate differences across different time ranges
latest_one_year <- change_data %>% filter(Years_available>1 & (year_diff==0|year_diff==1)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "1 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE, Value - lag(Value),(Value - lag(Value))/lag(Value)*100))
latest_three_year <- change_data %>% filter(Years_available>3 & (year_diff==0|year_diff==3)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "3 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE,Value - lag(Value),(Value - lag(Value))/lag(Value)*100))
latest_five_year <- change_data %>% filter(Years_available>5 & (year_diff==0|year_diff==5)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "5 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE,Value - lag(Value),(Value - lag(Value))/lag(Value)*100))
latest_ten_year <- change_data %>% filter(Years_available>10 & (year_diff==0|year_diff==10)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "10 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE,Value - lag(Value),(Value - lag(Value))/lag(Value)*100))  
indicators_change <- rbind(latest_one_year,latest_three_year,latest_five_year,latest_ten_year)
indicators_change <- indicators_change %>% filter(!is.na(change_value))

####### Lists for the drop-down menus    
glasgow_regions <- c("East Dunbartonshire", "East Renfrewshire", "Glasgow City", "Inverclyde",
                    "North Lanarkshire", "Renfrewshire", "South Lanarkshire", "West Dunbartonshire")
uk_regions <- c("Cardiff Capital Region", "Edinburgh and South East Scotland City Region", "Glasgow City Region",
                "Greater Manchester", "Liverpool City Region", "North of Tyne", "Sheffield City Region", 
                "West Midlands", "West of England", "West Yorkshire")

indicators <- c(unique(indicators_data$Indicator))
indicators_cleaned <- c(unique(indicators_data$Indicator[!grepl("[A-Z] :",indicators_data$Indicator)]))

comparators <- c("Scotland", "Scottish City Region Top Quartile Averages", "City Region Top Quartile Averages", "United Kingdom")