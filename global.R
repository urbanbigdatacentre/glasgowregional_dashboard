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
library(sf) # for working with shapefiles
library(rmarkdown) #PDF report format
library(knitr) #for genrating PDF report
library(plotly) #for interactive graphs
library(DT) # to output summary table
library(RColorBrewer) # for seeting colour scales
library(raster)
library(shinyBS) # for modals
library(tinytex) # for downloading .doc and .pdf extension reports
library(webshot) # for downloading pngs
webshot::install_phantomjs(force=FALSE)
library(htmlwidgets)
library(mapview) #to download png of map
library(htmltools) #for tooltips
library(stringr) # for editing text of labels on graphs

###### Main datasets
glasgow_map_regions <- readRDS("data/glasgow_regions.rds") # shapefile for glasgow city region areas
uk_map_regions <- readRDS("data/uk_regions.rds")
NUTS_3_regions <- readRDS("data/NUTS3_regions.rds")
#change the name of area_columns
colnames(glasgow_map_regions@data)[colnames(glasgow_map_regions@data)=="lad19nm"]<- "area_name"
colnames(uk_map_regions@data)[colnames(uk_map_regions@data)=="cauth19nm"]<- "area_name"
colnames(NUTS_3_regions@data)[colnames(NUTS_3_regions@data)=="nuts318nm"]<- "area_name"

indicators_data <- readRDS("data/indicators_data_trial.rds") # read in indicators data
#trim any unnecessary white spaces that may have been accidentally added during data update
indicators_data <- indicators_data %>% mutate_if(is.character, ~str_trim(.))
#clean up defintions
indicators_data$Definition[is.na(indicators_data$Definition)|indicators_data$Definition==""] <- "No description available"

#wrapped area_labels for graphs
indicators_data$Region_label <- str_wrap(indicators_data$Region, width = 15)

###### Calculations for app
#making a table of the latest data for the summarhy page as indicators differ in year for latest data
#table of most recent years
most_recent_years <- indicators_data %>% group_by(Indicator,Job_sector,Region) %>% summarise(recent_year = max(Year), Years_available = n()) %>% ungroup()
#join latest years with original table to get value for that year
latest_data <- inner_join(most_recent_years,indicators_data, by = c("Region","Indicator","Job_sector","recent_year"="Year")) %>% rename(Year = recent_year)
latest_data$Value <- round(latest_data$Value,2)

#get unique list for data sources
source_data <- unique(indicators_data[,c('Indicator','Source')])
#calculate % changes for 1 year, 3, 5 and 10 year periods where appropriate
change_data <- inner_join(indicators_data, most_recent_years, by = c("Region","Indicator","Job_sector")) %>% mutate_at(vars(Year,recent_year), ~as.numeric(.))
change_data <- change_data %>% mutate(year_diff = recent_year - Year) %>% dplyr::select(-Source)

latest_one_year <- change_data %>% filter(Years_available>1 & (year_diff==0|year_diff==1)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "1 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE, Value - lag(Value),(Value - lag(Value))/lag(Value)*100))
latest_three_year <- change_data %>% filter(Years_available>3 & (year_diff==0|year_diff==3)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "3 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE,Value - lag(Value),(Value - lag(Value))/lag(Value)*100)) %>% ungroup()
latest_five_year <- change_data %>% filter(Years_available>5 & (year_diff==0|year_diff==5)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "5 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE,Value - lag(Value),(Value - lag(Value))/lag(Value)*100)) %>% ungroup()
latest_ten_year <- change_data %>% filter(Years_available>10 & (year_diff==0|year_diff==10)) %>% arrange(Region,Indicator,Year) %>% group_by(Indicator,Region) %>% summarise(change = "10 year change", change_value = ifelse(grepl("%|[A-Z] : |\\([a-z|0-9].+\\)",Indicator)==TRUE,Value - lag(Value),(Value - lag(Value))/lag(Value)*100)) %>% ungroup()  
indicators_change <- rbind(latest_one_year,latest_three_year,latest_five_year,latest_ten_year)
indicators_change <- indicators_change %>% filter(!is.na(change_value))

#for individual area need to be able to print out value where NUTS level 3 areas used instead of LA's (i.e. for service exports) - so need to split combined areas
NUTS_3_latest_data <- latest_data %>% filter(Indicator=="Service Exports Per Job (£)")
NUTS_3_latest_data <- separate_rows(NUTS_3_latest_data,3,sep=",")
NUTS_3_latest_data <- separate_rows(NUTS_3_latest_data,3,sep=" and ")
NUTS_3_latest_data$Region <- str_trim(NUTS_3_latest_data$Region)
NUTS_3_indicators_change <- indicators_change %>% filter(Indicator=="Service Exports Per Job (£)")
if(nrow(NUTS_3_indicators_change)>0) {NUTS_3_indicators_change <- separate_rows(NUTS_3_indicators_change,2,sep=",")
                            NUTS_3_indicators_change <- separate_rows(NUTS_3_indicators_change,2,sep=" and ")
                            str_trim(NUTS_3_indicators_change$Region)
}

####### Lists for the drop-down menus    
glasgow_regions <- c("East Dunbartonshire", "East Renfrewshire", "Glasgow City", "Inverclyde",
                    "North Lanarkshire", "Renfrewshire", "South Lanarkshire", "West Dunbartonshire")
uk_regions <- c("Cardiff Capital Region", "Edinburgh and South East Scotland City Region", "Glasgow City Region",
                "Greater Manchester", "Liverpool City Region", "North of Tyne", "Sheffield City Region", 
                "West Midlands", "West of England", "West Yorkshire")
combined_regions_list <- c("East Dunbartonshire, West Dunbartonshire and Helensburgh & Lomond", "Glasgow City","Inverclyde, East Renfrewshire and Renfrewshire","North Lanarkshire", "South Lanarkshire")

all_indicators <- c(unique(indicators_data$Indicator))
indicators_cleaned <- c(unique(indicators_data$Indicator[indicators_data$Indicator!="Jobs by Sector"]))
  #c(unique(indicators_data$Indicator[!grepl("[A-Z] :",indicators_data$Indicator)]))

  
job_sectors <- unique(indicators_data$Job_sector[indicators_data$Indicator == "Jobs by Sector" & grepl("[A-Z] :",indicators_data$Job_sector)])

comparators <- c("Scotland", "Scottish City Region Top Quartile Averages", "City Region Top Quartile Averages", "United Kingdom")

combined_areas_one <- c("East Dunbartonshire", "West Dunbartonshire")
combined_areas_two <- c("Inverclyde", "East Renfrewshire","Renfrewshire")

#Function for tooltip measure
tooltip_measure <- function(data){
  if (unique(data$Measure == "Percentage (%)")){ measure_to_show <- "%"
  } else if (unique(data$Measure == "GBP (£) weekly")) { measure_to_show <- "GBP weekly"
  } else if (unique(data$Measure == "GBP (£)")) { measure_to_show <- "GBP"
  } else if (unique(data$Measure == "Count")) { measure_to_show <- ""
  } else { measure_to_show <- unique(data$Measure) }
  return(measure_to_show)
}