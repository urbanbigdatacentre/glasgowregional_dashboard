server <- function(input, output, session) {
  
##############################
#Landing page
##############################
output$regionmap <- renderLeaflet({
  leaflet(data=glasgow_map_regions, options=leafletOptions(zoomControl = FALSE)) %>% addPolygons(color = "white",
                                                                     weight = 1,
                                                                     smoothFactor = 0.5,
                                                                     opacity = 1.0,
                                                                     fillOpacity = 0.5,
                                                                     highlightOptions = highlightOptions(color = "#E9BD43",
                                                                                                         weight = 2,
                                                                                                         bringToFront = TRUE),
                                                                     layerId =~lad19nm,
                                                                     #options = pathOptions(pane= "highlight", clickable=TRUE), 
                                                                     label=~lad19nm)
})

#this records what area has been selected internally
observeEvent(input$regionmap_shape_click, { 
  p <- input$regionmap_shape_click
  print(p$id)
  shinyjs::hide(id="intro_text")
  shinyjs::show(id="area_detail")
  }, ignoreInit = TRUE)

#this is to print out area on UI
output$region_name <- renderText({input$regionmap_shape_click$id})

#printing out summary statistics for the area selected in map - i.e. the latest data for each indicator
#rounding decimals to 2 decimal places
#rounding % to 3 significant figures
output$region_population <- renderText({format(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Total Population"],big.mark=",")})
output$region_jobs <- renderText({format(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Total Jobs"], big.mark=",")})
output$region_GVAworked <- renderText({paste0("£",round(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "GVA per hour worked (£)"],2))})
output$region_enterprises <- renderText({round(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"],2)})
output$region_serviceexports <- renderText({paste0("£",round(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Service Exports Per Job (£)"],2))})
output$region_medianincome <- renderText({round(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Residence-based Full-time Median Weekly Earnings"],2)})
output$region_noqualification <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Working-age population with no qualifications (%)"],3),"%")})
output$region_unemployed <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Unemployment Rate 16-64 (%)"],3),"%")})
output$region_ecoinactive <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Economic Inactivity Rate 16-64 (%)"],3),"%")})
output$region_claimcount <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"],3),"%")})
output$region_childpoverty <- renderText({paste0(signif((latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "% of children in poverty (after housing costs)"]),3),"%")})
output$region_emissions <- renderText({paste0(round(latest_data$Value[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Emissions per capita (tonnes)"],2)," tonnes")})

#Hover to show extra info about data source, year of data being shown, and % changes over specific time periods
#Total Population
output$pop_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Total Population"]
  a(href=link,"Data Source") })
output$pop_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Total Population"]
  p(recent_year) })
output$pop_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "3 year change"],2)
  five_change <-round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "5 year change"],2)
  ten_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>","<br>","<p>",ten_change,"% 10-year change","</p>"))
})

#Total jobs
output$jobs_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Total Jobs"]
  a(href=link,
    "Data Source") })
output$jobs_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Total Jobs"]
  p(recent_year) })
output$jobs_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Jobs" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Jobs" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Total Jobs" & indicators_change$change == "5 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>"))
})

#GVA worked
output$GVAworked_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "GVA per hour worked (£)"]
  a(href=link,
    "Data Source") })
output$GVAworked_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "GVA per hour worked (£)"]
  p(recent_year) })
output$GVAworked_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>","<br>","<p>",ten_change,"% 10-year change","</p>"))
})

#Enterprises
output$enterprises_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  a(href=link,
    "Data Source") })
output$enterprises_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  p(recent_year) })
output$enterprises_year_change <- renderUI({
  one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Service exports
output$serviceexports_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Service Exports Per Job (£)"]
  a(href=link,
    "Data Source") })
output$serviceexports_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Service Exports Per Job (£)"]
  p(recent_year) })
#only one year for this - will there be more?
output$serviceexports_year_change <- renderUI({
  one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Service Exports Per Job (£)" & indicators_change$change == "1 year change"],2)
  #three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Service Exports Per Job (£)" & indicators_change$change == "3 year change"],2)
  ifelse(length(one_change)==0,HTML("No historical data available"),HTML(paste0("<p>",one_change,"% 1-year change","</p>")))#,"<br>","<p>",three_change,"% 3-year change","</p>"))
     })

#Median Income
output$medianincome_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Residence-based Full-time Median Weekly Earnings"]
  a(href=link,
    "Data Source") })
output$medianincome_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Residence-based Full-time Median Weekly Earnings"]
  p(recent_year) })
output$medianincome_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "3 year change"],2)
  five_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>","<br>","<p>",ten_change,"% 10-year change","</p>"))
})

#Working age population with no qualification
output$noqualification_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Working-age population with no qualifications (%)"]
  a(href=link,
    "Data Source") })
output$noqualification_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Working-age population with no qualifications (%)"]
  p(recent_year) })
output$noqualification_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Unemployed
output$unemployed_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Unemployment Rate 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$unemployed_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Unemployment Rate 16-64 (%)"]
  p(recent_year) })
output$unemployed_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "3 year change"],2)
  five_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "5 year change"],2)
  ten_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Economic Inactivity
output$ecoinactive_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Economic Inactivity Rate 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$ecoinactive_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Economic Inactivity Rate 16-64 (%)"]
  p(recent_year) })
output$ecoinactive_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Claimant count
output$claimcount_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$claimcount_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"]
  p(recent_year) })
output$claimcount_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "3 year change"],2)
  five_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "5 year change"],2)
  ten_change <-round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Child poverty
output$childpoverty_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "% of children in poverty (after housing costs)"]
  a(href=link,
    "Data Source") })
output$childpoverty_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "% of children in poverty (after housing costs)"]
  p(recent_year) })
output$childpoverty_year_change <- renderUI({
  one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "% of children in poverty (after housing costs)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "% of children in poverty (after housing costs)" & indicators_change$change == "3 year change"],2)
  HTML(paste0("<p>",one_change,"% 1-year point change","</p>","<br>","<p>",three_change,"% 3-year point change","</p>"))
})

#Emissions
output$emissions_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Emissions per capita (tonnes)"]
  a(href=link,
    "Data Source") })
output$emissions_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionmap_shape_click$id & latest_data$Indicator == "Emissions per capita (tonnes)"]
  p(recent_year) })
output$emissions_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "5 year change"],2)
  ten_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Creating the code to be able to download a summary of the area being presented
#request whether the user wants it by pdf or word when downloading
output$single_area_summary_md <- downloadHandler(
  filename = paste0(input$regionmap_shape_click$id,"_summary","_",Sys.Date(),".doc"),
  content = function(file){
    tempReport <- file.path(tempdir(), "region_summary_download.Rmd")
    file.copy("region_summary_download.Rmd",tempReport, overwrite = TRUE)
    #parameters to pass to Rmd document
    params <- list(Region = input$regionmap_shape_click$id)
    #knitting the document
    rmarkdown:: render(tempReport, 'word_document', output_file = file,
                       params = params,
                       envir =  new.env(parent = globalenv())
                       )
  }
)

##############################################
#Glasgow City Regions comparison page
##############################################
######Definition
definition <- reactive({
  unique(indicators_data$Definition[indicators_data$Indicator == input$uk_economic_indicator_choice])
})

#observing the definitions button click on either page 2 or 3 and howing the definition in a modal
observeEvent(input$definition|input$uk_definition, {
  link <- source_data$Source[source_data$Indicator == input$uk_economic_indicator_choice]
  showModal(modalDialog(
    title = input$uk_economic_indicator_choice,
    p(definition(), style = "font-size: 14px;"),
    br(),
    a(href=link,"Data Source", style = "font-size: 12px;"),
    size = "l",
    easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")
  ))
  #making sure not to run on start of the app
}, ignoreInit = TRUE)


#####Creating reactive datasets###############
#subset data for indicator selected
selected_indicator_data <- reactive ({
    indicators_data %>%
      subset(Indicator == input$economic_indicator_choice &
               Region %in% input$glasgow_region_choice)            
  })

#get the data for latest years by subsetting the latest_year table that only has most recent years' data
latest_indicator_data <- reactive({
  latest_data %>% subset(Indicator %in% input$economic_indicator_choice &
                           Region %in% input$glasgow_region_choice)
})

#subset data for the region that the user selects to be the comparator
comparator_data <- reactive({
  latest_data %>% subset(Indicator %in% input$economic_indicator_choice & Region %in% input$comparator_choice)
})

#create a new table that now has the comparator on each row (needed for bar chart)
selected_indicator_data_year <- reactive ({
  latest_data_filtered <- latest_data %>% subset(Indicator %in% input$economic_indicator_choice &
                           Region %in% input$glasgow_region_choice) %>% mutate(Comparator = comparator_data()$Region, Comp_Value = comparator_data()$Value) 
  latest_data_sorted <- latest_data_filtered[order(match(latest_data_filtered$Region,selected_areas()$lad19nm)),]  
  return(latest_data_sorted)
})

#UX optimisation - only want graphs to show when more than 1 area selected
observeEvent(input$glasgow_region_choice,{
  #this first line is required because have set it that if user clicks an area on first page,
  #this automatically gets selected on the next page - which would otherwise trigger the event even before the user got to that page
  req(length(input$glasgow_region_choice)>1)
  shinyjs::hide(id="intro_page2_text")
  shinyjs::show(id="glasgow_areas_comparison") 
  }, ignoreInit = TRUE)

#filter the shapefile to only show the regions selected
selected_areas <- reactive ({
  glasgow_map_regions %>%
    subset(lad19nm %in% input$glasgow_region_choice)
})

#update select input for region based on first page
observe({
map_area <- input$regionmap_shape_click$id
#update the drop-down to have this area in on the second page
updateSelectizeInput(session,"glasgow_region_choice", label = NULL,
                     choices = glasgow_regions, selected = map_area,
                     options = list(maxOptions = 1300, 
                                    placeholder = "Select one or more local authorities of interest"))

})

#reactive element that will only show the comparators available for the indicator that has been selected (as not all comparator areas are available for all indicators)
comparators_available <- reactive({
  list <-c(unique(latest_data$Region[latest_data$Indicator == input$economic_indicator_choice & latest_data$Region %in% comparators]))
  return(list)
})

#update comparator list based on what's available for that indicator
observeEvent(input$economic_indicator_choice, {

  
updateSelectizeInput(session,"comparator_choice", label = NULL, choices=comparators_available(),
                options = list(maxOptions = 1300, 
                                                placeholder = "Select a comparator area to compare regions to"))

updateSelectizeInput(session,"uk_economic_indicator_choice", label = NULL, choices=all_indicators,
                     selected = input$economic_indicator_choice)

})


################################################################################################
#Visualisations
########### Map #####################
#map title
output$timeseries_title <- renderText({ paste0("Historical data for ",input$economic_indicator_choice) })
output$latest_data <- renderText({ paste0("Latest data available: ", unique(selected_indicator_data_year()$Year))})
output$indicator_title <- renderUI({ HTML(paste0("<h3>",input$economic_indicator_choice,"</h3>","<p>(Latest data: ",unique(selected_indicator_data_year()$Year),")</p>"))})

######################
#######################################################COME BACK
################################
###############################
# output$notes <- renderUI({
#   #for (i in length(combined_areas)){
#  # if(region_1[i] %in% input$glasgow_region_choice){
#   if("East Dunbartonshire" %in% input$glasgow_region_choice) {
#     text <- paste0("Please note East Dunbartonshire is part of the Scottish region combining East Dunbartonshire, for this indicator and the value presented for this local authority is representative for this region as a whole.")
#   }# else if () {
#     print(text)
#   #}
#     })
  
#rendering of the map
output$glasgow_map <- renderLeaflet({
glasgow_map_react()
})

#setting the colour breaks for the map
pal <- reactive({
  colorQuantile("Purples", selected_indicator_data_year()$Value)
})

# #tooltip for map
map_tooltip <- reactive({
#    htmltools::HTML(
      paste("<b>",htmlEscape(selected_areas()$lad19nm),"</b>","<br/>",htmlEscape(selected_indicator_data_year()$Value),"<br/>")#)
})

map_title <- reactive({
  paste0(input$economic_indicator_choice," (",unique(selected_indicator_data_year()$Year)," data)")
})

#map code
glasgow_map_react <- reactive({
 # tooltip <- paste("<b>",htmlEscape(lad19nm),"</b>","<br/>",htmlEscape(selected_indicator_data_year()$Value),"<br/>")#)
   leaflet(data=selected_areas(),options=leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("CartoDB.Positron") %>% #CartoDB.Positron, CartoDB.Voyager maybe addTiles() %>% #addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>% 
              addPolygons(
                #outline of polygons
                weight = 0.8, 
                color = "grey",
                smoothFactor = 0.5,
                opacity = 1,
                #polygon fill
                fillOpacity = 0.7,
                fillColor = ~pal()(selected_indicator_data_year()$Value),#~colorQuantile("PuOr", selected_indicator_data_year()$Value)(selected_indicator_data_year()$Value),
                highlightOptions = highlightOptions(color = "#E9BD43",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                layerId =~lad19nm,
                #label=~HTML(map_tooltip())) %>%
                label = ~paste(lad19nm, format(round(selected_indicator_data_year()$Value,2), big.mark = ","))) %>%
              addLegend('topright', pal = pal(), values = round(selected_indicator_data_year()$Value,2), title = unique(selected_indicator_data_year()$Measure),
                        #by default legend shows quantile % ranges instead of numeric values - change
                        labFormat = function(type,cuts,p) {
                                                    n = length(cuts)
                                                    cuts[n] = max(selected_indicator_data_year()$Value)
                                                    for (i in 2:(n-1)){cuts[i] = ""} 	
                                                    cuts[1] = min(selected_indicator_data_year()$Value)
                                                    p = paste0(cuts[-n], cuts[-1])}) %>%
             addControl(map_title(),position="topleft") #
})

########### Summary Table ###################
#table title
output$table_title <- renderText({ "Latest data" })
#table
output$glasgow_summary_table <- DT::renderDataTable({
 # selected_columns <- selected_indicator_data_year() %>% select(Region, Value, Year)
  DT::datatable(selected_indicator_data_year()[,c("Region","Value","Measure","Year")],
                style = 'bootstrap', rownames = FALSE, options = list(dom = 't', language = list(
                  zeroRecords = "Please make selection above to view data"),
                  columnDefs = list(list(className = 'text-right', targets = 2))), 
                  colnames = c("Region", "Value", "Measure", "Period")
                )
  
})

#############Donut chart for jobs by sector#####
###############################################
output$jobs_by_sector <- renderPlotly({
  plot_ly(data= selected_jobs_sector_latest() ,labels = ~Indicator, values = ~Value) %>%
  add_pie(hole = 0.6) %>% layout(title = "Donut charts using Plotly",  showlegend = F,
                        xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                        yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})

###########Time trend graph #######################
#trend title
output$glasgow_timetrend_title <- renderText({ paste0("Historical data for ",input$economic_indicator_choice) })
#set tooltip label
tooltip_trend <- reactive({
  paste0(selected_indicator_data()$Region,"<br>",format(round(selected_indicator_data()$Value,2),big.mark=","),"<br>",selected_indicator_data()$Year)
})
#trend graph
output$time_trend_glasgow <- renderPlotly({
  plot_ly(data=selected_indicator_data(), x=~Year,  y = ~Value,
          color = ~Region,
          colors = "PuOr",
          text=tooltip_trend(), 
          hoverinfo="text"#, 
         ) %>% 
    add_trace(type = 'scatter', mode = 'lines+markers', marker = list(size = 8)#,
             # symbol = ~Region#, symbols = symbols_trend
                            ) %>%
    layout(
      #title = paste0("Time trend for ",input$economic_indicator_choice),
      xaxis = list(title = "", color='white',tickcolor='white'),
      yaxis = list(title = ~unique(Measure), color='white',tickcolor='white'),
      legend = list(font = list(color ='white')),
      plot_bgcolor='rgba(0, 0, 0, 0)',
      paper_bgcolor='rgba(0, 0, 0, 0.2)',
      #fig_bgcolor='rgba(0, 0, 0, 0)',#selected_indicator_data()$Measure)
      showlegend = TRUE
    ) %>%
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
})

region_order <- reactive({
  as.array(selected_indicator_data()$Region)
})
############## Bar Graph #########
#bar graph title
output$glasgow_bar_title <- renderText({ 
 unique(paste0("Local authorities compared against ",input$comparator_choice, " (",selected_indicator_data_year()$Year," data)"))
 })

output$glasgow_bar_subtitle <- renderText({
  unique(paste0("Horizontal line represents ", input$comparator_choice, " with a value of: ", format(round(selected_indicator_data_year()$Comp_Value,2),big.mark=",")))
})

#bar graph
glasgow_bar_chart <- reactive({
  plot_ly(data = selected_indicator_data_year(), hoverinfo="none")%>%  #,text=tooltip_bar, hoverinfo="text",
                  #for comaparator
  add_trace(x = ~Region, y = ~Comp_Value, name= ~unique(Comparator), type = 'scatter', mode = 'lines',
           line = list(color = '#7d3778'), showlegend = FALSE, hoverinfo="skip") %>%
  add_bars(x = ~reorder(Region,-Value), y = ~Value, marker = list(color='#E9BD43'), text = ~paste(Region, "<br>", format(round(Value,2),big.mark = ",")), hoverinfo="text") %>% 
   layout(annotations = list(),
     xaxis = list(title="", color='white',tickcolor='white',categoryorder="array",categoryarray = region_order(),showgrid=FALSE),
         yaxis = list(title=~unique(Measure), color='white',tickcolor='white',showgrid=FALSE),
         plot_bgcolor='rgba(0, 0, 0, 0)',
         paper_bgcolor='rgba(0, 0, 0, 0.2)') %>% #,
         #fig_bgcolor='rgba(0, 0, 0, 0)') 
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
                          })

output$rank_plot <- renderPlotly({
  glasgow_bar_chart()
})

#########Downloading the rest of the data as a report

output$LA_comparison_download <- downloadHandler(
  filename = paste0("Glasgow_LA_comparison_",Sys.Date(),".pdf"),
  content = function(file){
    tempReport <- file.path(tempdir(), "glasgow_comparison.Rmd")
    file.copy("glasgow_comparison.Rmd",tempReport, overwrite = TRUE)
    #parameters to pass to Rmd document
    params <- list(Indicator = input$economic_indicator_choice, Bar_Data = selected_indicator_data_year(), Comparator = input$comparator_choice, Time_Data = selected_indicator_data())#, Areas = input$glasgow_region_choice, Comparator = input$comparator_choice)#, Data = selected_indicator_data_year())#, Image = place)
    #knitting the document
    rmarkdown:: render(tempReport, 'pdf_document', output_file = file,
                       params = params,
                       envir =  new.env(parent = globalenv())
    )
  }
)

#downloading the data as csv
output$glasgow_data_download <- downloadHandler(
  filename = paste0("glasgow_economic_data_extract_",Sys.Date(),".csv"),
    content = function(file){
   write.csv(selected_indicator_data_year()[,c("Indicator","Region","Value","Measure","Year")], file, row.names=FALSE) }
)

output$glasgow_historical_data_download <- downloadHandler(
  filename = paste0("glasgow_economic_historical_data_extract_",Sys.Date(),".csv"),
  content = function(file){
    write.csv(selected_indicator_data()[,c("Indicator","Region","Value","Measure","Year")], file, row.names=FALSE) }
)

#downloading map as a png separately
output$download_glasgow_map <- downloadHandler(
  filename = paste0("Glasgow_Map_",Sys.Date(),".png"),
  content = function(file) {
    mapshot(glasgow_map_react(),file = file, clicprect = "viewport") 
  })

##############################################
#UK City Regions comparison page
##############################################
#####Creating reactive datasets###############

#subset data to indicator selected
uk_selected_indicator_data <- reactive ({
  #jobs by sector is an indicator for the uk city regions, so adding this extra layer to the dataset filter
  if (input$uk_economic_indicator_choice=="Jobs by Sector"){
    indicators_data %>%
      subset(Indicator == input$uk_economic_indicator_choice &
               Job_sector == input$uk_jobs_choice &
               Measure == "Count" & # need to modify somewhere
               Region %in% input$uk_region_choice)             
  } else {
    indicators_data %>%
      subset(Indicator == input$uk_economic_indicator_choice &
               Region %in% input$uk_region_choice)           
  }
})

# uk_latest_indicator_data <- reactive({
#   latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice &
#                            Region %in% input$uk_region_choice)
# })

#eventReactive instead of reactive so that it changes based on input - otherwise get error messages when you change indicator and comparator doesn't exist
uk_comparator_data <- eventReactive(input$uk_comparator_choice,{  
  if (input$uk_economic_indicator_choice=="Jobs by Sector"){
  latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice & Region %in% input$uk_comparator_choice & Job_sector %in% input$uk_jobs_choice & Measure == ifelse(input$job_measure_picker == "perc","Percentage (%)","Count"))#job_sector_measure())
 # print(input$jobs_measure_picker)
    } else {
  latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice & Region %in% input$uk_comparator_choice) #%>% rename(Comparator = Region, Comp_Value = Value)
  }
 
})

#####################################################################
#########################revisit
######################################################################
#####################################################################
#####################################################################

job_sector_measure <- observeEvent(input$job_measure_picker,{
  if (input$job_measure_picker =="count") { "Count"}
  else if (input$job_measure_picker =="perc") {"Percentage (%)"}
  })

uk_selected_indicator_data_year <- reactive ({
  if (input$uk_economic_indicator_choice=="Jobs by Sector"){
  latest_data_filtered <- latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice &
                                                   Job_sector %in% input$uk_jobs_choice &
                                                   Measure == job_sector_measure() & #ifelse(input$job_measure_picker == "perc","Percentage (%)","Count") &
                                                   Region %in% input$uk_region_choice) #%>% mutate(Comparator = uk_comparator_data()$Region, Comp_Value = uk_comparator_data()$Value)
  latest_data_filtered$Comparator <- rep(uk_comparator_data()$Region,nrow(latest_data_filtered))
  latest_data_filtered$Comp_Value <- rep(uk_comparator_data()$Value,nrow(latest_data_filtered))
  } else {
  latest_data_filtered <- latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice &
                                                     Region %in% input$uk_region_choice) #%>% mutate(Comparator = uk_comparator_data()$Region, Comp_Value = uk_comparator_data()$Value)  
  latest_data_filtered$Comparator <- rep(uk_comparator_data()$Region,nrow(latest_data_filtered))
  latest_data_filtered$Comp_Value <- rep(uk_comparator_data()$Value,nrow(latest_data_filtered))
  }
  return(latest_data_filtered)
})


observeEvent(input$uk_region_choice,{
  shinyjs::hide(id="intro_page3_text")
  shinyjs::show(id="uk_areas_comparison")
}, ignoreInit = TRUE)

#need uk shapefile for map
uk_selected_areas <- reactive ({
  uk_map_regions %>%
    subset(cauth19nm %in% input$uk_region_choice)
})

#for jobs will need to subset twice for % of jobs by sector
# uk_selected_jobs_sector_latest <- reactive ({
#   
#   latest_data %>% subset(Indicator %in% input$uk_jobs_choice &
#                            Region %in% input$uk_region_choice)
#   
# })

uk_comparators_available <- reactive({
  list <-c(unique(latest_data$Region[latest_data$Indicator == input$uk_economic_indicator_choice & latest_data$Region %in% comparators]))
  return(list)
})

#update comparator list based on what's available for that indicator
#observe({
observeEvent(input$uk_economic_indicator_choice,{
  #updating comparator drop-down
  updateSelectizeInput(session,"uk_comparator_choice", label = NULL, choices=uk_comparators_available(),
                       options = list(maxOptions = 1300, 
                                      placeholder = "Select a comparator area to compare regions to"))

  #make sure when clicking out of "jobs by sector", the second input field (that specified the sector) is cleared
  if (input$uk_economic_indicator_choice!="Jobs by Sector") {
    updateSelectizeInput(session, "jobs_choice", label = "Show for sector", choices = job_sectors, 
                         selected = NULL)
    
    #Jobs by Sector is not available at smaller geographies so only updating if this isn't what has been selected
    updateSelectizeInput(session,"economic_indicator_choice", label = NULL, choices=indicators_cleaned,
                         selected = input$uk_economic_indicator_choice)
  }
  print(input$job_measure_picker)

})
###############################################
#Visualisations
########### Map #####################
#map title
output$uk_map_title <- renderText({ unique(paste0("Latest data available: ", latest_data$Year[latest_data$Indicator == input$uk_economic_indicator_choice])) })
output$uk_timeseries_title <- renderText({ paste0("Historical data for ",input$uk_economic_indicator_choice) })
output$uk_latest_data <- renderText({ paste0("Latest data available: ", unique(uk_selected_indicator_data_year()$Year))})
output$uk_indicator_title <- renderUI({ HTML(paste0("<h3>",input$uk_economic_indicator_choice,"</h3>","<p>(Latest data: ",unique(uk_selected_indicator_data_year()$Year),")</p>"))})

#create colorQuantiles for chloropleth map
pal_uk <- reactive({
  colorQuantile("Purples", uk_selected_indicator_data_year()$Value)
})

#create tooltip for map

#map title
uk_map_title <- reactive({
  paste0(input$uk_economic_indicator_choice," (",unique(uk_selected_indicator_data_year()$Year)," data)")
})

#code for the UK regions map
uk_map_react <- reactive({
  leaflet(data=uk_selected_areas(),options=leafletOptions(zoomControl = FALSE)) %>% addProviderTiles("CartoDB.Positron") %>% 
    addPolygons(#outline of polygons
                weight = 0.8,
                color = "grey",
                smoothFactor = 0.5,
                opacity = 1,
                #polygon fill
                fillOpacity = 0.7,
                fillColor = ~pal_uk()(uk_selected_indicator_data_year()$Value),#~colorQuantile("PuOr", uk_selected_indicator_data_year()$Value)(uk_selected_indicator_data_year()$Value),
                highlightOptions = highlightOptions(color = "#E9BD43",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                layerId =~cauth19nm,
                # label=~lad19nm) 
                label=~paste(
                  cauth19nm," : ",
                  format(round(uk_selected_indicator_data_year()$Value,2), big.mark = ","))) %>%
    addLegend('topright', pal = pal_uk(), values = round(uk_selected_indicator_data_year()$Value,2), title = unique(uk_selected_indicator_data_year()$Measure),
              #by default legend shows quantile % ranges instead of numeric values - change
              labFormat = function(type,cuts,p) {
                n = length(cuts)
                cuts[n] = max(uk_selected_indicator_data_year()$Value)
                for (i in 2:(n-1)){cuts[i] = ""} 	
                cuts[1] = min(uk_selected_indicator_data_year()$Value)
                p = paste0(cuts[-n], cuts[-1])}) %>%
    addControl(uk_map_title(),position="topleft")
  
})


#render UK regiosn map
output$uk_map <- renderLeaflet({
  uk_map_react()
})

########### Summary Table ###################
#table title
output$uk_table_title <- renderText({ "Latest data" })

#table
output$uk_summary_table <- DT::renderDataTable({
  # selected_columns <- selected_indicator_data_year() %>% select(Region, Value, Year)
  DT::datatable(uk_selected_indicator_data_year()[,c("Region","Value", "Measure","Year")],
                style = 'bootstrap', rownames = FALSE, options = list(dom = 't', language = list(
                  zeroRecords = "Please make selection above to view data"),
                  columnDefs = list(list(className = 'text-right', targets = 2))), 
                colnames = c("Region", "Value","Measure", "Period")
  )
  
})

###########Time trend graph #######################
#trend title
output$uk_timetrend_title <- renderText({ paste0("Historical data for ",input$uk_economic_indicator_choice) })
#set tooltip label
uk_tooltip_trend <- reactive({
  paste0(uk_selected_indicator_data()$Region,"<br>",format(round(uk_selected_indicator_data()$Value,2), big.mark = ","),"<br>",uk_selected_indicator_data()$Year)
  #HTML(paste0("<b>",uk_selected_indicator_data()$Region,"</b><br>",format(round(uk_selected_indicator_data()$Value), big.mark = ","),"<br>",uk_selected_indicator_data()$Year))
  })
#trend graph
output$time_trend_uk <- renderPlotly({
  #req(nrow(uk_selected_indicator_data()) > 2)
  plot_ly(data=uk_selected_indicator_data(), x=~Year,  y = ~Value,
          color = ~Region,
          colors = "PuOr",
          # colors=colorRampPalette(brewer.pal(("#7d3780","#E9BD43"),(length(~Region)))),
          text=uk_tooltip_trend(), 
          hoverinfo="text"#, 
          # height = 600 
  ) %>% 
    add_trace(type = 'scatter', mode = 'lines+markers', marker = list(size = 8)#,
              # symbol = ~Region#, symbols = symbols_trend
    ) %>%
    layout(
      #title = paste0("Time trend for ",input$economic_indicator_choice),
      xaxis = list(title = "", color='white',tickcolor='white', dtick = 1),
      yaxis = list(title = ~unique(Measure), color='white',tickcolor='white'),
      legend = list(font = list(color ='white')),
      plot_bgcolor='rgba(0, 0, 0, 0)',
      paper_bgcolor='rgba(0, 0, 0, 0.2)',
      #fig_bgcolor='rgba(0, 0, 0, 0)',#selected_indicator_data()$Measure)
      showlegend = TRUE
    ) %>%
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
})

region_order <- reactive({
  as.array(uk_selected_indicator_data()$Region)
})
############## Bar Graph #########
#bar graph title
output$uk_bar_title <- renderText({ 
  #req(nrow(selected_indicator_data_year()>2)) 
  unique(paste0("City regions compared against ",input$uk_comparator_choice, " (",uk_selected_indicator_data_year()$Year," data)")) })

output$uk_bar_subtitle <- renderText({
  unique(paste0("Horizontal line represents ", input$uk_comparator_choice, " with a value of: ", format(round(uk_selected_indicator_data_year()$Comp_Value,2),big.mark=",")))
})

#bar graph
output$uk_rank_plot <- renderPlotly({
  #req(nrow(uk_selected_indicator_data_year()>0))
  plot_ly(data = uk_selected_indicator_data_year(), hoverinfo="none")%>%  #,text=tooltip_bar, hoverinfo="text",
    #for comaparator
    add_trace(x = ~Region, y = ~Comp_Value, name= ~unique(Comparator), type = 'scatter', mode = 'lines',
              line = list(color = '#7d3778'), showlegend = FALSE) %>%
    add_bars(x = ~reorder(Region,-Value), y = ~Value, marker = list(color='#E9BD43'), text = ~paste(Region, "<br>", format(round(Value,2),big.mark = ",")), hoverinfo="text") %>% 
    layout(annotations = list(),
           xaxis = list(title="", color='white',tickcolor='white',categoryorder="array",categoryarray = region_order(),showgrid=FALSE),
           yaxis = list(title=~unique(Measure), color='white',tickcolor='white',showgrid=FALSE),
           plot_bgcolor='rgba(0, 0, 0, 0)',
           paper_bgcolor='rgba(0, 0, 0, 0.2)') %>% #,
    #fig_bgcolor='rgba(0, 0, 0, 0)') 
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
})

#########Downloading the rest of the data as a report

output$uk_comparison_download <- downloadHandler(
  filename = paste0("UK_CityRegions_comparison_",Sys.Date(),".pdf"),
  content = function(file){
    tempReport <- file.path(tempdir(), "uk_comparison.Rmd")
    file.copy("uk_comparison.Rmd",tempReport, overwrite = TRUE)
    #parameters to pass to Rmd document
    params <- list(Indicator = input$uk_economic_indicator_choice, Bar_Data = uk_selected_indicator_data_year(), Comparator = input$uk_comparator_choice, Time_Data = uk_selected_indicator_data())#, Areas = input$glasgow_region_choice, Comparator = input$comparator_choice)#, Data = selected_indicator_data_year())#, Image = place)
    #knitting the document
    rmarkdown:: render(tempReport, 'pdf_document', output_file = file,
                       params = params,
                       envir =  new.env(parent = globalenv())
    )
  }
)

#downloading the data as csv
output$uk_data_download <- downloadHandler(
  filename = paste0("uk_economic_data_extract_",Sys.Date(),".csv"),
  content = function(file){
    write.csv(uk_selected_indicator_data_year()[,c("Indicator","Region","Value","Measure","Year")], file, row.names=FALSE) }
)

output$uk_historical_data_download <- downloadHandler(
  filename = paste0("uk_economic_historical_data_extract_",Sys.Date(),".csv"),
  content = function(file){
    write.csv(uk_selected_indicator_data()[,c("Indicator","Region","Value","Measure","Year")], file, row.names=FALSE) }
)

#downloading map as a png separately
output$download_uk_map <- downloadHandler(
  filename = paste0("UK_CityRegions_map_",Sys.Date(),".png"),
  content = function(file) {
    mapshot(uk_map_react(),file = file, clicprect = "viewport") 
  })

} # server closing bracket