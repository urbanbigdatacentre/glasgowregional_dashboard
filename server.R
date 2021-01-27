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
  })

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
#output$serviceexports_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Service Exports Per Job (£)" & indicators_change$change == "1 year change"],2)
  #three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionmap_shape_click$id & indicators_change$Indicator == "Service Exports Per Job (£)" & indicators_change$change == "3 year change"],2)
 # HTML(paste0("<p>",one_change,"% 1-year change","</p>","<br>","<p>",three_change,"% 3-year change","</p>"))
#})

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

#downloading of the data - first go # works, but doesn't download in the right format
# output$single_area_summary_md <- downloadHandler(
#   filename = paste0(input$regionmap_shape_click$id,"_area_summary.docx"),
#   content = function(file){
#     tempReport <- file.path(tempdir(), "region_summary_download.Rmd")
#     file.copy("region_summary_download.Rmd",tempReport, overwrite = TRUE)
#     #parameters to pass to Rmd document
#     params <- list(Region = input$regionmap_shape_click$id)
#     #knitting the document
#     rmarkdown:: render(tempReport, 'word_document', output_file = file,
#                        params = params,
#                        envir =  new.env(parent = globalenv())
#                        )
#   }
# )

#downloading data by creating report first
download_file <- reactive({
    tempReport <- file.path(tempdir(), "region_summary_download.Rmd")
    file.copy("region_summary_download.Rmd",tempReport, overwrite = TRUE)
    #parameters to pass to Rmd document
    params <- list(Region = input$regionmap_shape_click$id)
    #knitting the document
    rmarkdown:: render(tempReport, 'word_document', output_file = tempReport,
                       params = params,
                       envir =  new.env(parent = globalenv()))
                    
    report$filepath <- tempReport        
})

#now instructions for download button
output$single_area_summary_md <- downloadHandler(
  filename = function() {
    paste0(input$client,"_",Sys.Date(),".docx") %>%
      gsub(" ","_",.)
      },
  content = function(file){
    file.copy(report$filepath, file)
  }
)
##############################################
#Glasgow City Regions comparison page
##############################################
######Definition
indicator <- reactive({
  input$economic_indicator_choice
})

definition <- reactive({
  unique(indicators_data$Definition[indicators_data$Indicator == input$economic_indicator_choice])
})

observeEvent(input$definition, {
  showModal(modalDialog(
    title = indicator(),
    p(definition()),size = "l",
    easyClose = TRUE, fade=FALSE,footer = modalButton("Close (Esc)")
  ))
})
#####Creating reactive datasets###############
#subset data to indicator selected
selected_indicator_data <- reactive ({
  #trend <- 
  indicators_data %>%
    subset(Indicator == input$economic_indicator_choice &
             Region %in% input$glasgow_region_choice) #|
  # Region == input$comparator_choice)
})

latest_indicator_data <- reactive({
  latest_data %>% subset(Indicator %in% input$economic_indicator_choice &
                           Region %in% input$glasgow_region_choice)
})

comparator_data <- reactive({
  latest_data %>% subset(Indicator %in% input$economic_indicator_choice & Region %in% input$comparator_choice) #%>% rename(Comparator = Region, Comp_Value = Value)
})

selected_indicator_data_year <- reactive ({
  latest_data_filtered <- latest_data %>% subset(Indicator %in% input$economic_indicator_choice &
                           Region %in% input$glasgow_region_choice) %>% mutate(Comparator = comparator_data()$Region, Comp_Value = comparator_data()$Value) 
  latest_data_sorted <- latest_data_filtered[order(match(latest_data_filtered$Region,selected_areas()$lad19nm)),]  
  return(latest_data_sorted)
})

observeEvent(input$glasgow_region_choice,{
  shinyjs::hide(id="intro_page2_text")
  shinyjs::show(id="glasgow_areas_comparison")
})

selected_areas <- reactive ({
  glasgow_map_regions %>%
    subset(lad19nm %in% input$glasgow_region_choice)
})

#for jobs will need to subset twice for % of jobs by sector
selected_jobs_sector_latest <- reactive ({

  latest_data %>% subset(Indicator %in% input$jobs_choice &
             Region %in% input$glasgow_region_choice)

  })

#update select input for region based on first page
observe({
map_area <- input$regionmap_shape_click$id

#if(!is.null(map_area))

updateSelectizeInput(session,"glasgow_region_choice", label = NULL,
                     choices = glasgow_regions, selected = map_area,
                     options = list(maxOptions = 1300, 
                                    placeholder = "Select one or more regions of interest"))

})

comparators_available <- reactive({
  list <-c(unique(latest_data$Region[latest_data$Indicator == input$economic_indicator_choice & latest_data$Region %in% comparators]))
  return(list)
})

#update comparator list based on what's available for that indicator
observeEvent(input$economic_indicator_choice, {

  
updateSelectizeInput(session,"comparator_choice", label = NULL, choices=comparators_available(),
                options = list(maxOptions = 1300, 
                                                placeholder = "Select a comparator area to compare regions to"))


#updateSelectizeInput("uk_economic_indicator_choice", label = NULL, choices=indicators_cleaned,
 #                selected = input$economic_indicator_choice)    
})
################################################################################################
#Visualisations
########### Map #####################
#map title
output$glasgow_map_title <- renderText({ unique(paste0("Latest data available: ", latest_data$Year[latest_data$Indicator == input$economic_indicator_choice])) })

output$glasgow_map <- renderLeaflet({
   req(nrow(selected_areas()) > 1)
   leaflet(data=selected_areas(),options=leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% #addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>% 
              addPolygons(stroke = FALSE, weight = 1,
                smoothFactor = 0.5,
                fillOpacity = 0.5,
                color = ~colorQuantile("PuOr", selected_indicator_data_year()$Value)(selected_indicator_data_year()$Value),
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                layerId =~lad19nm,
               # label=~lad19nm) 
               label=~paste(
                 lad19nm," : ",
                 selected_indicator_data_year()$Value))
})
   
########### Summary Table ###################
#table title

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
  paste0(selected_indicator_data()$Region,"<br>",selected_indicator_data()$Value,"<br>",selected_indicator_data()$Year)
})
#trend graph
output$time_trend_glasgow <- renderPlotly({
  req(nrow(selected_indicator_data()) > 0)
  plot_ly(data=selected_indicator_data(), x=~Year,  y = ~round(Value,2),
          color = ~Region,
          colors = "PuOr",
         # colors=colorRampPalette(brewer.pal(("#7d3780","#E9BD43"),(length(~Region)))),
          text=tooltip_trend(), 
          hoverinfo="text"#, 
        # height = 600 
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
  #req(nrow(selected_indicator_data_year()>0))
  unique(paste0("Horizontal line represents ", input$comparator_choice, " as comparator with a value of: ", format(round(selected_indicator_data_year()$Comp_Value,2),big.mark=","))) })
#tooltip text

#bar graph
output$rank_plot <- renderPlotly({
  req(nrow(selected_indicator_data_year()>0))
  plot_ly(data = selected_indicator_data_year())%>%  #,text=tooltip_bar, hoverinfo="text",
                  #for comaparator
  add_trace(x = ~Region, y = ~Comp_Value, name= ~unique(Comparator), type = 'scatter', mode = 'lines',
           line = list(color = '#7d3778'), showlegend = FALSE, hoverinfo="skip") %>%
  add_bars(x = ~reorder(Region,-Value), y = ~Value, marker = list(color='#E9BD43')) %>% 
   layout(annotations = list(),
     xaxis = list(title="", color='white',tickcolor='white',categoryorder="array",categoryarray = region_order(),showgrid=FALSE),
         yaxis = list(title=~unique(Measure), color='white',tickcolor='white',showgrid=FALSE),
         plot_bgcolor='rgba(0, 0, 0, 0)',
         paper_bgcolor='rgba(0, 0, 0, 0.2)') %>% #,
         #fig_bgcolor='rgba(0, 0, 0, 0)') 
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
                          })

##############################################
#UK City Regions comparison page
##############################################
#####Creating reactive datasets###############
#subset data to indicator selected
uk_selected_indicator_data <- reactive ({
  #trend <- 
  indicators_data %>%
    subset(Indicator == input$uk_economic_indicator_choice &
             Region %in% input$uk_region_choice) #|
  # Region == input$comparator_choice)
})

uk_latest_indicator_data <- reactive({
  latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice &
                           Region %in% input$uk_region_choice)
})

uk_comparator_data <- reactive({
  latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice & Region %in% input$uk_comparator_choice) #%>% rename(Comparator = Region, Comp_Value = Value)
})

uk_selected_indicator_data_year <- reactive ({
  latest_data_filtered <- latest_data %>% subset(Indicator %in% input$uk_economic_indicator_choice &
                                                   Region %in% input$uk_region_choice) %>% mutate(Comparator = uk_comparator_data()$Region, Comp_Value = uk_comparator_data()$Value) 
  latest_data_sorted <- latest_data_filtered#[order(match(latest_data_filtered$Region,selected_areas()$lad19nm)),]  
  return(latest_data_sorted)
})


observeEvent(input$uk_region_choice,{
  shinyjs::hide(id="intro_page3_text")
  shinyjs::show(id="uk_areas_comparison")
})

#need uk shapefile for map
uk_selected_areas <- reactive ({
  uk_map_regions %>%
    subset(cauth19nm %in% input$uk_region_choice)
})

#for jobs will need to subset twice for % of jobs by sector
uk_selected_jobs_sector_latest <- reactive ({
  
  latest_data %>% subset(Indicator %in% input$uk_jobs_choice &
                           Region %in% input$uk_region_choice)
  
})

uk_comparators_available <- reactive({
  list <-c(unique(latest_data$Region[latest_data$Indicator == input$uk_economic_indicator_choice & latest_data$Region %in% comparators]))
  return(list)
})

#update comparator list based on what's available for that indicator
#observe({
observeEvent(input$uk_economic_indicator_choice,{

  updateSelectizeInput(session,"uk_comparator_choice", label = NULL, choices=uk_comparators_available(),
                       options = list(maxOptions = 1300, 
                                      placeholder = "Select a comparator area to compare regions to"))
})
###############################################
#Visualisations
########### Map #####################
#map title
output$uk_map_title <- renderText({ unique(paste0("Latest data available: ", latest_data$Year[latest_data$Indicator == input$uk_economic_indicator_choice])) })

#map code
output$uk_map <- renderLeaflet({
  req(nrow(selected_areas()) > 1)
  leaflet(data=uk_selected_areas(),options=leafletOptions(zoomControl = FALSE)) %>% addTiles() %>% #addProviderTiles(providers$Stadia.AlidadeSmoothDark) %>% 
    addPolygons(stroke = FALSE, weight = 1,
                smoothFactor = 0.5,
                fillOpacity = 0.5,
                color = ~colorQuantile("PuOr", uk_selected_indicator_data_year()$Value)(uk_selected_indicator_data_year()$Value),
                highlightOptions = highlightOptions(color = "white",
                                                    weight = 2,
                                                    bringToFront = TRUE),
                layerId =~cauth19nm,
                # label=~lad19nm) 
                label=~paste(
                  cauth19nm," : ",
                  selected_indicator_data_year()$Value))
})
########### Summary Table ###################
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

#############Donut chart for jobs by sector#####
###############################################
output$uk_jobs_by_sector <- renderPlotly({
  plot_ly(data= uk_selected_jobs_sector_latest() ,labels = ~Indicator, values = ~Value) %>%
    add_pie(hole = 0.6) %>% layout(title = "Donut charts using Plotly",  showlegend = F,
                                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
})

###########Time trend graph #######################
#trend title
output$uk_timetrend_title <- renderText({ paste0("Historical data for ",input$uk_economic_indicator_choice) })
#set tooltip label
uk_tooltip_trend <- reactive({
  paste0(uk_selected_indicator_data()$Region,"<br>",uk_selected_indicator_data()$Value,"<br>",uk_selected_indicator_data()$Year)
})
#trend graph
output$time_trend_uk <- renderPlotly({
  req(nrow(uk_selected_indicator_data()) > 0)
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
  as.array(uk_selected_indicator_data()$Region)
})
############## Bar Graph #########
#bar graph title
output$uk_bar_title <- renderText({ 
  #req(nrow(selected_indicator_data_year()>0))
  unique(paste0("Horizontal line represents ", input$uk_comparator_choice, " as comparator with value of ", format(round(uk_selected_indicator_data_year()$Comp_Value,2),big.mark=","))) })
#tooltip text

#bar graph
output$uk_rank_plot <- renderPlotly({
  req(nrow(uk_selected_indicator_data_year()>0))
  plot_ly(data = uk_selected_indicator_data_year())%>%  #,text=tooltip_bar, hoverinfo="text",
    #for comaparator
    add_trace(x = ~Region, y = ~Comp_Value, name= ~unique(Comparator), type = 'scatter', mode = 'lines',
              line = list(color = '#7d3778'), showlegend = FALSE, hoverinfo="skip") %>%
    add_bars(x = ~reorder(Region,-Value), y = ~Value, marker = list(color='#E9BD43')) %>% 
    layout(annotations = list(),
           xaxis = list(title="", color='white',tickcolor='white',categoryorder="array",categoryarray = region_order(),showgrid=FALSE),
           yaxis = list(title=~unique(Measure), color='white',tickcolor='white',showgrid=FALSE),
           plot_bgcolor='rgba(0, 0, 0, 0)',
           paper_bgcolor='rgba(0, 0, 0, 0.2)') %>% #,
    #fig_bgcolor='rgba(0, 0, 0, 0)') 
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
})

} # server closing bracket