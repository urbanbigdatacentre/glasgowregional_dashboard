server <- function(input, output) {
  
##############################
#Landing page
##############################
output$regionsmap <- renderLeaflet({
  leaflet(data=glasgow_map_regions, options=leafletOptions(zoomControl = FALSE)) %>% addPolygons(color = "#444444",
                                                                     weight = 1,
                                                                     smoothFactor = 0.5,
                                                                     opacity = 1.0,
                                                                     fillOpacity = 0.5,
                                                                     highlightOptions = highlightOptions(color = "white",
                                                                                                         weight = 2,
                                                                                                         bringToFront = TRUE),
                                                                     layerId =~lad19nm,
                                                                     #options = pathOptions(pane= "highlight", clickable=TRUE), 
                                                                     label=~lad19nm)
})

#this records what area has been selected internally
observeEvent(input$regionsmap_shape_click, { 
  p <- input$regionsmap_shape_click
  print(p$id)
  shinyjs::hide(id="intro_text")
  shinyjs::show(id="area_detail")
  })

#this is to print out area on UI
output$region_name <- renderText({input$regionsmap_shape_click$id})

#printing out summary statistics for the area selected in map - i.e. the latest data for each indicator
#rounding decimals to 2 decimal places
#rounding % to 3 significant figures
output$region_population <- renderText({format(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Total Population"],big.mark=",")})
output$region_jobs <- renderText({format(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Total Jobs"], big.mark=",")})
output$region_GVAworked <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "GVA per hour worked (£)"],2)})
output$region_enterprises <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"],2)})
output$region_serviceexports <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Service Exports Per Job (£)"],2)})
output$region_medianincome <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Residence-based Full-time Median Weekly Earnings"],2)})
output$region_noqualification <- renderText({signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Working-age population with no qualifications (%)"],3)})
output$region_unemployed <- renderText({signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Unemployment Rate 16-64 (%)"],3)})
output$region_ecoinactive <- renderText({signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Economic Inactivity Rate 16-64 (%)"],3)})
output$region_claimcount <- renderText({signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"],3)})
output$region_childpoverty <- renderText({signif(100*(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "% of children not in poverty (after housing costs) "]),3)})
output$region_emissions <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Emissions per capita (tonnes)"],2)})

#Hover to show extra info about data source, year of data being shown, and % changes over specific time periods
#Total Population
output$pop_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Total Population"]
  a(href=link,"Data Source") })
output$pop_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Total Population"]
  p(recent_year) })

#Total jobs
output$jobs_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Total Jobs"]
  a(href=link,
    "Data Source") })
output$jobs_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Total Jobs"]
  p(recent_year) })

#GVA worked
output$GVAworked_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "GVA per hour worked (£)"]
  a(href=link,
    "Data Source") })
output$GVAworked_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "GVA per hour worked (£)"]
  p(recent_year) })

#Enterprises
output$enterprises_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  a(href=link,
    "Data Source") })
output$enterprises_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  p(recent_year) })

#Service exports
output$serviceexports_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Service Exports Per Job (£)"]
  a(href=link,
    "Data Source") })
output$serviceexports_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  p(recent_year) })

#Median Income
output$medianincome_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Residence-based Full-time Median Weekly Earnings"]
  a(href=link,
    "Data Source") })
output$medianincome_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Residence-based Full-time Median Weekly Earnings"]
  p(recent_year) })

#Working age population with no qualification
output$noqualification_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Working-age population with no qualifications (%)"]
  a(href=link,
    "Data Source") })
output$noqualification_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Working-age population with no qualifications (%)"]
  p(recent_year) })

#Unemployed
output$unemployed_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Unemployment Rate 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$unemployed_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Unemployment Rate 16-64 (%)"]
  p(recent_year) })

#Economic Inactivity
output$ecoinactive_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Economic Inactivity Rate 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$ecoinactive_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Economic Inactivity Rate 16-64 (%)"]
  p(recent_year) })

#Claimant count
output$claimcount_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$claimcount_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"]
  p(recent_year) })

#Child poverty
output$childpoverty_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "% of children not in poverty (after housing costs)"]
  a(href=link,
    "Data Source") })
output$childpoverty_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "% of children not in poverty (after housing costs) "]
  p(recent_year) })

#Emissions
output$emissions_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Emissions per capita (tonnes)"]
  a(href=link,
    "Data Source") })
output$emissions_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Emissions per capita (tonnes)"]
  p(recent_year) })


#downloading of the data
output$single_area_summary_md <- downloadHandler(
  filename = paste0(input$regionsmap_shape_click$id,"_area_summary.docx"),
  content = function(file){
    tempReport <- file.path(tempdir(), "region_summary_download.Rmd")
    file.copy("region_summary_download.Rmd",tempReport, overwrite = TRUE)
    #parameters to pass to Rmd document
    params <- list(Region = input$regionsmap_shape_click$id)
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
#output$data_source <- renderText({ifelse(source_data$Source[source_data$Indicator == input$economic_indicator_choice] != "",source_data$Source[source_data$Indicator == input$economic_indicator_choice],"")})



} # server closing bracket