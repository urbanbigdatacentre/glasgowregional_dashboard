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
output$region_GVAworked <- renderText({paste0("£",round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "GVA per hour worked (£)"],2))})
output$region_enterprises <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"],2)})
output$region_serviceexports <- renderText({paste0("£",round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Service Exports Per Job (£)"],2))})
output$region_medianincome <- renderText({round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Residence-based Full-time Median Weekly Earnings"],2)})
output$region_noqualification <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Working-age population with no qualifications (%)"],3),"%")})
output$region_unemployed <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Unemployment Rate 16-64 (%)"],3),"%")})
output$region_ecoinactive <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Economic Inactivity Rate 16-64 (%)"],3),"%")})
output$region_claimcount <- renderText({paste0(signif(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"],3),"%")})
output$region_childpoverty <- renderText({paste0(signif((latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "% of children in poverty (after housing costs)"]),3),"%")})
output$region_emissions <- renderText({paste0(round(latest_data$Value[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Emissions per capita (tonnes)"],2)," tonnes")})

#Hover to show extra info about data source, year of data being shown, and % changes over specific time periods
#Total Population
output$pop_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Total Population"]
  a(href=link,"Data Source") })
output$pop_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Total Population"]
  p(recent_year) })
output$pop_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "3 year change"],2)
  five_change <-round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "5 year change"],2)
  ten_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Population" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>","<br>","<p>",ten_change,"% 10-year change","</p>"))
})

#Total jobs
output$jobs_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Total Jobs"]
  a(href=link,
    "Data Source") })
output$jobs_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Total Jobs"]
  p(recent_year) })
output$jobs_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Jobs" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Jobs" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Total Jobs" & indicators_change$change == "5 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>"))
})

#GVA worked
output$GVAworked_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "GVA per hour worked (£)"]
  a(href=link,
    "Data Source") })
output$GVAworked_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "GVA per hour worked (£)"]
  p(recent_year) })
output$GVAworked_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "GVA per hour worked (£)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>","<br>","<p>",ten_change,"% 10-year change","</p>"))
})

#Enterprises
output$enterprises_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  a(href=link,
    "Data Source") })
output$enterprises_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Number of Enterprises per 10,000 population (16-64)"]
  p(recent_year) })
output$enterprises_year_change <- renderUI({
  one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Number of Enterprises per 10,000 population (16-64)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Service exports
output$serviceexports_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Service Exports Per Job (£)"]
  a(href=link,
    "Data Source") })
output$serviceexports_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Service Exports Per Job (£)"]
  p(recent_year) })
#only one year for this - will there be more?
#output$serviceexports_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Service Exports Per Job (£)" & indicators_change$change == "1 year change"],2)
  #three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Service Exports Per Job (£)" & indicators_change$change == "3 year change"],2)
 # HTML(paste0("<p>",one_change,"% 1-year change","</p>","<br>","<p>",three_change,"% 3-year change","</p>"))
#})

#Median Income
output$medianincome_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Residence-based Full-time Median Weekly Earnings"]
  a(href=link,
    "Data Source") })
output$medianincome_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Residence-based Full-time Median Weekly Earnings"]
  p(recent_year) })
output$medianincome_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "3 year change"],2)
  five_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Residence-based Full-time Median Weekly Earnings" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year change","</p>","<br>","<p>",five_change,"% 5-year change","</p>","<br>","<p>",ten_change,"% 10-year change","</p>"))
})

#Working age population with no qualification
output$noqualification_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Working-age population with no qualifications (%)"]
  a(href=link,
    "Data Source") })
output$noqualification_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Working-age population with no qualifications (%)"]
  p(recent_year) })
output$noqualification_year_change <- renderUI({
  #one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Working-age population with no qualifications (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Unemployed
output$unemployed_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Unemployment Rate 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$unemployed_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Unemployment Rate 16-64 (%)"]
  p(recent_year) })
output$unemployed_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "3 year change"],2)
  five_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "5 year change"],2)
  ten_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Unemployment Rate 16-64 (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Economic Inactivity
output$ecoinactive_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Economic Inactivity Rate 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$ecoinactive_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Economic Inactivity Rate 16-64 (%)"]
  p(recent_year) })
output$ecoinactive_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "5 year change"],2)
  ten_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Economic Inactivity Rate 16-64 (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Claimant count
output$claimcount_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"]
  a(href=link,
    "Data Source") })
output$claimcount_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Claimants as a proportion of residents aged 16-64 (%)"]
  p(recent_year) })
output$claimcount_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "3 year change"],2)
  five_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "5 year change"],2)
  ten_change <-round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Claimants as a proportion of residents aged 16-64 (%)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#Child poverty
output$childpoverty_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "% of children in poverty (after housing costs)"]
  a(href=link,
    "Data Source") })
output$childpoverty_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "% of children in poverty (after housing costs)"]
  p(recent_year) })
output$childpoverty_year_change <- renderUI({
  one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "% of children in poverty (after housing costs)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "% of children in poverty (after housing costs)" & indicators_change$change == "3 year change"],2)
  HTML(paste0("<p>",one_change,"% 1-year point change","</p>","<br>","<p>",three_change,"% 3-year point change","</p>"))
})

#Emissions
output$emissions_extra_info <- renderUI({
  link <- source_data$Source[source_data$Indicator == "Emissions per capita (tonnes)"]
  a(href=link,
    "Data Source") })
output$emissions_year <- renderUI({
  recent_year <- latest_data$Year[latest_data$Region == input$regionsmap_shape_click$id & latest_data$Indicator == "Emissions per capita (tonnes)"]
  p(recent_year) })
output$emissions_year_change <- renderUI({
 # one_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "1 year change"],2)
  three_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "3 year change"],2)
  five_change<- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "5 year change"],2)
  ten_change <- round(indicators_change$change_value[indicators_change$Region == input$regionsmap_shape_click$id & indicators_change$Indicator == "Emissions per capita (tonnes)" & indicators_change$change == "10 year change"],2)
  HTML(paste0("<p>",three_change,"% 3-year point change","</p>","<br>","<p>",five_change,"% 5-year point change","</p>","<br>","<p>",ten_change,"% 10-year point change","</p>"))
})

#downloading of the data - first go # works, but doesn't download in the right format
# output$single_area_summary_md <- downloadHandler(
#   filename = paste0(input$regionsmap_shape_click$id,"_area_summary.docx"),
#   content = function(file){
#     tempReport <- file.path(tempdir(), "region_summary_download.Rmd")
#     file.copy("region_summary_download.Rmd",tempReport, overwrite = TRUE)
#     #parameters to pass to Rmd document
#     params <- list(Region = input$regionsmap_shape_click$id)
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
    params <- list(Region = input$regionsmap_shape_click$id)
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
#subset data to indicator selected
selected_indicator_data <- reactive ({
  #trend <- 
  indicators_data %>%
    subset(Indicator == input$economic_indicator_choice &
             Region %in% input$location_choice) #|
  # Region == input$comparator_choice)
})

selected_indicator_data_year <- reactive ({
  #trend <- 
  latest_data %>%
    subset(Indicator == input$economic_indicator_choice &
             Region %in% input$location_choice) #|
  # Region == input$comparator_choice)
})

#for jobs will need to subset twice for % of jobs by sector

########### Map #####################
#map title
output$glasgow_map_title <- renderText({ input$economic_indicator_choice })
#set tooltip label

#map

########### Summary Table ###################
#table title

#table


###########Time trend graph #######################
#trend title
output$glasgow_timetrend_title <- renderText({ paste0("Historical data for ",input$economic_indicator_choice) })
#set tooltip label
tooltip_trend <- reactive({
  paste0(selected_indicator_data()$Region,"<br>",selected_indicator_data()$Value,"<br>",selected_indicator_data()$Year)
})
#trend graph
output$time_trend_glasgow <- renderPlotly({
  plot_ly(data=selected_indicator_data(), x=~Year,  y = ~Value,
          color = ~Region,
          text=tooltip_trend(), 
          hoverinfo="text"#, 
        # height = 600 
         ) %>% 
    add_trace(type = 'scatter', mode = 'lines+markers', marker = list(size = 8),
              symbol = ~Region#, symbols = symbols_trend
                            ) %>%
    layout(
      #title = paste0("Time trend for ",input$economic_indicator_choice),
      xaxis = list(title = "Year", color='white',tickcolor='white'),
      yaxis = list(title = "Value", color='white',tickcolor='white'),
      legend = list(font = list(color ='white')),
      plot_bgcolor='rgba(0, 0, 0, 0)',
      paper_bgcolor='rgba(0, 0, 0, 0.2)',
      fig_bgcolor='rgba(0, 0, 0, 0)',#selected_indicator_data()$Measure)
      showlegend = TRUE
    ) %>%
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
})

############## Bar Graph #########
#bar graph title
output$glasgow_bar_title <- renderText({ paste0(input$economic_indicator_choice, " compared across regions") })
#tooltip text

#bar graph
output$rank_plot <- renderPlotly({
  plot_ly(data = selected_indicator_data_year(), x = ~Region, y = ~Value, type='bar', marker = list(color='#E9BD43')) %>%  #,text=tooltip_bar, hoverinfo="text",
                                    #  marker = list(color = ~color_pal)
                  #for comaparator
  #add_trace(x = ~areaname, y = ~comp_value, name = ~unique(comp_name), type = 'scatter', mode = 'lines',
   #         line = list(color = '#FF0000'), showlegend = FALSE, hoverinfo="skip") %>%
  layout(xaxis = list(title="Regions", color='white',tickcolor='white'),
         yaxis = list(title="Value", color='white',tickcolor='white'),
         plot_bgcolor='rgba(0, 0, 0, 0)',
         paper_bgcolor='rgba(0, 0, 0, 0.2)',
         fig_bgcolor='rgba(0, 0, 0, 0)') %>%
    config(displayModeBar = FALSE, displaylogo = F) # taking out the plotly functions bar up top
                          })
} # server closing bracket