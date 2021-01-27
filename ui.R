###############################################
#Formatting required for header and navbar
###############################################
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs from UI for the functions to work
  #div(img(src="glasgow_header.jpg", style="height:100%; width: 100%; position: relative;")),
  div(class = "container",
      br(),
      p(class="header-title", "GLASGOW CITY REGION", style = "margin-top: 10px; opacity: 0.9;"),   #, style="color: white; font-family: 'Franklin Gothic Medium', 'Franklin Gothic', 'ITC Franklin Gothic', Arial, sans-serif;  font-size: 40px; line-height: 20px"),
      p(class="header-title", "ECONOMIC", style = "opacity: 0.7;"), #style="color: font-family: 'Franklin Gothic Medium', 'Franklin Gothic', 'ITC Franklin Gothic', Arial, sans-serif;  font-size: 40px;"),
      p(class="header-title", "DASHBOARD", style = "opacity: 0.5;"),#, style="color: white; font-family: 'Franklin Gothic Medium', 'Franklin Gothic', 'ITC Franklin Gothic', Arial, sans-serif;  font-size: 40px;")
      br()
      ),
  navbarPage(id = "intabset", #landingpage title
             title = "",
             windowTitle = "Glasgow City Region Economic Dashboard", #title for browser tab
             theme = shinytheme("slate"), #Theme of the app #dark blue background
             collapsible = TRUE, #tab panels collapse into menu in small screens
             # header =         
             # tags$head( #CSS styles
             #  cookie_box, ##Cookie box
            #tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
            header = includeCSS("style.css"),
             #  HTML("<base target='_blank'>") # to make external links open a new tab
           #  ,
             
########################################################
#Landing Page
#########################################################
  tabPanel(class= "tabs",title="Single Local Authority Summary", icon = icon("search-dollar"),   
            mainPanel(width=11, style="margin-left:4%, margin-right:4%",
                      fluidRow(column(7,p(h3("Welcome to the Glasgow City Region Tool")))),
                      #print out map showing Glasgow city regions - for user interaction
                      fluidRow(column(6,leafletOutput("regionmap")),
                              #only show summary if an area has been selected
                               # conditionalPanel("input$regionsmap_shape_click$id!==null && input$regionsmap_shape_click$id!==''",
                                        #print out area that has been selected on the map
                                        column(6,
                                        div(id="intro_text", 
                                            br(),br(),br(),br(),br(),br(), br(),br(),p("Please select an area on the map to get overall statistics for that area")),
                                 hidden(div(id="area_detail",textOutput("region_name"),
                                        br(), br(),
                                        #printing out summary data          
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Total Population")),
                                                                       div(uiOutput("pop_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_population"))),
                                            div(class="expanding_element",uiOutput("pop_year_change", style="line-height: 1px;"),
                                                                          br(),uiOutput("pop_extra_info"),br())),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Total Jobs")),
                                                         div(uiOutput("jobs_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_jobs"))),
                                            div(class= "expanding_element",uiOutput("jobs_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("jobs_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("GVA/Hr Worked")),
                                                                      div(uiOutput("GVAworked_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_GVAworked"))),
                                            div(class= "expanding_element",uiOutput("GVAworked_year_change", style="line-height: 1px;"),
                                                        br(),uiOutput("GVAworked_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Enterprises/10k Population")),
                                                                      div(uiOutput("enterprises_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_enterprises"))),
                                            div(class= "expanding_element",uiOutput("enterprises_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("enterprises_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Service Exports/Job")),
                                                                      div(uiOutput("serviceexports_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_serviceexports"))),
                                            div(class= "expanding_element",#uiOutput("serviceexports_year_change", style="line-height: 1px;"), br(),
                                                uiOutput("serviceexports_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Total Weekly Median Income")),
                                                                      div(uiOutput("medianincome_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_medianincome"))),
                                            div(class= "expanding_element",uiOutput("medianincome_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("medianincome_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Working Age Population with No Qualifications")),
                                                                      div(uiOutput("noqualification_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_noqualification"))),
                                            div(class= "expanding_element",uiOutput("noqualification_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("noqualification_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Unemployment Rate")),
                                                                      div(uiOutput("unemployed_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_unemployed"))),
                                            div(class= "expanding_element",uiOutput("unemployed_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("unemployed_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Economic Inactivity Rate")),
                                                                      div(uiOutput("ecoinactive_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_ecoinactive"))),
                                            div(class= "expanding_element",uiOutput("ecoinactive_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("ecoinactive_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Claimant Count (of residents aged 16-64)")),
                                                                       div(uiOutput("claimcount_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_claimcount"))),
                                            div(class= "expanding_element",uiOutput("claimcount_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("claimcount_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Children in Poverty")),
                                                                      div(uiOutput("childpoverty_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_childpoverty"))),
                                            div(class= "expanding_element",uiOutput("childpoverty_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("childpoverty_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Emissions Per Capita")),
                                                                      div(uiOutput("emissions_year"),style = "line-height:1px; font-size: 10px;"),
                                            div(class= "summary_value",textOutput("region_emissions"))),
                                            div(class= "expanding_element",uiOutput("emissions_year_change", style="line-height: 1px;"),
                                                br(),uiOutput("emissions_extra_info")))),
                                        br(),br(),
                                        fluidRow(column(6,offset=3,
                                          div(class="button",downloadButton("single_area_summary_md", 'Download this summary') 
                                        )))
                                 )) #hidden brackets
                                        ) # column bracket
                             #   ) #conditionalPanel bracket
                      )# row bracket
                      ) #panel bracket
            ),  #tab bracket
             
########################################################
#Compare Glasgow City Regions to each other tab
#########################################################
             
             tabPanel(title = "Compare Glasgow City Local Authorities", icon = icon("balance-scale-left"),
                      wellPanel(class= "subheader", fluidRow(
                        column(4,div(class="selector",p(tags$b("1. Select an indicator of interest")),
                                   selectizeInput("economic_indicator_choice", label = NULL, choices=indicators_cleaned,
                                               selected = NULL)),
                               conditionalPanel(condition= "input.economic_indicator_choice == 'Total Jobs'",
                               div(class="selector",
                                   selectizeInput("jobs_choice", label = "Show for sector", choices = job_sectors, 
                                                  selected = NULL, multiple=TRUE, 
                                                  options = list(maxOptions = 1300, placeholder = "Showing all job sectors unless filtered")))
                                                 ),
                               actionButton("definition","Indicator definition"), br()
                               ),
                        column(4,div(class="selector",p(tags$b("2. Select local authorities of interest")),
                                     selectizeInput("glasgow_region_choice", label = NULL,
                                                          choices = glasgow_regions, selected = character(0), multiple = TRUE,
                                                          options = list(maxOptions = 1300, 
                                                                         placeholder = "Select one or more local authorities of interest")))
                                     #selectInput("location_choice", label = "Multiple areas may be chosen", choices=glasgow_regions,
                                      #           selected = NULL, multiple=TRUE))
                               ),
                        column(4,div(class="selector",p(tags$b("3. Choose a comparator area")),
                                     selectizeInput("comparator_choice", label = NULL, choices=comparators,
                                                 selected = NULL,  options = list(maxOptions = 1300, 
                                                                                  placeholder = "Select a comparator area to compare regions to")))
                      )
                      )),
                      fluidRow(
                        div(id="intro_page2_text", 
                                br(),br(),p("Please select at least two areas to compare")),
                        hidden(div(id="glasgow_areas_comparison",
                        #Leaflet map to show areas
                        column(7,div(class="graph_title",
                                     textOutput("glasgow_map_title")),
                                br(),
                               leafletOutput("glasgow_map"),
                              # div(class="graph_title",
                               #    textOutput("glasgow_timetrend_title")),
                               plotlyOutput("time_trend_glasgow")
                               ), # column bracket
                        column(5, #offset=1, 
                              # div(class="latest_figure",uiOutput("latest_figure")),
                               div(class="graph_title",textOutput("glasgow_bar_title")),
                               plotlyOutput("rank_plot"),
                             # conditionalPanel(condition="input.economic_indicator_choice == 'Total Jobs' & input.jobs_choice != character(0)",
                              #                 plotlyOutput("jobs_by_sector")),
                              br(), br(),DTOutput('glasgow_summary_table'), br(), br(),
                             div(class="button",downloadButton("glasgow_regions_download", 'Download comparison data', class = "down")) 
                               )))) # row bracket
                    #  fluidRow(
                     #   column(4,
                      #         )
                     # ) #row bracket                                                                
                     ), # end of tab
########################################################
#Compare UK City Regions to each other tab
#########################################################             
             tabPanel(title="Compare UK City Regions", icon=icon("globe-europe"),
                      wellPanel(class= "subheader", fluidRow(
                        column(4,div(class="selector",p(tags$b("1. Select an indicator of interest")),
                                     selectizeInput("uk_economic_indicator_choice", label = NULL, choices=indicators_cleaned,
                                                    selected = NULL)),
                               conditionalPanel(condition= "input.economic_indicator_choice == 'Total Jobs'",
                                                div(class="selector",
                                                    selectizeInput("uk_jobs_choice", label = "Show for sector", choices = job_sectors, 
                                                                   selected = NULL, multiple=TRUE, 
                                                                   options = list(maxOptions = 1300, placeholder = "Showing all job sectors unless filtered")))
                               )
                        ),
                        column(4,div(class="selector",p(tags$b("2. Select UK city regions of interest")),
                                     selectizeInput("uk_region_choice", label = NULL,
                                                    choices = uk_regions, selected = "Glasgow City Region", multiple = TRUE,
                                                    options = list(maxOptions = 1300, 
                                                                   placeholder = "Select one or more regions of interest")))
                               #selectInput("location_choice", label = "Multiple areas may be chosen", choices=glasgow_regions,
                               #           selected = NULL, multiple=TRUE))
                        ),
                        column(4,div(class="selector",p(tags$b("3. Choose a comparator area")),
                                     selectizeInput("uk_comparator_choice", label = NULL, choices=comparators,
                                                    selected = NULL,  options = list(maxOptions = 1300, 
                                                                                     placeholder = "Select a comparator area to compare regions to")))
                        )
                      )),
                      fluidRow(
                        div(id="intro_page3_text", 
                            br(),br(),p("Please select at least two areas to compare")),
                        hidden(div(id="uk_areas_comparison",
                                   #Leaflet map to show areas
                                   column(7,div(class="graph_title",
                                                textOutput("uk_map_title")), br(),
                                          leafletOutput("uk_map"),
                                          plotlyOutput("time_trend_uk")
                                   ), # column bracket
                                   column(5, #offset=1, 
                                          # div(class="latest_figure",uiOutput("latest_figure")),
                                          div(class="graph_title",textOutput("uk_bar_title")),
                                          plotlyOutput("uk_rank_plot"),
                                          # conditionalPanel(condition="input.economic_indicator_choice == 'Total Jobs' & input.jobs_choice != character(0)",
                                          #                 plotlyOutput("jobs_by_sector")),
                                          br(), br(),DTOutput('uk_summary_table'), br(), br(),
                                          div(class="button",downloadButton("uk_regions_download", 'Download comparison data', class = "down")) 
                                   ))))
             ), #end of tab
tabPanel(class= "tabs",title="About", icon = icon("info-circle"),
         mainPanel(width=11, style="margin-left:4%, margin-right:4%",
           #fluidRow(column(7,br(),br())),
           fluidRow(column(9,
            h3("About"),
             p("This dashboard was designed by UDBC for Glasgow City Regions Intelligence Hub with the aim of allowing users to explore key economic indicators in relation to the Glasgow City Region.
                 If you have any questions, please contact us at email@email.com."),
             h3("How to Use"),
             p("The app allows you to do three specific things using the corresponding tabs:"),
             p("  1. Explore all indicators at the same time for a local authority within the Glasgow City Region."),
             p("  2. Compare local authorities within the Glasgow City Region for a specific indicator."),
             p("  3. Compare city regions within the UK for a specific indicator."),
             h3("Data Behind the Tool"),
             p("All data presented in the tool comes from open national datasets and individual sources for each indicator are referenced within the app.
               We aim to update the data behind the app within x days of new data being published. Should you have any questions about the data specifically please
               contact us at email@email.com.")
            ),
            column(3,br(),div(img(src="UBDC_transparent_logo.png", style="height:80%; width: 80%; position: right;")))
             ) # row bracket
         ) #panel closing bracket
                  )   #end of tab       
             
  ))
