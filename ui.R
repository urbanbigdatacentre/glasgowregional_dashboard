###############################################.
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs from UI for the functions to work
  #div(img(src="glasgow_header.jpg", style="height:100%; width: 100%; position: relative;")),
  div(class = "container",
      br(),
      p(class="header-title", "GLASGOW REGIONS", style = "margin-top: 10px; opacity: 0.9;"),   #, style="color: white; font-family: 'Franklin Gothic Medium', 'Franklin Gothic', 'ITC Franklin Gothic', Arial, sans-serif;  font-size: 40px; line-height: 20px"),
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
  tabPanel(class= "tabs",title="Single Area Summary", icon = icon("search-dollar"),   
            mainPanel(width=11, style="margin-left:4%, margin-right:4%",
                      fluidRow(column(7,p(h3("Welcome to the Glasgow City Region Tool")))),
                      #print out map showing Glasgow city regions - for user interaction
                      fluidRow(column(6,leafletOutput("regionsmap")),
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
                                                                          br(),uiOutput("pop_extra_info"))),
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
                                          downloadButton("single_area_summary_md", 'Download this summary', class = "down") 
                                        ))
                                 )) #hidden brackets
                                        ) # column bracket
                             #   ) #conditionalPanel bracket
                      )# row bracket
                      ) #panel bracket
            ),  #tab bracket
             
             ########################################################
             #Papers in PubMed Central
             #########################################################
             
             tabPanel(title = "Compare Glasgow City Regions", icon = icon("balance-scale-left"),
                      wellPanel(fluidRow(
                        column(4,div(class="selector",p(tags$b("1. Select an indicator of interest")),
                                   selectInput("economic_indicator_choice", label = NULL, choices=indicators_cleaned,
                                               selected = NULL))
                              # textOutput("data_source")
                             #  pickerInput(
                              #   inputId = "Id088",
                               #  label = "Step 1. Select an indicator of interest.", 
                                # choices = indicators_cleaned,
                                # options = list(
                                 #  style = "#282829")
                               ),
                        column(4,div(class="selector",p(tags$b("2. Select local authorities of interest")),
                                     selectInput("location_choice", label = "Multiple areas may be chosen", choices=glasgow_regions,
                                                 selected = NULL, multiple=TRUE))
                               ),
                        column(4,div(class="selector",p(tags$b("3. Choose a comparator area")),
                                     selectInput("comparator_choice", label = "All regions are shown by default, delete accordingly", choices=comparators,
                                                 selected = NULL))
                      )
                      )),
                      fluidRow(
                        #Leaflet map to show areas
                        column(5,div(class="graph_title",
                                     textOutput("glasgow_map_title"))
                               ), # column bracket
                        column(6, offset=1, div(class="graph_title",
                                     textOutput("glasgow_timetrend_title")),
                               plotlyOutput("time_trend_glasgow")
                               )) # row bracket
                    #  fluidRow(
                     #   column(4,
                      #         )
                     # ) #row bracket                                                                
                     ), # end of tab
             
             tabPanel(title="Compare UK City Regions", icon=icon("globe-europe")
                      
             ) #end of tab
             
             
  ))
