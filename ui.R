###############################################.
tagList( #needed for shinyjs
  useShinyjs(),  # Include shinyjs from UI for the functions to work
  #div(img(src="glasgow_header.jpg", style="height:100%; width: 100%; position: relative;")),
  div(class = "container",
      img(src="glasgow_header.jpg", style="width:100%; height:220px;")#,
      #p(h1("Title over text", style="color: white; z-index: 100;"))
      ),
  navbarPage(id = "intabset", #landingpage title
             title = "", #div(tags$a(img(src="glasgow_header.jpg", height=35)),
             #            style = "position: relative; top: -6px;"), # Navigation bar
             windowTitle = "Glasgow City Region", #title for browser tab
             theme = shinytheme("slate"), #Theme of the app #dark blue background
             collapsible = TRUE, #tab panels collapse into menu in small screens
             # header =         
             # tags$head( #CSS styles
             #  cookie_box, ##Cookie box
             #  tags$link(rel="shortcut icon", href="favicon_scotpho.ico"), #Icon for browser tab
            header = includeCSS("style.css"),
             #  HTML("<base target='_blank'>") # to make external links open a new tab
           #  ,
             
########################################################
#Landing Page
#########################################################
  tabPanel(title="Single Area Summary", icon = icon("search-dollar"),   
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
                                            div(class= "summary_title",p("Population")),
                                            div(class= "summary_value",textOutput("region_population"))),
                                            div(class="expanding_element",uiOutput("pop_year"),
                                            uiOutput("pop_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Total Jobs")),
                                            div(class= "summary_value",textOutput("region_jobs"))),
                                            div(class= "expanding_element",uiOutput("jobs_year"),uiOutput("jobs_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("GVA per Hr Worked (£)")),
                                            div(class= "summary_value",textOutput("region_GVAworked"))),
                                            div(class= "expanding_element",uiOutput("GVAworked_year"), uiOutput("GVAworked_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Enterprises Per 10k Population")),
                                            div(class= "summary_value",textOutput("region_enterprises"))),
                                            div(class= "expanding_element",uiOutput("enterprises_year"),uiOutput("enterprises_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Service Exports Per Job (£)")),
                                            div(class= "summary_value",textOutput("region_serviceexports"))),
                                            div(class= "expanding_element",uiOutput("serviceexports_year"),uiOutput("serviceexports_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Total Median Income(weekly/£)")),
                                            div(class= "summary_value",textOutput("region_medianincome"))),
                                            div(class= "expanding_element",uiOutput("medianincome_year"),uiOutput("medianincome_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Working Age Population - no Qualifications (%)")),
                                            div(class= "summary_value",textOutput("region_noqualification"))),
                                            div(class= "expanding_element",uiOutput("noqualification_year"),uiOutput("noqualification_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Unemployment Rate (%)")),
                                            div(class= "summary_value",textOutput("region_unemployed"))),
                                            div(class= "expanding_element",uiOutput("unemployed_year"),uiOutput("unemployed_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("Economic Inactivity Rate (%)")),
                                            div(class= "summary_value",textOutput("region_ecoinactive"))),
                                            div(class= "expanding_element",uiOutput("ecoinactive_year"),uiOutput("ecoinactive_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Claimant Count (% of residents aged 16-64)")),
                                            div(class= "summary_value",textOutput("region_claimcount"))),
                                            div(class= "expanding_element",uiOutput("claimcount_year"),uiOutput("claimcount_extra_info")))),
                                        fluidRow(column(4,offset=1,div(class= "summary_box",
                                            div(class= "summary_title",p("% Children in Poverty")),
                                            div(class= "summary_value",textOutput("region_childpoverty"))),
                                            div(class= "expanding_element",uiOutput("childpoverty_year"),uiOutput("childpoverty_extra_info"))),
                                        column(4,offset=2,div(class= "summary_box",
                                            div(class= "summary_title",p("Emissions Per Capita")),
                                            div(class= "summary_value",textOutput("region_emissions"))),
                                            div(class= "expanding_element",uiOutput("emissions_year"),uiOutput("emissions_extra_info")))),
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
                        column(4,div(class="selector",p(tags$b("2. Change local authorities being shown")),
                                     selectInput("location_choice", label = "All regions are shown by default, delete accordingly", choices=glasgow_regions,
                                                 selected = glasgow_regions, multiple=TRUE))
                               ),
                        column(4,div(class="selector",p(tags$b("3. Choose a comparator area")),
                                     selectInput("comparator_choice", label = "All regions are shown by default, delete accordingly", choices=comparators,
                                                 selected = NULL))
                      )
                      )
                                                                ) # panel bracket
              ), # end of tab
             
             tabPanel(title="Compare UK City Regions", icon=icon("globe-europe")
                      
             ) #end of tab
             
             
  ))
