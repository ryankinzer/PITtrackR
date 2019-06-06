# Header ----
header <- dashboardHeader(
  title = "PITtrackR",

  tags$li(a(href = 'http://www.nptfisheries.org',
             img(src = 'NPTlogos2.png',
                 title = 'Company Home', height = "30px"),
             style = "padding-top:10px; padding-bottom:10px;"),
           class = "dropdown")
)

# Side Bar ----
side <- dashboardSidebar(
  
  sidebarMenu(
    menuItem("Snake Basin Summary", startExpanded = TRUE,
             
             #span(helpText('Please select a return year and press "Load Data".'),
             #     style = "color:red;padding-left:20px;"),
             selectInput('rtn_year', "Spawn Year:",
                         choices = c(2018:year(Sys.Date())),
                         selected = year(Sys.Date())),
             radioButtons('basin_spp', "Species:", inline = TRUE,
                          choices = 'Chinook',#c('Chinook','Coho', 'Steelhead'),
                          selected = NULL),
             
             # withBusyIndicatorUI(
             #   div(style="display:inline-block",
             #   actionButton(
             #     "loadData",
             #     "Load Data",
             #     class = "btn-primary"
             #   )
             #   )
             # ),
             
             tags$head(
               tags$style(HTML('#loadData{background-color:lightblue}'))
             ),
             
             #div(style="display:inline-block",actionButton("loadData", "Load Data")),
             actionButton("loadData", "Load/Refresh Data", width="87%", class = "btn btn-primary"),
             span(textOutput("loaded"),style="display:inline-block;
       color:red;padding-left:52px; padding-top:5px; padding-bottom:5px;"),
             #div(style="display:inline-block",textOutput("loaded")),
             #withSpinner(textOutput("loaded")),             
             
             
             
      menuSubItem("Lower Granite Observations", tabName = 'dams', selected = TRUE),
      menuSubItem("Snake Basin Tag Observations", tabName = 'basin')
      ),
      
    menuItem("Watershed Summaries",# startExpanded = TRUE,
      menuSubItem("Watershed Observations", tabName = 'watershed'),    
      radioButtons('pit_spp', "Watershed Species:", inline = TRUE,
                   choices = c('Chinook', "Bull Trout"),
                   selected = 'Chinook'),
      uiOutput("watershed_menu"),
      uiOutput("tagid_menu"),
      menuSubItem("Watershed Reports")
    ),

    
    menuItem('Raw Data', #startExpanded = TRUE,
             menuSubItem("Processed Tag Data", tabName = 'prochist_data')
            # menuSubItem("Chinook PTAGIS Data", tabName = 'chinook_data'),
            # menuSubItem("Bull Trout PTAGIS Data", tabName = 'bull_data')
    )
  )
) 

# Body ----
body <- dashboardBody(

  tabItems(
    tabItem("dams",
            fluidRow(
              column(6,
                     valueBoxOutput("windowCnts_grp1", width = NULL)),
              column(6,
                     valueBoxOutput("windowCnts_grp2", width = NULL))
            ),
            fluidRow(
              column(4, 
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                            title = 'Previously Tagged Adults Observed at Lower Granite',
                            plotOutput("release_grp"))),
              column(4,
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                            title = 'Lower Granite Window Count',
                            plotOutput("window_plot"))),
              column(4,
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                         title = 'Newly Tagged Adults Released from Lower Granite',
                         plotOutput("gra_tagged")))
              ),
            fluidRow(
              column(12,
                     box(width = NULL, height = 1100, solidHeader = TRUE, status = 'primary',
                         title = 'Daily Tag Detections at Lower Granite',
                         plotOutput("release_plot", height = 1000)))
            )

    ),
    tabItem("basin",
            fluidRow(
              column(4,
                     valueBoxOutput("tagsGRA", width = NULL)
              ),
              column(4,
                     valueBoxOutput("tagsObs", width = NULL)
              ),
              column(4,
                     valueBoxOutput("tagsPercent", width = NULL)
              )
            ),

      fluidRow(
            column(7,
                   box(width = NULL, solidHeader = TRUE, status = 'primary',
                       title = 'Tags observed at each detection site.',
                       leafletOutput("map", width = '100%', height = 540)
                       )
                   ),
            column(5,
                   box(width = NULL, solidHeader = TRUE, status = 'primary',
                       title = 'Total tags observed in sub-basins.',
                       DT::DTOutput('site_tags')
                     )
                   )
            ),
      fluidRow(
          column(6,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Origin of tags detected at site.',
                    # title = 'Minimum arrival date to each site.',
                 plotOutput("basin_tag_plot"))),
          column(6,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Travel time from Lower Granite to sub-basins..',
                 plotOutput('basin_travel'))),
          column(12,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Estimated tags and site detection efficiencies.',
                     DT::DTOutput("basin_est_tags")))
      )
      ),
      tabItem("watershed",
            fluidRow(
                column(6,
                       box(width = NULL, solidHeader= TRUE, status = 'primary',
                           title = 'First arrival date at each site.',
                           plotOutput("arrival_plot"))),
                column(6,
                       box(width = NULL, solidHeader = TRUE, status = 'primary',
                           title = 'Travel time of through watershed reaches.',
                           plotOutput("watershed_travel")))
              ),
            fluidRow(
              # column(4,
              #        box(width = NULL, solidHeader = TRUE, status = 'primary',
              #            title = 'Tags observed by release location.',
              #            DT::DTOutput("unique_tags")
              #        )
              # ),
              column(6,
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                         title = 'Tags observed at each site.',
                         DT::DTOutput("unique_tags_site")
                     )
                ),
              column(6,
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                         title = 'Estimated tags and detection efficiencies.',
                         DT::DTOutput("watershed_est_tags")
                     )
              )
              ),            
            fluidRow(
              column(12,
                  box(width = NULL, solidHeader = TRUE, status = 'primary', 
                      title = 'Migratory patterns through watershed reaches.',
                      plotOutput("mig_plot", width = "100%")
                    )
                )
              )
      ),
    tabItem('prochist_data',
            downloadButton('data_export', "Export Data"),
            DT::DTOutput('raw_data'),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
            )
    )
  )      

# Final Format ----
dashboardPage(
  header,
  side,#dashboardSidebar(disable = TRUE),
  body
  )



# # Data Tabs ----
#                       hr(),
#                       tabsetPanel(
# # Summary Plots and Tables ----
#                       tabPanel("Data Summary",
#                                fluidPage(
#                                  fluidRow(
#                                    column(12,
#                                           h2("Summary Plots of Unique Tags")),
#                                    column(4, align = 'center',
#                                           h3("Arrival date to a site:"),
#                                           uiOutput("sized_arrival")),
#                                    column(4, align = 'center',
#                                           h3("Current Tag Status based on last detection:"),
#                                           uiOutput("sized_nomove")),
#                                    column(4, align = 'center',
#                                           h3("Travel time between sites:"),
#                                           uiOutput("sized_travel_1_3"))
#                                           #uiOutput("sized_travel_3_4"),
#                                           #uiOutput("sized_travel_4_5"))
#                                  ),
#                                  fluidRow(
#                                   column(12,
#                                          h2("Summary Counts of Unique Tags")
#                                          ),
#                                   column(4, align = 'center',
#                                          h3("By origin:"),
#                                          tableOutput("unique_tags"),
#                                          h3("By passage route:"),
#                                          tableOutput("passage_route"),
#                                          h3("Passage rate:"),
#                                          tableOutput("passage_rate")
#                                          ), # close column
#                                   column(4, align = 'center',
#                                          h3("By origin and release site:"),
#                                          tableOutput("unique_tags_site"),
#                                          h3("By trap status:"),
#                                          tableOutput("trap_status")
#                                   ), # close column
#                                   column(4, align = 'center',
#                                          h3("By origin and current tag fate:"),
#                                          tableOutput("tag_fate")
#                                   ) # close column
#                                  ),
#                                 fluidRow(
#                                   column(12,
#                                          h2("Estimated Tags")),
#                                   column(12, align = 'center',
#                                          h3("Detection efficiencies and estimated tags by node:"),
#                                          tableOutput("est_tags_node"),
#                                          h3("Detection efficiencies and estimated tags by site:"),
#                                          tableOutput("est_tags_site"))
#                                 )
#                                ) # closes fluid page for summary panel
#                                ), # closes summary panel
# # Raw Data Tab ----
#                       tabPanel("Raw Data",
#                                fluidPage(
#                                  fluidRow(
#                                    column(4,
#                                           h3("Export Raw PITtrackR Data"),
#                                           helpText("Select the dataset you wish to export:")),
#                                    column(4,radioButtons('rawdata', h3("Dataset:"), inline = TRUE,
#                                                        choiceNames = c('PITcleanR Processed', "Detection History", "Raw Chinook", "Raw Bull Trout"),
#                                                        choiceValues = c('dat_all', 'detect_hist', 'raw_chs', 'raw_bull'))),
#                                    column(2, offset = 2,
#                                           h3("Download:"),
#                                           downloadButton("data_export", label = "Export .CSV File", class = "mybutton"))
#                                  ), # close first fluidrow
#                                  fluidRow(
#                                    column(12,
#                                     DT::dataTableOutput("raw_data"))
#                                    )# close fluidRow
#                                ) # close fluid page for raw data
#                             ), # close raw data tabPanel
# # Citations and Contacts ----
#                       tabPanel("Citations",
#                                fluidPage(
#                                  fluidRow(
#                                    column(12,
#                                           h3("Acknowledgements"),
#                                           align = "center"
#                                    )
#                                  ),
#                                  fluidRow(
#                                    column(12,
#                                           p("The information presented in the PITtrackR web application is the product of 
#                                             a collaborative effort between Joseph Feldhaus, Tim Whitsel, Paul Sankovich
#                                             and Ryan Kinzer. The information is intended to assist Imnaha River fisheries
#                                             managers in making operational decisions for the Imnaha River weir using near real-time
#                                             PIT-tag detections.  PITtrackR uses PIT-tag data that is collected and managed by
#                                             multiple tribes, states and federal agencies and made available from PTAGIS.
#                                             Without everyone's generous policies regarding public and regional
#                                             PIT-tag data sharing this application would not be possible. We would like to
#                                             individually thank all of the people involved with the data presented on this site,
#                                             however, the list of contributors is too numerous to identify everyone by name.
#                                             If we have used data that you are personally connected with, thank you!  And if
#                                             you feel any data presented here is portrayed incorrectly, or if you would like
#                                             the information you are connected with removed from this application please
#                                             contact us to discuss.")
#                                           )
#                                           ),
#                                  hr(),
#                                  fluidRow(
#                                    column(12,
#                                           h3("Contacts"),
#                                           align = "center"
#                                    )
#                                  ),
#                                  hr(),
#                                  fluidRow(
#                                    column(6,
#                                           h4("Ryan Kinzer"), 
#                                           h4("Nez Perce Tribe"),
#                                           h4("Department of Fisheries Resources Management"),
#                                           h4("208-640-5290"),
#                                           h4("ryank@nezperce.org"),
#                                           align = "center"
#                                           ),
#                                    column(6,
#                                           h4("Joseph Feldhaus"),
#                                           h4("Oregon Department of Fish and Wildlife"),
#                                           h4("Northeast Region Fish Research"),                                          
#                                           h4("541-962-3724"),
#                                           h4("Joseph.Feldhaus@state.or.us"),
#                                           align = "center"
#                                         )
#                                  ),
#                                  hr(),
#                                  fluidRow(
#                                    column(12,
#                                           h3("Data Sources, R Packages and References"),
#                                           align = "center"
#                                    )
#                                  ),
#                                  hr(),
#                                  fluidRow(
#                                    column(12,
#                                           p("Kevin See, Ryan N. Kinzer, Rick Orme and Mike Ackerman. PITcleanr: Cleans up PIT tag capture histories for DABOM. R package version 0.0.0.9000.
#                                             https://github.org/kevinsee/PITcleanR"),
#                                           p("Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1.
#                                             https://CRAN.R-project.org/package=tidyverse"),
#                                           p("  Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL
#                                             http://www.jstatsoft.org/v40/i03/."),
#                                           p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version
#                                             1.0.5. https://CRAN.R-project.org/package=shiny"),
#                                           p("Joe Cheng, Bhaskar Karambelkar and Yihui Xie (2017). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package
#                                             version 1.1.0. https://CRAN.R-project.org/package=leaflet"),
#                                           p(paste0("PTAGIS. The Columbia Basin PIT Tag Information System. Accessed through ftp server site on ", Sys.Date(),"."))
#                                           ) # close column
#                                           ) # close fluidRow
#                                  ) # close fluidPage
#                                ) # close tabPanel for citation
#                       ) # close tabsetPanel
#               ) # close tabPanel PITtrackR
#  ) # close navbarPage
#) # close server
