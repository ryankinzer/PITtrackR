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
    #menuItem("Snake Basin Summary", startExpanded = TRUE,
             
             #span(helpText('Please select a return year and press "Load Data".'),
             #     style = "color:red;padding-left:20px;"),
    menuItem("Data Selection", tabName = 'data',icon = icon("fish"), startExpanded = TRUE,
             selectInput('rtn_year', "Spawn Year:",
                         choices = c(2018:year(Sys.Date())),
                         selected = year(Sys.Date())),
             radioButtons('basin_spp', "Species:", inline = TRUE,
                          choices = 'Chinook', #c('Chinook','Coho', 'Steelhead', 'Sockeye'),
                          selected = 'Chinook'),
             
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
       color:red;padding-left:52px; padding-top:5px; padding-bottom:5px;")
             #div(style="display:inline-block",textOutput("loaded")),
             #withSpinner(textOutput("loaded")),             
             
    ), 
             
      menuItem("Lower Granite Observations", tabName = 'dams', icon = icon("line-chart"), selected = TRUE),
      menuItem("Snake Basin Summary", tabName = 'basin', icon = icon("bar-chart")),
      menuItem("Watershed Summaries", tabName = 'watershed', icon = icon("pie-chart")),
      menuItem('Raw Data', tabName = 'data', icon = icon("table")),
      menuItem('Download Reports', tabName = 'reports', icon = icon("file-download"))
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
                         plotOutput("gra_tagged"))),
              column(12,
                     box(width = NULL, height = 1100, solidHeader = TRUE, status = 'primary',
                         title = 'Daily Tag Detections at Lower Granite',
                         plotOutput("release_plot", height = 1000))),
              
              column(12,
                     box(width = NULL, solidHeader = TRUE, status = 'primary',
                         title = 'Cumulative arrival of adults tagged as juveniles to Lower Granite',
                         plotOutput("gra_arrival", height = 1000)))
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
            column(6,
                   box(width = NULL, solidHeader = TRUE, status = 'primary',
                       title = 'Tags observed at each detection site.',
                       leafletOutput("map", width = '100%')#, height = 540)
                       )
                   ),
            column(6,
                   box(width = NULL, solidHeader = TRUE, status = 'primary',
                       title = 'Cumulative arrival of tags to each sub-basin.',
                       plotOutput('basin_arrival')))
            ),
      fluidRow(
          column(6,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Origin of tags detected at site.',
                    # title = 'Minimum arrival date to each site.',
                 plotOutput("basin_tag_plot"))),
          column(6,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Travel time from Lower Granite to sub-basins.',
                 plotOutput('basin_travel'))),
          column(6,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Total tags observed in sub-basins.',
                     DT::DTOutput('site_tags'))),
          column(6,
                 box(width = NULL, solidHeader = TRUE, status = 'primary',
                     title = 'Estimated tags and site detection efficiencies.',
                     DT::DTOutput("basin_est_tags")))
          # column(12,
          #        box(width = NULL, solidHeader = TRUE, status = 'primary',
          #            title = 'Migratory time from Lower Granite to sub-basins.',
          #            plotOutput('basin_mig_plot')))
      )
      ),
      tabItem("watershed",
              fluidRow(
                       box(solidHeader = TRUE, status = 'primary',
                           title = 'Watershed Summary Selections',
                           uiOutput("watershed_menu"),
                           radioButtons('pit_spp', "Species of Interest:", inline = TRUE,
                                    choices = c('Chinook', "Bull Trout"),
                                    selected = 'Chinook')
                       ),
                      
                      column(4,
                              valueBoxOutput("watershed_tags", width = NULL)
                       )
              ),
              
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
                      uiOutput("tagid_menu"),
                      plotOutput("mig_plot", width = "100%")
                    )
                )
              )
      ),
    tabItem('data',
            # selectInput('rawdata', "Data Type:",
            #             choices = c('Processed Observations', 'PTAGIS Chinook Query', 'PTAGIS Bull Trout Query'),
            #             selected = 'Processed Observations'),
            downloadButton('data_export', "Export Data"),
            DT::DTOutput('raw_data'),style = "height:800px; overflow-y: scroll;overflow-x: scroll;"
            ),
    tabItem('reports',
            box(solidHeader = TRUE, status = 'primary',
                title = 'Select Watershed Report',
                selectInput("pdf_reports", "Available Reports:", 
                            choices = c('Imnaha River'), selected = 'Imnaha River'),
                downloadButton("reports", label = 'Download Report')
            )
            )
    )
  )      

# Final Format ----
dashboardPage(
  header,
  side,
  body
  )