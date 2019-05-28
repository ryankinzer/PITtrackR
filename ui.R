#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
#library(tidyverse)
#library(lubridate)

# Define UI for application that draws a histogram
shinyUI(
  navbarPage(title = div(#div(id = "header-id", "KUS: DFRM Fisheries Data"),
    div(id = 'logo-id',img(src="NPTlogos2.png", height = 70)),
    tags$a("DFRM Home",href = 'http://www.nptfisheries.org')),
    id = "kus_navbar",
    windowTitle = "PITtrackR",
    theme = "styles.css",
    position = "fixed-top",
    collapsible = TRUE,
    footer = div(hr(),div(id = "footer-id",
                 "The data presented in this web application may not be solely collected, managed or owned
                 by the Nez Perce Tribe. All data should be considered draft and is not guaranteed for
                 accuracy.  Permission to use the data should be sought from the original collectors and data managers.
                Citations for the data and R packages used to create this application can be found on the reference tab.")),
    # tabPanel("Kus Home",
    #          fluidPage(
    #            fluidRow(
    #              column(12,
    #                     div(id = "homo-photo", img(src="P9040096.JPG")),
    #                     align = "center"
    #              )
    #            )
    #          )
    # ),
     tabPanel("PITtrackR",
                    fluidPage(
                      fluidRow(
                        column(12, align = "center",
                               h1("PITtrackR")
                               #textOutput("pittrackr_title")
                               )
                      ),
                      fluidRow(
                            column(3,
                                   helpText("Select the species, watershed and return year of interest:"),
                                   radioButtons('pit_spp', h3("Species:"), inline = TRUE,
                                                choiceNames = c('Chinook salmon', "Bull Trout"),
                                                choiceValues = c('Chinook', 'Bull Trout')),
                                   selectInput('watershed', h3("Watershed:"),
                                               c("Choose one" = "","Imnaha River" = "Imnaha River")),                                  
                                   selectInput('pit_year', h3("Return Year:"),
                                               c("Choose one" = "","2018"))
                                 ),
                            column(3,
                                   helpText("Select a single or multiple PIT-tags of interest:"),
                                   uiOutput("tagid_menu")
                                  ),
                            column(6,
                                   plotOutput("mig_plot", width = "100%")
                                  )
                          )
                      ),
                      hr(),
                      tabsetPanel(
                      tabPanel("Data Summary",
                               fluidPage(
                                 fluidRow(
                                   column(12,
                                          h2("Summary Plots of Unique Tags")),
                                   column(4, align = 'center',
                                          h3("Arrival date to a site:"),
                                          uiOutput("sized_arrival")),
                                   column(4, align = 'center',
                                          h3("Current Tag Status based on last detection:"),
                                          uiOutput("sized_nomove")),
                                   column(4, align = 'center',
                                          h3("Travel time between sites:"),
                                          uiOutput("sized_travel_1_3"))
                                          #uiOutput("sized_travel_3_4"),
                                          #uiOutput("sized_travel_4_5"))
                                 ),
                                 fluidRow(
                                  column(12,
                                         h2("Summary Counts of Unique Tags")
                                         ),
                                  column(4, align = 'center',
                                         h3("By origin:"),
                                         tableOutput("unique_tags"),
                                         h3("By passage route:"),
                                         tableOutput("passage_route"),
                                         h3("Passage rate:"),
                                         tableOutput("passage_rate")
                                         ), # close column
                                  column(4, align = 'center',
                                         h3("By origin and release site:"),
                                         tableOutput("unique_tags_site"),
                                         h3("By trap status:"),
                                         tableOutput("trap_status")
                                  ), # close column
                                  column(4, align = 'center',
                                         h3("By origin and current tag fate:"),
                                         tableOutput("tag_fate")
                                  ) # close column
                                 ),
                                fluidRow(
                                  column(12,
                                         h2("Estimated Tags")),
                                  column(12, align = 'center',
                                         h3("Detection efficiencies and estimated tags by node:"),
                                         tableOutput("est_tags_node"),
                                         h3("Detection efficiencies and estimated tags by site:"),
                                         tableOutput("est_tags_site"))
                                )
                               ) # closes fluid page for summary panel
                               ), # closes summary panel
                      tabPanel("Raw Data",
                               fluidPage(
                                 fluidRow(
                                   column(4,
                                          h3("Export Raw PITtrackR Data"),
                                          helpText("Select the dataset you wish to export:")),
                                   column(4,radioButtons('rawdata', h3("Dataset:"), inline = TRUE,
                                                       choiceNames = c('PITcleanR Processed', "Detection History", "Raw Chinook", "Raw Bull Trout"),
                                                       choiceValues = c('dat_all', 'detect_hist', 'raw_chs', 'raw_bull'))),
                                   column(2, offset = 2,
                                          h3("Download:"),
                                          downloadButton("data_export", label = "Export .CSV File", class = "mybutton"))
                                 ), # close first fluidrow
                                 fluidRow(
                                   column(12,
                                    DT::dataTableOutput("raw_data"))
                                   )# close fluidRow
                               ) # close fluid page for raw data
                            ), # close raw data tabPanel
                      # tabPanel("Pictures",
                      #          fluidPage(
                      #            fluidRow(
                      #              column(6,
                      #                     h3("Imnaha weir sill plate on June 13th"),
                      #                     img(src="0613181020_HDR.JPG", height = 400)),
                      #              column(6,
                      #                     h3("Looking downstream at IR4 on June 13th"),
                      #                     img(src="0613181028a.JPG", height = 400))
                      #            ), # close first fluid row for pics
                      #           fluidRow(
                      #              column(6,
                      #                     h3("Looking upstream at IR5 on June 13th"),
                      #                     img(src="0613181023_HDR.JPG", height = 400)),
                      #              column(6,
                      #                     h3("Imnaha ladder entrance on June 13th"),
                      #                     img(src="0613181023_ladder.JPG", height = 800))
                      #            ) # close fluid row for pics
                      #          ) # close fluidpage for pictures
                      #   ), # close tabPanel for pictures
                      tabPanel("Citations",
                               fluidPage(
                                 fluidRow(
                                   column(12,
                                          h3("Acknowledgements"),
                                          align = "center"
                                   )
                                 ),
                                 fluidRow(
                                   column(12,
                                          p("The information presented in the PITtrackR web application is the product of 
                                            a collaborative effort between Joseph Feldhaus, Tim Whitsel, Paul Sankovich
                                            and Ryan Kinzer. The information is intended to assist Imnaha River fisheries
                                            managers in making operational decisions for the Imnaha River weir using near real-time
                                            PIT-tag detections.  PITtrackR uses PIT-tag data that is collected and managed by
                                            multiple tribes, states and federal agencies and made available from PTAGIS.
                                            Without everyone's generous policies regarding public and regional
                                            PIT-tag data sharing this application would not be possible. We would like to
                                            individually thank all of the people involved with the data presented on this site,
                                            however, the list of contributors is too numerous to identify everyone by name.
                                            If we have used data that you are personally connected with, thank you!  And if
                                            you feel any data presented here is portrayed incorrectly, or if you would like
                                            the information you are connected with removed from this application please
                                            contact us to discuss.")
                                          )
                                          ),
                                 hr(),
                                 fluidRow(
                                   column(12,
                                          h3("Contacts"),
                                          align = "center"
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(6,
                                          h4("Ryan Kinzer"), 
                                          h4("Nez Perce Tribe"),
                                          h4("Department of Fisheries Resources Management"),
                                          h4("208-640-5290"),
                                          h4("ryank@nezperce.org"),
                                          align = "center"
                                          ),
                                   column(6,
                                          h4("Joseph Feldhaus"),
                                          h4("Oregon Department of Fish and Wildlife"),
                                          h4("Northeast Region Fish Research"),                                          
                                          h4("541-962-3724"),
                                          h4("Joseph.Feldhaus@state.or.us"),
                                          align = "center"
                                        )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(12,
                                          h3("Data Sources, R Packages and References"),
                                          align = "center"
                                   )
                                 ),
                                 hr(),
                                 fluidRow(
                                   column(12,
                                          p("Kevin See, Ryan N. Kinzer, Rick Orme and Mike Ackerman. PITcleanr: Cleans up PIT tag capture histories for DABOM. R package version 0.0.0.9000.
                                            https://github.org/kevinsee/PITcleanR"),
                                          p("Hadley Wickham (2017). tidyverse: Easily Install and Load the 'Tidyverse'. R package version 1.2.1.
                                            https://CRAN.R-project.org/package=tidyverse"),
                                          p("  Garrett Grolemund, Hadley Wickham (2011). Dates and Times Made Easy with lubridate. Journal of Statistical Software, 40(3), 1-25. URL
                                            http://www.jstatsoft.org/v40/i03/."),
                                          p("Winston Chang, Joe Cheng, JJ Allaire, Yihui Xie and Jonathan McPherson (2017). shiny: Web Application Framework for R. R package version
                                            1.0.5. https://CRAN.R-project.org/package=shiny"),
                                          p("Joe Cheng, Bhaskar Karambelkar and Yihui Xie (2017). leaflet: Create Interactive Web Maps with the JavaScript 'Leaflet' Library. R package
                                            version 1.1.0. https://CRAN.R-project.org/package=leaflet"),
                                          p(paste0("PTAGIS. The Columbia Basin PIT Tag Information System. Accessed through ftp server site on ", Sys.Date(),"."))
                                          ) # close column
                                          ) # close fluidRow
                                 ) # close fluidPage
                               ) # close tabPanel for citation
                      ) # close tabsetPanel
               ) # close tabPanel PITtrackR
  ) # close navbarPage
) # close server
