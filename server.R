# Ryan N. Kinzer
# Modified: 5/31/19
# Web application for tracking PIT-tagged Chinook salmon and Bull trout in the Snake River.

# Load Packages, Scripts and Data ----
library(shiny)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(scales)
library(viridis)

#source('scripts/nodeEfficiency.R')
#source('scripts/nodeDetectionEvent.R')
source('scripts/estimateSpawnLoc.R')
source('scripts/aws_keys.R')
#load("data/config_data.rda")

# Server Logic ----
shinyServer(function(input, output) {

# Set amazon pass codes

  setKeys()
   

# Load PIT Data ----
# Loads processed data from Amazon and subsets by selected species
# Data loads from Amazon S3 bucket after year is selected
  
  #observeEvent(input$pit_year,{
  dat_all <- reactive({

         
      aws.s3::s3read_using(FUN = read_csv,
                                  bucket = "nptfisheries-pittracking",
                                  object = paste0("PITcleanr_", input$pit_year ,"_chs_bull"))

      # detect_hist <- aws.s3::s3read_using(FUN = read_csv,
      #                                 bucket = "nptfisheries-pittracking",
      #                                 object = paste0("detection_history_", input$pit_year))
      # 
      # raw_chs <- aws.s3::s3read_using(FUN = read_csv,
      #                              bucket = "nptfisheries-pittracking",
      #                              object = paste0("PITtrackR_Chinook_Complete_Tag_History_", input$pit_year))
      # 
      # raw_bull <- aws.s3::s3read_using(FUN = read_csv,
      #                              bucket = "nptfisheries-pittracking",
      #                              object = paste0("PITtrackR_Bull_Complete_Tag_History_", input$pit_year))
   })
   

   # subset datasets based on the species selected with the radio toggle
   tmp_dat <- reactive({
      dat_all() %>%
         filter(Mark.Species == input$pit_spp) # will need to add stream name to dataset if we were to add more watersheds, also year?
   })
   
   # tmp_detect_hist <- reactive({
   #    
   #    detect_hist[detect_hist$Mark.Species == input$pit_spp,]
   #    
   # })  
   
   watersheds <- reactive({
      w <- unique(tmp_dat()$Group)
      w[!is.na(w)]
   })
   
   output$watershed_menu <- renderUI({
      selectInput('watershed', h3("Watershed:"),
                  choices = watersheds(),
                  selected = 'ImnahaRiver',
                  multiple = FALSE
                  #size = 5
                  #c("Choose one" = "","Imnaha River" = "Imnaha River")
                  )                                  
   })
   
   dat <- reactive({
      tmp_dat() %>%
         filter(Group == input$watershed)
   })
   
# Tag Migration Plot ----
   
   # Get unique tag_ids for inputUI selection
   tag_ids <- reactive({
      dat() %>%
         distinct(TagID) %>%
         pull()
   })
   
   output$tagid_menu <- renderUI({
      selectInput('tag_id', h3("Unique Tag IDs:"), tag_ids(), multiple=TRUE, selectize=FALSE, size = 20
                  # selected = c("3DD.00775FB8E4","3DD.00777AEC3F","3DD.00777B19A0","3DD.0077AE880C",
                  #              "3D9.1C2DE10CEE", "3DD.007771C738", "3DD.0077725D55", "3DD.007772BBC9")
                  )
   })
   
   # above_weir <- data.frame(firstObsDateTime = c(as.numeric(ymd_hms("20180611 15:00:00")), as.numeric(Sys.Date())),
   #                          Node = c("IMNAHW", "IR5A0"))
   
   
   # Plot migration patterns of selected tag ids

   selected_tag_ids <- reactive({
      input$tag_id
   })
   
   
   output$mig_plot <- renderPlot({

      if(is.null(input$tag_id)){
         return()
      } else {
      lims <- c(min(dat()$firstObsDateTime), Sys.time())
      tmp_tag <- selected_tag_ids()

      axis_labs <- dat() %>%
         distinct(Node, RKMTotal) %>%
         mutate(RKMTotal = case_when(
            grepl('B0', Node) ~ RKMTotal - 2,
            grepl('AO', Node) ~ RKMTotal + 2,
            TRUE ~ RKMTotal))

      dat() %>%
         #dat %>%
         filter(TagID %in% tmp_tag) %>%
         ggplot(aes(x = firstObsDateTime, y = RKMTotal, group = TagID, colour = Release.Site.Code), drop = TRUE) +
         #geom_polygon(data = above_weir) +
         geom_line() +
         geom_point() +
         #geom_vline(xintercept = as.numeric(ymd_hms("20180611 15:00:00")), linetype=2)+
         scale_x_datetime(limits = lims, labels = date_format("%d-%b")) +
         scale_y_continuous(breaks = axis_labs$RKMTotal, labels = axis_labs$Node) +
         #scale_y_discrete(drop = FALSE) +
         #scale_colour_manual(values = c("Chinook" = "blue", "Bull Trout" = "red")) +
         scale_colour_brewer(palette = 'Set1') +
         #facet_wrap(~TagID) +
         theme_bw() +
         theme(legend.position = 'bottom',
               text = element_text(size = 18)) +
         labs(x = 'Observation Date',
              y = 'Node',
              colour = 'Release Site')
      }
   }, height = 475, width = 700)
   

   
# Order Sites and Nodes ----
# Arrange the factor levels for fish traveling upstream in the selected branch
   # node_vec <- dat_all %>%
   #    distinct(Node, NodeOrder) %>%
   #    arrange(NodeOrder, Node) %>%
   #    pull(Node)
   # 
   # site_vec <- dat_all %>%
   #    select(SiteID, NodeOrder) %>%
   #    group_by(SiteID) %>%
   #    slice(which.min(NodeOrder)) %>%
   #    arrange(NodeOrder, SiteID) %>%
   #    pull(SiteID)
   # 
   # dat_all <- dat_all %>%
   #    mutate(Node = fct_relevel(Node, node_vec),
   #           Node = fct_relevel(Node, c("COCB0", "COCA0", "IR1", "IR2", "BSCB0", "BSCA0")),
   #           SiteID = fct_relevel(SiteID, site_vec))
   # 
   # node_vec <- levels(dat_all$Node)
   # site_vec <- levels(dat_all$SiteID)
   

# Create Summary Plots ---- 
  #  output$arrival_plot <- renderPlot({
  #    tmp_dat() %>%
  #      #tmp_dat %>%
  #      #filter(!SiteID%in%c("BSC","COC")) %>%
  #      group_by(TagID, Mark.Species, Origin, Release.Site.Code, SiteID) %>%
  #      slice(which.min(lastObsDateTime)) %>%
  #      ggplot(aes(x=lastObsDateTime, fill=Origin)) +
  #      geom_histogram(colour = 'black', bins = 100) +
  #      geom_vline(xintercept = as.numeric(ymd_hms("20180611 15:00:00")), linetype=2)+
  #      scale_fill_brewer(palette = 'Set1') +
  #      #scale_fill_viridis(discrete = TRUE) +
  #      #facet_grid(factor(SiteID,levels=c("IR1","IR2","IR3","IR4","IML","IR5"))~Mark.Species,scales="free_y") +
  #      facet_grid(SiteID ~ .) +
  #      theme_bw() +
  #      theme(panel.grid.major=element_blank())+
  #      theme(legend.position = 'bottom') +
  #      labs(x = "First detection date",
  #           y = "Count")
  #  })
  #  
  #  # output$nomovement_plot <- renderPlot({
  #  #   #tmp_dat() %>%
  #  #     tmp_dat %>%
  #  #     #filter(!SiteID %in% c('COC','BSC')) %>%
  #  #     #mutate(SiteID = factor(SiteID,levels=c("IR1","IR2","IR3","IR4","IML","IR5"))) %>%
  #  #     group_by(Mark.Species, Origin, SiteID, TagID) %>%
  #  #     summarise(minObsDate = min(lastObsDateTime),
  #  #               maxObsDate = max(lastObsDateTime),
  #  #               hours = difftime(maxObsDate, minObsDate, units = 'days')) %>%
  #  #     ggplot(aes(x = hours, fill = Origin)) +
  #  #     geom_histogram(colour = 'black', bins = 100) +
  #  #     scale_fill_brewer(palette = 'Set1') +
  #  #     facet_grid(SiteID ~ Mark.Species) +
  #  #     theme_bw() +
  #  #     theme(panel.grid.major=element_blank())+
  #  #     theme(legend.position = 'bottom') +
  #  #     labs(x = 'Days (max(obsDate) - min(obsDate))',
  #  #          y = 'Count') 
  #  # })
  #  
  #  
  #  output$nomovement_plot <- renderPlot({
  # 
  #   lims <- c(ymd("20180501"), Sys.Date())
  #    
  #   tmp_detect_hist() %>%
  #   #tmp_detect_hist %>%
  #      filter(str_detect(TagPath,"IR4|IR5|IML|IMNAHW"))%>%
  #      filter(LastObs >= ymd(20180601)) %>%
  #      mutate(LastObs = as.Date(LastObs,format="%m/%d/%Y")) %>%
  #      ggplot(aes(x = LastObs, fill = TagStatus))+
  #      geom_histogram(colour = 'black', binwidth=1) +
  #     geom_vline(xintercept = as.numeric(as.Date("2018-06-11")), linetype=2)+
  #     facet_wrap(~TrapStatus,scales="free_y",ncol=1)+
  #     scale_fill_brewer(palette = 'Set1') +
  #     #scale_fill_viridis(discrete = TRUE) +
  #     scale_x_date(limits = lims) +
  #     theme_bw() +
  #     theme(legend.position = 'bottom')+
  #     guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  #     theme(panel.grid.major=element_blank())+
  #     labs(x = "Last Observation Date",
  #          y = "Count")
  #    
  #  })
  #  
  #  output$traveltime_plot_IR1_IR3 <- renderPlot({
  #    
  #     tmp_detect_hist() %>%
  #      #tmp_detect_hist %>%
  #      select(TagID, Mark.Species, Origin, Arrival_Month, IR4_IML:IR4_IR5) %>%
  #      #mutate_at(c(5:8), as.numeric) %>%
  #      gather(Reach, Travel_Time, IR4_IML:IR4_IR5) %>%
  #      mutate(Reach = fct_relevel(Reach, c("IR4_IML", "IML_IMNAHW", "IR4_IMNAHW", "IR4_IR5")),
  #             Arrival_Month = fct_relevel(Arrival_Month, c("June", "July"))) %>%
  #      filter(Travel_Time >= 0) %>%
  #      #filter(Reach == "IR4_IR5") %>%
  #      ggplot(aes(x = Travel_Time, fill = Origin)) +
  #      geom_histogram(colour = 'black', binwidth = 1) +
  #      scale_fill_brewer(palette = 'Set1') +
  #      facet_grid(Reach ~ Arrival_Month, scales = "free") +
  #      theme_bw() +
  #      theme(legend.position = 'bottom') +
  #      labs(x = 'Travel Time (Days)',
  #           y = 'Count') 
  #  })
  #  
  #  # output$traveltime_plot_IR3_IR4 <- renderPlot({
  #  #   
  #  #   tmp_detect_hist() %>%
  #  #     #tmp_detect_hist %>%
  #  #     select(TagID, Mark.Species, Origin, arrival_date = IR3, Travel_Time = IR3_IR4) %>%
  #  #     mutate_at(5, as.numeric) %>%
  #  #     mutate(Arrival_Month = month(arrival_date, label = TRUE, abbr = FALSE)) %>%
  #  #     filter(Travel_Time >= 0) %>%
  #  #     ggplot(aes(x = Travel_Time, fill = Arrival_Month)) +
  #  #     geom_histogram(colour = 'black', bins = 100) +
  #  #     scale_fill_brewer(palette = 'Set1') +
  #  #     #scale_fill_viridis(discrete = TRUE) +
  #  #     #facet_wrap( ~ Arrival_Month, scales = 'free', ncol = 2) +
  #  #     theme_bw() +
  #  #     theme(panel.grid.major=element_blank())+
  #  #     theme(legend.position = 'bottom') +
  #  #     labs(x = 'Travel Time (Days)',
  #  #          y = 'Count',
  #  #          title = "IR3 to IR4") 
  #  # })
  #  # 
  #  # output$traveltime_plot_IR4_IR5 <- renderPlot({
  #  #   
  #  #   tmp_detect_hist() %>%
  #  #     #tmp_detect_hist %>%
  #  #     select(TagID, Mark.Species, Origin, arrival_date = IR4, Travel_Time = IR4_IR5) %>%
  #  #     mutate_at(5, as.numeric) %>%
  #  #     mutate(Arrival_Month = month(arrival_date, label = TRUE, abbr = FALSE)) %>%
  #  #     filter(Travel_Time >= 0) %>%
  #  #     ggplot(aes(x = Travel_Time, fill = Origin)) +
  #  #     geom_histogram(colour = 'black', bins = 100) +
  #  #     scale_fill_brewer(palette = 'Set1') +
  #  #     #scale_fill_viridis(discrete = TRUE) +
  #  #     facet_wrap( ~ Arrival_Month, scales = 'free', ncol = 2) +
  #  #     theme_bw() +
  #  #     theme(panel.grid.major=element_blank())+
  #  #     theme(legend.position = 'bottom') +
  #  #     labs(x = 'Travel Time (Days)',
  #  #          y = 'Count',
  #  #          title = "IR4 to IR5") 
  #  # })
  #  
  #  output$sized_arrival <- renderUI({
  #    plotOutput("arrival_plot", height = 600)
  #  })   
  #  
  #  output$sized_nomove <- renderUI({
  #    plotOutput("nomovement_plot", height = 600)
  #  })
  # 
  # 
  #  output$sized_travel_1_3 <- renderUI({
  #    plotOutput("traveltime_plot_IR1_IR3", height = 600)
  #  })
  #  
  #  # output$sized_travel_3_4 <- renderUI({
  #  #   plotOutput("traveltime_plot_IR3_IR4", height = 200)
  #  # })
  #  #   
  #  # output$sized_travel_4_5 <- renderUI({
  #  #   plotOutput("traveltime_plot_IR4_IR5", height = 200)
  #  # })
  #  
  #  #------------------------------------------------
  #  # Create Summary Tables
  #  #------------------------------------------------
  # 
  #  # unique detections
  #  
  #  output$unique_tags <- renderTable({
  #  tmp_dat() %>%
  #    group_by(Mark.Species, Origin) %>%
  #    summarise(Unique_Tags = n_distinct(TagID)) %>%
  #      rename(Species = Mark.Species, `Unique Tags` = Unique_Tags)
  #  })
  #  
  #  output$unique_tags_site <- renderTable({
  #  tmp_dat() %>%
  #    mutate(Release.Year = year(Release.Date)) %>%
  #    group_by(Mark.Species, Origin, Release.Site.Code) %>%
  #    summarise(Unique_Tags = n_distinct(TagID)) %>%
  #    rename(Species = Mark.Species, `Unique Tags` = Unique_Tags, `Release Site` = Release.Site.Code)  
  #  })
  #  
  #  output$tag_fate <- renderTable({
  #    tmp_detect_hist() %>%
  #      group_by(Mark.Species, Origin, TagStatus) %>%
  #      summarise(Unique_Tags = n()) %>%
  #      rename(Species = Mark.Species, `Unique Tags` = Unique_Tags)
  #  })
  #  
  #  output$passage_route <- renderTable({
  #    tmp_detect_hist() %>%
  #      filter(grepl('Passed', TagStatus),
  #             NewTag == 'False') %>%
  #      group_by(Mark.Species, PassageRoute, TrapStatus) %>%
  #      summarise(Unique_tags = n()) %>%
  #      rename(Species = Mark.Species, `Trap Status` = TrapStatus, `Unique Tags` = Unique_tags)
  #  })
  #  
  #  output$passage_rate <- renderTable({
  #    tmp_detect_hist() %>%
  #      filter(NewTag == "False",
  #             Mark.Species == "Bull Trout",
  #             TagStatus %in% c("At Weir","Attempted Ladder","Passed","Passed: <11 June","Trapped")) %>%
  #      mutate(Passed = ifelse(TagStatus %in% c("Passed","Passed: <11 June","Trapped"),"True","False")) %>%
  #      group_by(Mark.Species, NewTag, Passed, TagStatus) %>%
  #      summarise(Unique_tags = n()) %>%
  #      select(Species = Mark.Species, `New Tag` = NewTag, Passed, `Tag Status` = TagStatus, `Unique Tags` = Unique_tags)
  #  })  
  #  
  #  
  #  output$trap_status <- renderTable({
  #    tmp_detect_hist() %>%
  #      select(Mark.Species, TrapStatus, IR4, IML, IMNAHW, IR5) %>%
  #      gather(SiteID, firstObsDateTime, IR4:IR5, na.rm = TRUE) %>%
  #      group_by(Mark.Species, TrapStatus, SiteID) %>%
  #      summarise(Unique_tags = n()) %>%
  #      rename(Species = Mark.Species, `Trap Status` = TrapStatus, `Unique Tags` = Unique_tags)
  #  })
  #  
  #  
  #  #------------------------------------------------
  #  # Estimated tags
  #  #------------------------------------------------ 
  # 
  #  output$est_tags_node <- renderTable({
  #    tmp_df <- tmp_dat() %>%
  #      filter(!Node%in%c("COCA0","COCB0","BSCA0","BSCB0", "IMNAHR")) %>%
  #      anti_join(detect_hist %>%
  #                  filter(NewTag == 'True') %>%
  #                  select(TagID)
  #      ) %>%
  #      #mutate(Node = gsub("A0", "B0", Node)) %>%# calculates efficiency at the site level
  #      identity()
  #    
  #    tmp_df %>%
  #      #mutate(Obs_location = ifelse(grepl("IR", Node), "Instream", "Ladder")) %>%
  #      #group_by(Mark.Species, Obs_location) %>%
  #      group_by(Mark.Species) %>%
  #      do(nodeEfficiency(.,direction="Upstream")) %>%
  #      arrange(Mark.Species, NodeOrder) %>%
  #      mutate(det_eff = Recaps/Marks, #ifelse(!grepl('IMNAHW', Node), Recaps/Marks, 1.00),
  #             se_eff = (1/Marks^2)*(Marks*det_eff)*(1-det_eff),
  #             N_tags = Unique_tags/det_eff,
  #             se_N = sqrt(((Marks+1)*(Unique_tags+1)*(Marks - Recaps) * (Unique_tags - Recaps))/ ((Recaps+1)^2 * (Recaps+2))),
  #             lwr_N = N_tags - 1.96*se_N,
  #             upr_N = N_tags + 1.96*se_N,
  #             N_up = lag(N_tags),
  #             Conversion = ifelse(N_tags/N_up*100 > 100, 100 , round(N_tags/N_up*100)),
  #             Conversion = paste0(Conversion,"%")
  #             #Node = gsub("B0", "", Node)
  #      ) %>%
  #      ungroup() %>%
  #      select(Species = Mark.Species, Node, `Unique Tags` = Unique_tags, Marks, Recaps,
  #             `Detection Efficiency` = det_eff, `Estimated Tags` = N_tags,
  #             `Lower 95CI` = lwr_N, `Upper 95CI` = upr_N)
  #  })
  #  
  #  # estimated det. eff. by site
  #  output$est_tags_site <- renderTable({
  #    tmp_df <- tmp_dat() %>%
  #      filter(!Node%in%c("COCA0","COCB0","BSCA0","BSCB0", "IMNAHR")) %>%
  #      anti_join(detect_hist %>%
  #                  filter(NewTag == 'True') %>%
  #                  select(TagID)
  #      ) %>%
  #      mutate(Node = gsub("A0", "B0", Node)) %>% # calculates efficiency at the site level
  #      identity()
  #    
  #    tmp_df %>%
  #      #mutate(Obs_location = ifelse(grepl("IR", Node), "Instream", "Ladder")) %>%
  #      #group_by(Mark.Species, Obs_location) %>%
  #      group_by(Mark.Species) %>%
  #      do(nodeEfficiency(.,direction="Upstream")) %>%
  #      arrange(Mark.Species, NodeOrder) %>%
  #      mutate(det_eff = Recaps/Marks, #ifelse(!grepl('IMNAHW', Node), Recaps/Marks, 1.00),
  #             se_eff = (1/Marks^2)*(Marks*det_eff)*(1-det_eff),
  #             N_tags = Unique_tags/det_eff,
  #             se_N = sqrt(((Marks+1)*(Unique_tags+1)*(Marks - Recaps) * (Unique_tags - Recaps))/ ((Recaps+1)^2 * (Recaps+2))),
  #             lwr_N = N_tags - 1.96*se_N,
  #             upr_N = N_tags + 1.96*se_N,
  #             N_up = lag(N_tags),
  #             Conversion = ifelse(N_tags/N_up*100 > 100, 100 , round(N_tags/N_up*100)),
  #             Conversion = paste0(Conversion,"%"),
  #             SiteID = gsub("B0", "", Node)
  #      ) %>%
  #      ungroup() %>%
  #      select(Species = Mark.Species, SiteID, `Unique Tags` = Unique_tags, Marks, Recaps,
  #             `Detection Efficiency` = det_eff, `Estimated Tags` = N_tags,
  #             `Lower 95CI` = lwr_N, `Upper 95CI` = upr_N) %>%
  #      filter(SiteID != 'IR5')
  #  })
  #  
  #  
  # 
  # #--------------------------------
  # # Raw data
  # #--------------------------------
  # 
  # rawdat <- reactive({input$rawdata})
  #  
  # export_dat <- reactive({
  #   
  #   if(rawdat() == 'dat_all'){
  #     dat_all
  #   } else
  #   
  #   if(rawdat() == 'raw_chs'){
  #     raw_chs
  #   } else
  #     
  #   if(rawdat() == 'raw_bull'){
  #     raw_bull
  #   } else
  #     
  #   if(rawdat() == 'detect_hist'){
  #     detect_hist
  #   }
  # })
  # 
  # output$raw_data <- DT::renderDataTable({
  #   export_dat()
  # })
  # 
  # # function for downloading data
  # output$data_export <- downloadHandler(  #output name needs to match ui object id name
  #   
  #   #tmp_export <- export_dat()
  #   
  #   filename = function() {
  #     paste0(rawdat(),"_", Sys.Date(), "_.csv")
  #   },
  #   content = function(filename) {
  #     write.csv(export_dat(), filename, row.names = FALSE)
  #   }
  # )

})
