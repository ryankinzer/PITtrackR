# Server Logic ----
function(input, output) {

  output$loaded <- renderText({
    if(input$loadData == 0) {
      paste("Select and load data!")
      #paste("<font color=\"#FF0000\"><b> Select and load data! </b></font>")
    } else {
      return()
    }
  })

# Load PIT and Window Data ----

  window_df <- eventReactive(input$loadData, {
    
    if(input$basin_spp == 'Chinook'){spp <- c('fc', 'fcj')}
    #if(input$basin_spp == 'Coho'){spp <- c('fk', 'fkj')}
    #if(input$basin_spp == 'Steelhead'){spp <- c('fs', 'fsw')}
    
    queryWindowCnts(dam = 'LWG', #input$hydro_locs,
                    spp_code = spp, #c('fc', 'fcj', 'fk', 'fkj', 'fs', 'fsw'),
                    spawn_yr = input$rtn_year,
                    start_day = '01/01',
                    end_day = '12/31') %>%
      #select(Year, Date, Chinook, Jack_Chinook, Coho, Jack_Coho, Steelhead, Wild_Steelhead) %>%
      gather(key = spp, value = 'Count', -Year, -Date) %>%
      mutate(spp = gsub("_"," ",spp))#Chinook:Wild_Steelhead)
  })
  
  # Loads processed data from Amazon and subsets by selected species
  # Data loads from Amazon S3 bucket after year is selected
  
  dat_all <- eventReactive(input$loadData, {
    
    #withBusyIndicatorServer("loadData", {
  aws.s3::s3read_using(FUN = read_csv,
                         bucket = "nptfisheries-pittracking",
                         object = paste0("PITcleanr_",
                                         input$rtn_year,
                                         #2019,
                                         "_chs_bull")) %>%
      left_join(site_loc, by = 'SiteID')
    # })
  }) 
  
  
# Lower Granite Tab ----
  output$windowCnts_grp1 <- renderValueBox({

    n <- window_df() %>%
      filter(spp == input$basin_spp) %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    if(input$basin_spp!= 'Steelhead'){    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'blue',
      icon = icon("fish"),
      subtitle = paste0("Adult ",input$basin_spp, " Window Count")
      )
    } else {
      valueBox(
        value = prettyNum(n, big.mark = ","),
        color = 'blue',
        icon = icon("fish"),
        subtitle = paste0(input$basin_spp, " Window Count")
      )
    }
  })
  
  output$windowCnts_grp2 <- renderValueBox({
    
    if(input$basin_spp!= 'Steelhead'){
    n <- window_df() %>%
      filter(spp == paste0("Jack ",input$basin_spp)) %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'green',
      icon = icon("fish"),
      subtitle = paste0("Jack ", input$basin_spp, " Window Count")
      )
    } else {
      n <- window_df() %>%
        filter(spp == paste0("Wild_",input$basin_spp)) %>%
        summarise(n = sum(Count)) %>%
        pull(n)
      
      valueBox(
        value = prettyNum(n, big.mark = ","),
        color = 'green',
        icon = icon("fish"),
        subtitle = paste0("Wild ", input$basin_spp, " Window Count")
      )
    }
  })
  
  
  output$release_grp <- renderPlot({
    dat_all() %>%
      #dat_all %>%
      filter(SiteID == "GRA") %>%
      filter(grepl(input$basin_spp, Mark.Species)) %>%
      filter(!is.na(Release.Site.Code)) %>%
      filter(Release.Site.Code != 'LGRLDR') %>%
      mutate(Release.Year = year(Release.Date)) %>%
      group_by(Mark.Species, Origin, Release.Site.Code, Release.Year) %>%
      summarise(n = n_distinct(TagID)) %>%
      ggplot(aes(x = Release.Site.Code, y = n, fill = as.factor(Release.Year))) +
      geom_col(colour = 'black') +
      coord_flip() +
      facet_wrap(~Origin, scales = 'free') +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Observed Tags',
           y = 'Release Site',
           fill = 'Release Year') +
      theme_bw() +
      theme(legend.position = 'bottom')
  })
  
  
  output$window_plot <- renderPlot({
    window_df() %>%
      filter(grepl(input$basin_spp, spp)) %>%
      filter(Count != 0) %>%
      group_by(spp) %>%
      mutate(cumsum = cumsum(Count)) %>%
      ggplot() +
      #geom_line(aes(x = Date, y = cumsum, colour = spp), size = 2) +
      #geom_col(aes(x = Date, y = Count, fill = spp), colour = 'black') +
      geom_area(aes(x = Date, y = Count, fill = spp), colour = 'black') +
      scale_fill_brewer(palette = 'Paired')+
      scale_colour_brewer(palette = 'Set2')+
      facet_wrap(~spp, ncol = 1) +
      labs(x = "Arrival Date",
           y = "Window Count",
           colour = '',
           fill = '') +
      theme_bw() +
      theme(legend.position = 'bottom')
  })
  
  
  output$gra_tagged <- renderPlot({
    dat_all() %>%
      #dat_all %>%
      filter(SiteID == "GRA") %>%
      filter(grepl(input$basin_spp, Mark.Species)) %>%
      filter(Release.Site.Code == 'LGRLDR') %>%
      mutate(Release.Year = year(Release.Date)) %>%
      group_by(Mark.Species, Origin, Release.Site.Code, Release.Year) %>%
      summarise(n = n_distinct(TagID)) %>%
      ggplot(aes(x = Origin, y = n, fill = Origin)) +
      geom_col(colour = 'black') +
      #facet_wrap(~Origin, scales = 'free') +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Release Site',
          y = 'Observed Tags',
          fill = '') +
      theme_bw() +
      theme(legend.position = 'none')
  })
  
  output$release_plot <- renderPlot({
    dat_all() %>%
     #dat_all %>%
      filter(SiteID == "GRA") %>%
      filter(grepl(input$basin_spp, Mark.Species)) %>%
      filter(grepl('Chinook', Mark.Species)) %>%
      filter(Release.Site.Code != 'LGRLDR') %>%
      filter(Origin == 'Hat') %>%
      mutate(Release.Year = year(Release.Date),
             ObsDate = as.Date(firstObsDateTime)) %>%
      group_by(Release.Site.Code, Release.Year, Origin, ObsDate) %>%
      summarise(n = n_distinct(TagID)) %>%
      ggplot(aes(x = ObsDate, y = n, fill = as.factor(Release.Year))) +
      geom_col(colour = 'black') +
      #scale_x_datetime(labels = date_format("%d-%m"), breaks = "1 week") +
      facet_wrap( ~ Release.Site.Code, ncol = 3, drop = TRUE) +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Arrival Date',
           y = 'Observed Tags',
           fill = 'Release Year') +
      theme_bw() +
      theme(legend.position = 'bottom')
  })
  

# Snake Basin Tab ----
  # Create Basin Summary Plots/Tables ----   
  # use spp_dat()
  
  output$tagsGRA <- renderValueBox({
    n <- dat_all() %>%
      filter(SiteID == "GRA") %>%
      filter(grepl(input$basin_spp,Mark.Species)) %>%
      mutate(n = n_distinct(TagID)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'aqua',
      subtitle = "Released Granite Tags"
    )
  })
  
  output$tagsObs <- renderValueBox({
    
    n <- dat_all() %>%
      filter(SiteID != "GRA") %>%
      filter(grepl(input$basin_spp, Mark.Species)) %>%
      mutate(n = n_distinct(TagID)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'green',
      subtitle = "Observed Granite Tags"
    )
  })
  
  output$tagsPercent <- renderValueBox({
    
    n <- dat_all() %>%
      filter(SiteID == "GRA") %>%
      filter(grepl(input$basin_spp, Mark.Species)) %>%
      mutate(n = n_distinct(TagID)) %>%
      pull(n)
    
    o <- dat_all() %>%
      filter(SiteID != "GRA") %>%
      filter(grepl(input$basin_spp, Mark.Species)) %>%
      mutate(n = n_distinct(TagID)) %>%
      pull(n)
    
    p <- paste0(round((o/n)*100),"%")
    
    valueBox(
      value = p,
      color = 'blue',
      subtitle = "Percent Observed"
    )
  })
  
  
  # Map Data ----
   map_dat <- eventReactive(input$loadData, {
     dat_all() %>%
       filter(Mark.Species == input$basin_spp) %>%
       group_by(Mark.Species, SiteID, Latitude, Longitude) %>%
       summarise(n = n_distinct(TagID)) %>%
       ungroup()
   })
   
   
   output$map <- renderLeaflet({

      leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
         setView(lat = 45.8,
                 lng = -116.1,
                 zoom = 7) %>%
         setMaxBounds(lng1 = -119,
                      lat1 = 42,
                      lng2 = -114,
                      lat2 = 49) %>%
         #addProviderTiles(providers$OpenStreetMap)
         addProviderTiles(providers$Esri.WorldTopoMap) %>%
         addScaleBar(position = 'topright') %>%
   
    addCircles(map_dat()$Longitude, map_dat()$Latitude, group = 'sites', radius = 100,
               color = 'black',
    label = map_dat()$SiteID,
    labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                direction = 'right',
                                style = list("color" = 'black', "font-weight" = "bold", "padding-left" = "10px")),
    popup = paste("<b>Site ID:</b>", map_dat()$SiteID, "<br>",
                  "<b>Unique Tags:</b>", map_dat()$n, "<br>"),
    popupOptions = popupOptions(noHide = T, textsize = "15px"),
    highlightOptions = highlightOptions(color = "white",
                                     weight = 5, bringToFront = F, opacity = 1))

     # addMarkers(map_dat()$Longitude, map_dat()$Latitude, group = 'sites',
     #            label = map_dat()$SiteID,
     #            labelOptions = labelOptions(noHide = T),
     #            popup = paste("<b>Site ID:</b>", map_dat()$SiteID, "<br>",
     #                          "<b>Unique Tags:</b>", map_dat()$n, "<br>"),
     #            popupOptions = popupOptions(noHide = TRUE, textsize = "15px"))
                
    # addPopups(map_dat()$Longitude, map_dat()$Latitude,
    #             paste("<b>Site ID:</b>", map_dat()$SiteID, "<br>",
    #                          "<b>Unique Tags:</b>", map_dat()$n, "<br>"))
      
   })
   
   # observeEvent(input$loadData, 
   #   ignoreNULL = FALSE, {
   #    
   #    leafletProxy("map") %>%
   #       clearGroup("sites") %>%
   # 
   # })
   
   
   output$site_tags <- DT::renderDT({
     dat_all() %>%
       filter(Mark.Species == input$basin_spp) %>%
     mutate(Group = ifelse(is.na(Group),'GRA', Group)) %>%
       group_by(Mark.Species, Origin, Group) %>%
       summarise(n = n_distinct(TagID)) %>%     
       select(Species = Mark.Species,
              Group,
              `Unique Tags` = n)
   })  
   
   
   output$basin_tag_plot <- renderPlot({
     dat_all() %>%
       filter(Mark.Species == input$basin_spp) %>%
     filter(!is.na(Group)) %>%
       distinct(Mark.Species, Origin, Group, SiteID, TagID) %>%
       ggplot(aes(x = SiteID, colour = Origin, fill = Origin)) +
       geom_bar(colour = 'black') +
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~Group, scales = 'free') +
       theme_bw() +
       theme(legend.position = 'bottom')
   })
   
   
   output$basin_travel <- renderPlot({
     
     tmp2 <- dat_all() %>%
       filter(Mark.Species == input$basin_spp) %>%
     arrange(TagID, firstObsDateTime) %>%
       mutate(Group = ifelse(is.na(Group),"GRA", Group)) %>%
       group_by(TagID,Origin, Group, SiteID) %>%
       summarise(minObsDate = min(firstObsDateTime)) %>%
       ungroup() %>%
       group_by(TagID) %>%
       mutate(dwnObsDate = lag(minObsDate),
              Reach = paste0(lag(Group)," - ", Group),
              TravelTime = difftime(minObsDate, dwnObsDate, units = 'days'))
     
     tmp2 %>%
       filter(!is.na(TravelTime),
              grepl("GRA - ",Reach))%>%
       ggplot(aes(x = TravelTime, fill = Origin)) +
       geom_histogram(colour = 'black')+
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~Reach) +
       labs(x = 'Travel Time (days)',
            y = 'Observed Tags') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
   
   
   # basin site detection efficiency
   output$basin_est_tags <- DT::renderDT({
     
     dat_all() %>%
       filter(Mark.Species == input$basin_spp) %>%
       estNodeEff(node_order = node_order) %>%
       left_join(node_order %>%
                   select(Node, Group, SiteID = NodeSite)) %>%
       filter(tagsAtNode != 0) %>%
       mutate(detEff = round(detEff,2)) %>%
       select(Group, SiteID, Node, `Unique Tags` = tagsAtNode,
              `Est. Tags` = estTagsAtNode, `Detection Efficiency` = detEff)
   })
   
   
   output$basin_mig_plot <- renderPlot({
     
     # axis_labs <- dat_all() %>%
     #   distinct(SiteID, RKMTotal) %>% #changed from Node
     #   mutate(rkmStart = min(RKMTotal),
     #          RiverRKM = RKMTotal - rkmStart,
     #          SiteID = paste0(SiteID," (",RiverRKM,")")) #changed from Node
     
     tag_basin <- dat_all() %>%
       slice(which.max(lastObsDateTime)) %>%
       select(TagID, loc = Group)
     
     dat_all() %>%
       filter(Mark.Species == input$basin_spp) %>%
       mutate(Group = ifelse(is.na(Group), 'GRA', Group)) %>%
       group_by(TagID, Group) %>%
       slice(which.min(firstObsDateTime)) %>%
       left_join(tag_basin, by = 'TagID') %>%
         ggplot(aes(x = firstObsDateTime, y = RKMTotal, group = TagID)) +
         geom_line() +
         #scale_color_brewer(palette = 'Set1') +
       labs(x = 'Observation Date',
            y = '(RKM)',
            colour = 'Sub-basin') +
         theme_bw() +
         theme(legend.position = 'bottom')
   })
   
# Watershed Summary ----
   
   watersheds <- reactive({
    w <- dat_all() %>%
          filter(Mark.Species == input$pit_spp) %>%
          distinct(Group) %>%
          pull()
     
      #w <- unique(spp_dat()$Group)
      w[!is.na(w)]
   })
   
   output$watershed_menu <- renderUI({
      selectInput('watershed', "Watershed:",
                  choices = watersheds(),
                  selected = 'ImnahaRiver',
                  multiple = FALSE
                  #size = 5
                  #c("Choose one" = "","Imnaha River" = "Imnaha River")
                  )                                  
   })
   
   
   # spp_dat <- eventReactive(input$pit_spp,{
   #   dat_all() %>%
   #     filter(Mark.Species == input$pit_spp)
   # })
   
   
   # Get unique tag_ids for selected watershed and species
   tag_ids <- reactive({
      dat_all() %>%
       filter(Mark.Species == input$pit_spp) %>%
       filter(Group == input$watershed) %>%
        distinct(TagID)# %>%
         #pull()
   })
   
   dat <- reactive({
     # inner_join(dat_all(),tag_ids(), by = 'TagID') %>%
        dat_all() %>%
        filter(Mark.Species == input$pit_spp) %>%
        filter(Group == input$watershed)
   })
   
   
   output$watershed_travel <- renderPlot({
    tmp2 <- dat() %>%
       filter(!is.na(Group)) %>%
       group_by(TagID, Origin, Group, SiteID) %>%
       summarise(minObsDate = min(firstObsDateTime)) %>%
       ungroup() %>%
       group_by(TagID) %>%
       mutate(dwnObsDate = lag(minObsDate),
              Reach = paste0(lag(SiteID)," - ", SiteID),
              TravelTime = difftime(minObsDate, dwnObsDate, units = 'days'))
     
     tmp2 %>%
       filter(!is.na(TravelTime))%>%
       ggplot(aes(x = TravelTime, fill = Origin)) +
       geom_histogram()+
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~Reach) +
       labs(x = 'Travel Time (days)',
            y = 'Observed Tags') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
   

   output$tagid_menu <- renderUI({
     
     tag_ids <- tag_ids() %>%
       pull()
     
     selectInput('tag_id', "Unique Tag IDs:", tag_ids, multiple=TRUE, selectize=FALSE, size = 5
     )
   })
   
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
         distinct(SiteID, RKMTotal) %>% #changed from Node
         mutate(rkmStart = min(RKMTotal),
                RiverRKM = RKMTotal - rkmStart,
           # RiverRKM = case_when(
           #  grepl('B0', Node) ~ RiverRKM - 2,
           #  grepl('AO', Node) ~ RiverRKM + 2,
           #  TRUE ~ RKMTotal),
           SiteID = paste0(SiteID," (",RiverRKM,")")) #changed from Node

      dat() %>%
         #dat %>%
         filter(TagID %in% tmp_tag) %>%
         filter(SiteID != 'GRA') %>%
        group_by(TagID, SiteID, Migration, Direction) %>% #new
        slice(which.min(firstObsDateTime)) %>% #new
         ggplot(aes(x = firstObsDateTime, y = RKMTotal, group = TagID, colour = Release.Site.Code), drop = TRUE) +
         #geom_polygon(data = above_weir) +
         geom_line() +
         geom_point() +
         #geom_vline(xintercept = as.numeric(ymd_hms("20180611 15:00:00")), linetype=2)+
         scale_x_datetime(limits = lims, labels = date_format("%d-%b")) +
         scale_y_continuous(breaks = axis_labs$RKMTotal, labels = axis_labs$SiteID) + #changed from Node
         #scale_y_discrete(drop = FALSE) +
         #scale_colour_manual(values = c("Chinook" = "blue", "Bull Trout" = "red")) +
         scale_colour_brewer(palette = 'Set1') +
         #facet_wrap(~TagID) +
         theme_bw() +
         theme(legend.position = 'bottom',
               text = element_text(size = 18)) +
         labs(x = 'Observation Date',
              y = 'SiteID (RKM)',
              colour = 'Release Site')
      }
   })#, height = 475, width = 700)
   
   

# Create Watershed Summary Plots ---- 
   output$arrival_plot <- renderPlot({
     dat() %>%
       group_by(TagID, Mark.Species, Origin, Release.Site.Code, SiteID) %>%
       slice(which.min(lastObsDateTime)) %>%
       ggplot(aes(x=lastObsDateTime, fill=Origin)) +
       geom_histogram(colour = 'black', bins = 100) +
       #geom_vline(xintercept = as.numeric(ymd_hms("20180611 15:00:00")), linetype=2)+
       scale_fill_brewer(palette = 'Set1') +
       #scale_fill_viridis(discrete = TRUE) +
       #facet_grid(factor(SiteID,levels=c("IR1","IR2","IR3","IR4","IML","IR5"))~Mark.Species,scales="free_y") +
       facet_grid(SiteID ~ .) +
       theme_bw() +
       theme(panel.grid.major=element_blank())+
       theme(legend.position = 'bottom') +
       labs(x = "First detection date",
            y = "Count")
   })
  #  
  #  # output$nomovement_plot <- renderPlot({
  #  #   #spp_dat() %>%
  #  #     spp_dat %>%
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
   # output$unique_tags <- DT::renderDT({
   # dat() %>%
   #   group_by(Mark.Species, Origin, Release.Site.Code) %>%
   #   summarise(Unique_Tags = n_distinct(TagID)) %>%
   #     select(Species = Mark.Species, Origin, `Release Site` = Release.Site.Code, `Unique Tags` = Unique_Tags)
   # })

   output$unique_tags_site <- DT::renderDT({
   dat() %>%
     group_by(Mark.Species, Origin, Release.Site.Code, SiteID) %>%
     summarise(Unique_Tags = n_distinct(TagID)) %>%
     select(Species = Mark.Species, Origin, `Release Site` = Release.Site.Code, SiteID, `Unique Tags` = Unique_Tags)
   })
   
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
  # # 

   
   output$watershed_est_tags <- DT::renderDT({
     
     dat() %>%
       #tmp %>%
       estNodeEff(node_order = node_order) %>%
       left_join(node_order %>%
                   select(Node, Group, SiteID = NodeSite)) %>%
       filter(tagsAtNode != 0) %>%
       mutate(detEff = round(detEff,2)) %>%
       select(SiteID, Node, `Unique Tags` = tagsAtNode,
              `Est. Tags` = estTagsAtNode, `Detection Efficiency` = detEff) 
   })  
   
   
  #  
  #  # estimated det. eff. by site
   # output$est_tags_site <- renderTable({
   #   tmp_df <- dat() %>%
   #     #filter(!Node%in%c("COCA0","COCB0","BSCA0","BSCB0", "IMNAHR")) %>%
   #     #anti_join(detect_hist %>%
   #     #            filter(NewTag == 'True') %>%
   #      #           select(TagID)
   #     #) %>%
   #     mutate(Node = gsub("A0", "B0", Node)) %>% # calculates efficiency at the site level
   #     identity()
   # 
   #   tmp_df %>%
   #     #mutate(Obs_location = ifelse(grepl("IR", Node), "Instream", "Ladder")) %>%
   #     #group_by(Mark.Species, Obs_location) %>%
   #     group_by(Mark.Species) %>%
   #     do(nodeEfficiency(.,direction="Upstream")) %>%
   #     arrange(Mark.Species, NodeOrder) %>%
   #     mutate(det_eff = Recaps/Marks, #ifelse(!grepl('IMNAHW', Node), Recaps/Marks, 1.00),
   #            se_eff = (1/Marks^2)*(Marks*det_eff)*(1-det_eff),
   #            N_tags = Unique_tags/det_eff,
   #            se_N = sqrt(((Marks+1)*(Unique_tags+1)*(Marks - Recaps) * (Unique_tags - Recaps))/ ((Recaps+1)^2 * (Recaps+2))),
   #            lwr_N = N_tags - 1.96*se_N,
   #            upr_N = N_tags + 1.96*se_N,
   #            N_up = lag(N_tags),
   #            Conversion = ifelse(N_tags/N_up*100 > 100, 100 , round(N_tags/N_up*100)),
   #            Conversion = paste0(Conversion,"%"),
   #            SiteID = gsub("B0", "", Node)
   #     ) %>%
   #     ungroup() %>%
   #     select(Species = Mark.Species, SiteID, `Unique Tags` = Unique_tags, Marks, Recaps,
   #            `Detection Efficiency` = det_eff, `Estimated Tags` = N_tags,
   #            `Lower 95CI` = lwr_N, `Upper 95CI` = upr_N) %>%
   #     filter(SiteID != 'IR5')
   # })
  #  
  #  
  # 
  #--------------------------------
  # Raw data
  #--------------------------------

  # rawdat <- reactive({input$rawdata})
  #  
  #  export_dat <- reactive({
  #  
  #    if(rawdat() == 'Processed Observations'){
  #      dat_all
  #    } else
  #  
  #    if(rawdat() == 'raw_chs'){
  #      raw_chs
  #    } else
  #  
  #    if(rawdat() == 'raw_bull'){
  #      raw_bull
  #    } else
  #  
  #    if(rawdat() == 'detect_hist'){
  #      detect_hist
  #    }
  # })
   
  output$raw_data <- DT::renderDataTable({
    #export_dat()
    dat_all()
  })

  # function for downloading data
  output$data_export <- downloadHandler(  #output name needs to match ui object id name

    #tmp_export <- export_dat()

    filename = function() {
      #paste0(rawdat(),"_", Sys.Date(), "_.csv")
      paste0("observations","_", Sys.Date(), "_.csv")
    },
    content = function(filename) {
      write.csv(dat_all(), filename, row.names = FALSE)
    }
  )

}#)
