# Server Logic ----
function(input, output) {

  showModal(modalDialog(
    title = "PITtrackR Use Agreement",
    "The PITtrackR web application uses live data obtained from Columbia Basin Research group at the University of Washington's School of Aquatic and Fisheries Science, and through their DART website. Please be patient while available data is received and processed."
  ))
  
  output$loaded <- renderText({
    if(input$loadData == 0) {
      paste("Select and load data!")
      #paste("<font color=\"#FF0000\"><b> Select and load data! </b></font>")
    } else {
      return()
    }
  })
  
  output$rtn_year_menu <- renderUI({
    
    yrs <- c(unique(all_dart_obs$spawn_yr), year(Sys.Date()))
    
    selectInput('rtn_year', "Spawn Year:",
              choices = yrs,
              selected = max(yrs))
  })
  
  output$spp_menu <- renderUI({
    
    if(input$rtn_year < year(Sys.Date())) {
      spp_choice = c('Chinook','Coho', 'Steelhead', 'Sockeye')
    } else {
      spp_choice = c('Chinook', 'Steelhead', 'Sockeye')
    }
    
    radioButtons('basin_spp', "Species:", inline = TRUE,
               choices = spp_choice,
               selected = 'Chinook')
  })  

observeEvent(input$loadData, {  
  
# Load PIT and Window Data ----
    
    if(input$basin_spp == 'Chinook'){spp <- c('fc', 'fcj')}
    if(input$basin_spp == 'Coho'){spp <- c('fk', 'fkj')}
    if(input$basin_spp == 'Steelhead'){spp <- c('fs', 'fsw')}
    if(input$basin_spp == 'Sockeye'){spp <- c('fb')}
    
  window_df <- queryWindowCnts(dam = 'LWG', 
                    spp_code = spp,
                    spawn_yr = as.numeric(input$rtn_year),
                    start_day = '01/01',
                    end_day = '12/31') %>%
      gather(key = spp, value = 'Count', -Year, -Date) %>%
      mutate(spp = gsub("_"," ",spp))
  
  # Loads processed data from Amazon and subsets by selected species
  # Data loads from Amazon S3 bucket after year is selected

  #dat_all <- eventReactive(input$loadData, {

    #withBusyIndicatorServer("loadData", {
    
  # load data from aws and PTAGIS ftp
  # aws.s3::s3read_using(FUN = read_csv,
  #                        bucket = "nptfisheries-pittracking",
  #                        object = paste0("PITcleanr_",
  #                                        input$rtn_year,
  #                                        #2019,
  #                                        "_chs_bull")) %>%
    # left_join(site_loc, by = 'SiteID') %>%
    #   mutate(Node = fct_relevel(Node, node_vec),
    #          SiteID = fct_relevel(SiteID, site_vec)) %>%
    #   left_join(tmp_mark, by = c('TagID' = 'tag_id'))
    
  # Load data from DART
  if(isolate(input$rtn_year < year(Sys.Date()))){
  
    dat_all <<- all_dart_obs %>%
    filter(species == input$basin_spp) %>%
    filter(spawn_yr == input$rtn_year)
  
    } else {
  
    tmp_ls <- processDART_LGR(species = input$basin_spp,
                                  spawnYear = as.numeric(input$rtn_year),
                                  configuration = my_config,
                                  truncate = T)

    tmp_mark <- tmp_ls$dart_obs %>%
      select(tag_id, mark_date, file_id, mark_site, rel_site, rel_date,
           t_rear_type, t_species, t_run, length, trans_status,
           trans_proj, trans_year) %>%
      distinct()

    dat_all <<- tmp_ls$proc_ch %>%
      left_join(site_loc, by = 'SiteID') %>%
      mutate(Node = fct_relevel(Node, node_vec),
           SiteID = fct_relevel(SiteID, site_vec),
           Group = as.character(Group)) %>%
      left_join(tmp_mark, by = c('TagID' = 'tag_id')) %>%
      mutate(species = input$basin_spp,
             spawn_yr = input$rtn_year) %>%
      ungroup()
  }
    
# Lower Granite Tab ----
  output$windowCnts_grp1 <- renderValueBox({

    n <- window_df %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    if(isolate(input$basin_spp)!= 'Steelhead'){    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'blue',
      icon = icon("fish"),
      subtitle = paste0("Adult ",isolate(input$basin_spp), " Window Count")
      )
    } else {
      valueBox(
        value = prettyNum(n, big.mark = ","),
        color = 'blue',
        icon = icon("fish"),
        subtitle = paste0(isolate(input$basin_spp), " Window Count")
      )
    }
  })
  
  output$windowCnts_grp2 <- renderValueBox({
    
    if(isolate(input$basin_spp)!= 'Steelhead'){
    n <- window_df %>%
      filter(spp == paste0("Jack ",isolate(input$basin_spp))) %>%
      summarise(n = sum(Count)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'green',
      icon = icon("fish"),
      subtitle = paste0("Jack ", isolate(input$basin_spp), " Window Count")
      )
    } else {
      n <- window_df %>%
        filter(spp == paste0("Wild ",isolate(input$basin_spp))) %>%
        summarise(n = sum(Count)) %>%
        pull(n)
      
      valueBox(
        value = prettyNum(n, big.mark = ","),
        color = 'green',
        icon = icon("fish"),
        subtitle = paste0("Wild ", isolate(input$basin_spp), " Window Count")
      )
    }
  })

# GRA previously tagged plot  
  output$release_grp <- renderPlot({
    dat_all %>%
      filter(SiteID == "GRA") %>%
      filter(t_rear_type != 'U') %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      filter(!is.na(rel_site)) %>%
      filter(rel_site != 'LGRLDR') %>%
      mutate(rel_year = year(rel_date)) %>%
      group_by(species, t_rear_type, rel_site, rel_year) %>%
      summarise(n = n_distinct(TagID)) %>%
      ggplot(aes(x = rel_site, y = n, fill = as.factor(rel_year))) +
      geom_col(colour = 'black') +
      coord_flip() +
      facet_wrap(~ t_rear_type, scales = 'free_y') +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Observed Tags',
           y = 'Release Site',
           fill = 'Release Year') +
      theme_bw() +
      theme(legend.position = 'bottom')
  })
  
 # window count plot 
  output$window_plot <- renderPlot({
    window_df %>%
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
  
# GRA tagged plot  
  output$gra_tagged <- renderPlot({
    dat_all %>%
      filter(SiteID == "GRA") %>%
      #filter(grepl(input$basin_spp, t_species)) %>%
      filter(mark_site == 'LGRLDR') %>%
      filter(spawn_yr == input$rtn_year) %>%
      mutate(rel_year = year(rel_date)) %>%
      group_by(t_species, t_rear_type, rel_site, rel_year) %>%
      summarise(n = n_distinct(TagID)) %>%
      ggplot(aes(x = t_rear_type, y = n, fill = t_rear_type)) +
      geom_col(colour = 'black') +
      #facet_wrap(~Origin, scales = 'free') +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Release Site',
          y = 'Observed Tags',
          fill = '') +
      theme_bw() +
      theme(legend.position = 'none')
  })
  
  # Table of tags observed at GRA
  output$rel_tag_grp <- DT::renderDT({
    dat_all %>%
      filter(SiteID == "GRA") %>%
      filter(!is.na(rel_site)) %>%
      #filter(Release.Site.Code != 'LGRLDR') %>%
      mutate(rel_year = year(rel_date)) %>%
      group_by(species, t_rear_type, rel_site, rel_year) %>%
      summarise(n = n_distinct(TagID),
                LastObs = as_date(max(ObsDate))) %>%
      arrange(desc(LastObs)) %>%
      select(Species = species, Origin = t_rear_type, `Release Site` = rel_site, `Release Year` = rel_year, `Unique Tags` = n, `Last Obs` = LastObs)
  })  
  
  # Cumlative Arrival
  output$gra_arrival <- renderPlot({
    
    dat_all %>%
      #filter(Mark.Species == input$basin_spp) %>%
      filter(SiteID == 'GRA') %>%
      filter(rel_site != 'LGRLDR') %>%
      filter(t_rear_type == 'H') %>%
      group_by(TagID, t_species, t_rear_type, rel_site) %>%
      #summarise(minObs = min(firstObsDateTime)) %>%
      ggplot(aes(x = TrapDate, colour = t_rear_type)) +
      stat_ecdf() +
      #geom_histogram(colour = 'black')+
      scale_colour_brewer(palette = 'Set1') +
      facet_wrap(~rel_site, ncol = 3, drop = TRUE) +
      labs(x = 'Date',
           y = 'Observed Tags',
           colour = '') +
      theme_bw() +
      theme(legend.position = 'none')
    
  })
  
# GRA arrival time plot
  output$release_plot <- renderPlot({
    dat_all %>%
     #dat_all %>%
      filter(SiteID == "GRA") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      #filter(grepl('Chinook', Mark.Species)) %>%
      filter(rel_site != 'LGRLDR') %>%
      filter(t_rear_type == 'H') %>%
      mutate(rel_year = year(rel_date)) %>%
      group_by(rel_site, rel_year, t_rear_type, TrapDate) %>%
      summarise(n = n_distinct(TagID)) %>%
      ggplot(aes(x = TrapDate, y = n, fill = as.factor(rel_year))) +
      geom_col(colour = 'black') +
      #scale_x_datetime(labels = date_format("%d-%m"), breaks = "1 week") +
      facet_wrap( ~ rel_site, ncol = 3, drop = TRUE) +
      scale_fill_brewer(palette = 'Set1') +
      labs(x = 'Arrival Date',
           y = 'Observed Tags',
           fill = 'Release Year') +
      theme_bw() +
      theme(legend.position = 'bottom')
  })

# Snake Basin Tab ----
  
  output$tagsGRA <- renderValueBox({
    n <- dat_all %>%
      filter(SiteID == "GRA") %>%
      #filter(grepl(input$basin_spp,Mark.Species)) %>%
      summarise(n = n_distinct(TagID)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'aqua',
      subtitle = "Released Granite Tags"
    )
  })
  
  output$tagsObs <- renderValueBox({
    
    n <- dat_all %>%
      filter(SiteID != "GRA") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      summarise(n = n_distinct(TagID)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'green',
      subtitle = "Observed Granite Tags"
    )
  })
  
  output$tagsPercent <- renderValueBox({
    
    n <- dat_all %>%
      filter(SiteID == "GRA") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      summarise(n = n_distinct(TagID)) %>%
      pull(n)
    
    o <- dat_all %>%
      filter(SiteID != "GRA") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      summarise(n = n_distinct(TagID)) %>%
      pull(n)
    
    p <- paste0(round((o/n)*100),"%")
    
    valueBox(
      value = p,
      color = 'blue',
      subtitle = "Percent Observed"
    )
  })
  
# Basin Map Data
   map_dat <- dat_all %>%
       #filter(Mark.Species == input$basin_spp) %>%
       group_by(t_species, SiteID, Latitude, Longitude) %>%
       summarise(n = n_distinct(TagID)) %>%
       ungroup()
   
# Basin map output   
   output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
         setView(lat = 45.8,
                 lng = -116.1,
                 zoom = 6) %>%
         # setMaxBounds(lng1 = -119,
         #              lat1 = 42,
         #              lng2 = -114,
         #              lat2 = 49) %>%
         #addProviderTiles(providers$OpenStreetMap)
         addProviderTiles(providers$Esri.WorldTopoMap) %>%
         addScaleBar(position = 'topright') %>%
   
    addCircles(map_dat$Longitude, map_dat$Latitude, group = 'sites', radius = 100,
               color = 'black',
    label = map_dat$SiteID,
    labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                direction = 'right',
                                style = list("color" = 'black', "font-weight" = "bold", "padding-left" = "10px")),
    popup = paste("<b>Site ID:</b>", map_dat$SiteID, "<br>",
                  "<b>Unique Tags:</b>", map_dat$n, "<br>"),
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
    #                          "<b>Unique Tags:</b>", map_dat()$n, "<br>")
   })
   
   # observeEvent(input$loadData, 
   #   ignoreNULL = FALSE, {
   #    
   #    leafletProxy("map") %>%
   #       clearGroup("sites") %>%
   # 
   # })
   
# Basin Summaries ----   
 # Basin unique tags per group table  
   output$site_tags <- DT::renderDT({
     dat_all %>%
       #filter(Mark.Species == input$basin_spp) %>%
     mutate(Group = ifelse(is.na(Group),'GRA', Group)) %>%
       group_by(species, t_rear_type, Group) %>%
       summarise(n = n_distinct(TagID),
                 LastObs = as_date(max(ObsDate))) %>%     
       select(Species = species,
              Origin = t_rear_type,
              `Sub-basin` = Group,
              `Last Obs` = LastObs,
              `Unique Tags` = n)
   })  
   
 # Basin unique tags per site  
   output$basin_tag_plot <- renderPlot({
     dat_all %>%
       #filter(Mark.Species == input$basin_spp) %>%
     filter(!is.na(Group)) %>%
       distinct(t_species, t_rear_type, Group, SiteID, TagID) %>%
       ggplot(aes(x = SiteID, colour = t_rear_type, fill = t_rear_type)) +
       geom_bar(colour = 'black') +
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~Group, scales = 'free') +
       labs(x = 'Site ID',
            y = 'Observed Tags',
            fill = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
   })
   
 # Basin travel time  
   output$basin_travel <- renderPlot({
     
     tmp2 <- dat_all %>%
       #filter(Mark.Species == input$basin_spp) %>%
     arrange(TagID, ObsDate) %>%
       mutate(Group = ifelse(is.na(Group),"GRA", Group)) %>%
       group_by(TagID,t_rear_type, Group, SiteID) %>%
       summarise(minObsDate = min(ObsDate)) %>%
       ungroup() %>%
       group_by(TagID) %>%
       mutate(dwnObsDate = lag(minObsDate),
              Reach = paste0(lag(Group)," - ", Group),
              TravelTime = difftime(minObsDate, dwnObsDate, units = 'days'))
     
     tmp2 %>%
       filter(!is.na(TravelTime),
              grepl("GRA - ", Reach))%>%
       ggplot(aes(x = TravelTime, fill = t_rear_type)) +
       geom_histogram(colour = 'black')+
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~ Reach) +
       labs(x = 'Travel Time (days)',
            y = 'Observed Tags',
            fill = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
   
# Cumlative Arrival
   output$basin_arrival <- renderPlot({
     
     dat_all %>%
       #filter(Mark.Species == input$basin_spp) %>%
       filter(!is.na(Group)) %>%
       group_by(TagID, t_species, t_rear_type, Group) %>%
       summarise(minObs = min(ObsDate)) %>%
       ggplot(aes(x = minObs, colour = t_rear_type)) +
       stat_ecdf() +
       #geom_histogram(colour = 'black')+
       scale_colour_brewer(palette = 'Set1') +
       facet_wrap(~Group) +
       labs(x = 'Date',
            y = 'Observed Tags',
            colour = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
   
   
# Basin detection efficiencies
   output$basin_est_tags <- DT::renderDT({
     
     dat_all %>%
      # filter(Mark.Species == input$basin_spp) %>%
       estNodeEff(node_order = node_order) %>%
       left_join(node_order %>%
                   select(Node, Group, SiteID = NodeSite)) %>%
       filter(tagsAtNode != 0) %>%
       mutate(detEff = round(detEff,2)) %>%
       select(`Sub-basin` = Group, SiteID, Node, `Unique Tags` = tagsAtNode,
              `Est. Tags` = estTagsAtNode, `Detection Efficiency` = detEff)
   })
   
 # Basin migration plot  
   # output$basin_mig_plot <- renderPlot({
   #   
   #   # axis_labs <- dat_all %>%
   #   #   distinct(SiteID, RKMTotal) %>% #changed from Node
   #   #   mutate(rkmStart = min(RKMTotal),
   #   #          RiverRKM = RKMTotal - rkmStart,
   #   #          SiteID = paste0(SiteID," (",RiverRKM,")")) #changed from Node
   #   
   #   tag_basin <- dat_all %>%
   #     slice(which.max(lastObsDateTime)) %>%
   #     select(TagID, loc = Group)
   #   
   #   dat_all %>%
   #     filter(Mark.Species == input$basin_spp) %>%
   #     mutate(Group = ifelse(is.na(Group), 'GRA', Group)) %>%
   #     group_by(TagID, Group) %>%
   #     slice(which.min(firstObsDateTime)) %>%
   #     left_join(tag_basin, by = 'TagID') %>%
   #       ggplot(aes(x = firstObsDateTime, y = RKMTotal, group = TagID)) +
   #       geom_line() +
   #       #scale_color_brewer(palette = 'Set1') +
   #     labs(x = 'Observation Date',
   #          y = '(RKM)',
   #          colour = 'Sub-basin') +
   #       theme_bw() +
   #       theme(legend.position = 'bottom')
   # })
   
# Watershed Observation Tab ----
   
    w <- dat_all %>%
          #filter(Mark.Species == input$pit_spp) %>%
          distinct(Group) %>%
          pull()
     
      w[!is.na(w)]
   
   output$watershed_menu <- renderUI({
      selectInput('watershed', "Selected Watershed:",
                  choices = w,
                  selected = 'ImnahaRiver',
                  selectize = FALSE,
                  multiple = FALSE
                  )                                  
   })
})

observeEvent(input$watershed,{   
# Watershed and species only data
     # inner_join(dat_all,tag_ids(), by = 'TagID') %>%
   dat <- dat_all %>%
        #filter(Mark.Species == input$pit_spp) %>%
        filter(Group == input$watershed)


# Get unique tag_ids for selected watershed and species
   tag_ids <- dat %>%
     distinct(TagID) %>%
     pull()
 
# Count of unique tags.     
   output$watershed_tags <- renderValueBox({
     
     n <- length(tag_ids)
     
     valueBox(
       value = prettyNum(n, big.mark = ","),
       color = 'green',
       icon = icon("fish"),
       subtitle = paste0("Unique tags observed in the watershed.")
     )
   })  
   
     
   output$tagid_menu <- renderUI({
     selectInput('tag_id', "Unique Tag IDs:", tag_ids, selected = tag_ids[1], multiple=TRUE, selectize=FALSE, size = 5
     )
   })
   
   selected_tag_ids <- reactive({  # may need to remove, or place into observer
     input$tag_id
   })

# Watershed Arrival time plot 
   output$arrival_plot <- renderPlot({
     dat %>%
       group_by(TagID, t_species, t_rear_type, rel_site, SiteID) %>%
       slice(which.min(ObsDate)) %>%
       ggplot(aes(x=ObsDate, fill = t_rear_type)) +
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
            y = "Observed Tags",
            fill = 'Origin')
   })   
   
   
# Watershed travel time plot
   output$watershed_travel <- renderPlot({
     
     tmp2 <- dat %>%
       filter(!is.na(Group)) %>%
       #mutate(Group = ifelse(is.na(Group), 'GRA', Group)) %>%
       group_by(TagID, t_rear_type, SiteID, Node) %>%
       summarise(minObsDate = min(ObsDate)) %>%
       ungroup() %>%
       group_by(TagID) %>%
       mutate(dwnObsDate = lag(minObsDate),
              Reach = paste0(lag(Node)," - ", Node),
              TravelTime = difftime(minObsDate, dwnObsDate, units = 'days'))
      
     tmp2 %>%
       filter(!is.na(TravelTime))%>%
       ggplot(aes(x = TravelTime, fill = t_rear_type)) +
       geom_histogram(colour = 'black')+
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~Reach) +
       labs(x = 'Travel Time (days)',
            y = 'Observed Tags',
            fill = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
     

# Watershed unique tags per site
   output$unique_tags_site <- DT::renderDT({
     dat %>%
       group_by(t_species, t_rear_type, rel_site, SiteID) %>%
       summarise(Unique_Tags = n_distinct(TagID),
                 LastObs = as_date(max(ObsDate))) %>%
       select(Species = t_species, Origin = t_rear_type, `Release Site` = rel_site, SiteID,
              `Last Obs` = LastObs, `Unique Tags` = Unique_Tags)
   })

# Watershed Detection Eff.  
   output$watershed_est_tags <- DT::renderDT({
     
     dat %>%
       #tmp %>%
       estNodeEff(node_order = node_order) %>%
       left_join(node_order %>%
                   select(Node, Group, SiteID = NodeSite)) %>%
       filter(tagsAtNode != 0) %>%
       mutate(detEff = round(detEff,2)) %>%
       select(SiteID, Node, `Unique Tags` = tagsAtNode,
              `Est. Tags` = estTagsAtNode, `Detection Efficiency` = detEff) 
   })  
   
# Watershed Migration Plot
   output$mig_plot <- renderPlot({
     
     if(is.null(input$tag_id)){
       return()
     } else {
       lims <- c(min(dat$ObsDate), Sys.time())
       tmp_tag <- selected_tag_ids()
       
       axis_labs <- dat %>%
         distinct(SiteID, RKMTotal) %>% #changed from Node
         mutate(rkmStart = min(RKMTotal),
                RiverRKM = RKMTotal - rkmStart,
                # RiverRKM = case_when(
                #  grepl('B0', Node) ~ RiverRKM - 2,
                #  grepl('AO', Node) ~ RiverRKM + 2,
                #  TRUE ~ RKMTotal),
                SiteID = paste0(SiteID," (",RiverRKM,")")) #changed from Node
       
       dat %>%
         #dat %>%
         filter(TagID %in% tmp_tag) %>%
         filter(SiteID != 'GRA') %>%
         group_by(TagID, SiteID, Migration, Direction) %>% #new
         slice(which.min(ObsDate)) %>% #new
         ggplot(aes(x = ObsDate, y = RKMTotal, group = TagID, colour = t_rear_type), drop = TRUE) +
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
              colour = 'Origin')
     }
   })#, height = 475, width = 700)   

})

# PDF Reports ----
   

output$reports <- downloadHandler(
  
  filename = function(){
    paste0(gsub(" ","_",input$pdf_reports),
           "_",
           format(Sys.time(), "%m_%d_%y_%H%M%S"),
           ".html")
  },
  
 content = function(file){
   tempReport <- "documents/2019_Chinook_Bull_report.Rmd"
   rmarkdown::render(tempReport, output_file = file)
 }
)
   
# Raw data ----


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
    dat_all
  })

  # function for downloading data
  output$data_export <- downloadHandler(  #output name needs to match ui object id name

    #tmp_export <- export_dat()

    filename = function() {
      #paste0(rawdat(),"_", Sys.Date(), "_.csv")
      paste0("observations","_", Sys.Date(), ".csv")
    },
    content = function(filename) {
      write.csv(dat_all, filename, row.names = FALSE)
    }
  )

}#)
