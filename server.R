# Server Logic ----
function(input, output) {

  output$deploy_time <- renderText({
    dt <- as.POSIXct(readLines("deploy_time.txt"), tz = "UTC")
    dt_boise <- format(dt, tz = "America/Boise", usetz = TRUE, "%B %d, %Y at %I:%M %p %Z")
    paste("Updated:", dt_boise)
  })
  
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
  
  #output$spp_menu <- renderUI({
    # 
    # if(input$rtn_year > year(Sys.Date())) {
    #   spp_choice = 'Steelhead'
    # } else {
      #spp_choice = c('Chinook','Coho', 'Steelhead', 'Sockeye')
    # }
    
    # radioButtons('basin_spp', "Species:", inline = TRUE,
    #              choices = c('Chinook','Coho', 'Steelhead', 'Sockeye'),
    #              selected = 'Chinook')
    
  #   print(input$basin_spp)
  # })  
  
  
  output$spp_menu <- renderUI({
    radioButtons('basin_spp', "Species:", inline = TRUE,
                 choices = c('Chinook'), #,'Coho', 'Steelhead', 'Sockeye'),
                 selected = 'Chinook')
  })
  
  output$rtn_year_menu <- renderUI({
    today <- Sys.Date()    
    
     if(input$basin_spp == 'Steelhead' & month(today)>=7){
        yrs <- year(today) + 1 #2010:(year(today)+1)
     } else {
      yrs <- year(today) #2010:year(today) #c(unique(all_dart_obs$spawn_yr), year(Sys.Date()), year(Sys.Date())+1)
     }
    
    selectInput('rtn_year', "Spawn Year:",
              choices = yrs,
              selected = max(yrs))
  })
  


observeEvent(input$loadData, {  
  
# Load PIT and Window Data ----
    print('loadData')
    if(input$basin_spp == 'Chinook'){spp <- c('fc', 'fcj')}
    if(input$basin_spp == 'Coho'){spp <- c('fk', 'fkj')}
    if(input$basin_spp == 'Steelhead'){spp <- c('fs', 'fsw')}
    if(input$basin_spp == 'Sockeye'){spp <- c('fb')}
    
  print(input$basin_spp)
  print(input$rtn_year)
  window_df <- queryWindowCnts(dam = 'LWG', 
                    spp_code = spp,
                    spawn_yr = as.numeric(input$rtn_year),
                    start_day = '01/01',
                    end_day = '12/31') %>%
      tidyr::gather(key = spp, value = 'Count', -Year, -Date) %>%
      mutate(spp = gsub("_"," ",spp))
  print('window_download')
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
    # left_join(site_loc, by = 'site_code') %>%
    #   mutate(Node = fct_relevel(Node, node_vec),
    #          site_code = fct_relevel(site_code, site_vec)) %>%
    #   left_join(tmp_mark, by = c('tag_code' = 'tag_id'))
    
  # Load data from DART
  # if(isolate(input$rtn_year < year(Sys.Date()))){
  # 
  #   dat_all <<- all_dart_obs %>%
  #   filter(species == input$basin_spp) %>%
  #   filter(spawn_yr == input$rtn_year)
  # 
  #   } else {
  # 
  
    # tmp_ls <- compressDART(species = input$basin_spp,
    #                        loc = 'GRA',
    #                        spawn_year = as.numeric(input$rtn_year),
    #                        configuration = configuration)
    print('compress')
    # tmp_ls <- processDART_LGR(species = input$basin_spp,
    #                               spawnYear = as.numeric(input$rtn_year),
    #                               configuration = configuration,
    #                               truncate = T)

    # tmp_mark <- tmp_ls$dart_obs %>%
    #   select(tag_code, mark_date, file_id, mark_site, rel_site, rel_date,
    #        mark_rear_type_name, t_species, t_run, length, trans_status,
    #        trans_proj, trans_year) %>%
    #   distinct()
    # 
    # dat_all <- tmp_ls$compress_obs %>%
    #   rename(site_code = node) %>%
    #   mutate(site_code = gsub('_D|_U|_M', '', site_code)) %>%
    #   left_join(site_loc, by = 'site_code') %>%
    #    mutate(#node = fct_relevel(node, node_vec),
    #         site_code = fct_relevel(site_code, site_vec)) %>%
    #   left_join(site_grp %>%
    #               select(site_code = node,
    #                      grp = grp),
    #             by = 'site_code') %>%
    #   left_join(tmp_mark, by = 'tag_code') %>%
    #   mutate(species = input$basin_spp,
    #          spawn_yr = input$rtn_year) %>%
    #   ungroup() %>%
    #   filter(!is.na(grp))
 # }
    
# Lower Granite Tab ----
  output$windowCnts_grp1 <- renderValueBox({
    print('windowCnts')
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
      filter(site_code == "LGR") %>%
      filter(mark_rear_type_name != 'U') %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      filter(!is.na(rel_site)) %>%
      filter(rel_site != 'LGRLDR') %>%
      mutate(rel_year = year(rel_date)) %>%
      group_by(species, mark_rear_type_name, rel_site, rel_year) %>%
      summarise(n = n_distinct(tag_code)) %>%
      ggplot(aes(x = rel_site, y = n, fill = as.factor(rel_year))) +
      geom_col(colour = 'black') +
      coord_flip() +
      facet_wrap(~ mark_rear_type_name, scales = 'free_y') +
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
      filter(site_code == "LGR") %>%
      #filter(grepl(input$basin_spp, t_species)) %>%
      filter(mark_site == 'LGRLDR') %>%
      filter(spawn_yr == yr) %>% #input$rel_year
      mutate(rel_year = year(rel_date)) %>%
      group_by(mark_species_name, mark_rear_type_name, rel_site, rel_year) %>%
      summarise(n = n_distinct(tag_code)) %>%
      ggplot(aes(x = mark_rear_type_name, y = n, fill = mark_rear_type_name)) +
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
      filter(site_code == "LGR") %>%
      filter(!is.na(rel_site)) %>%
      #filter(Release.Site.Code != 'LGRLDR') %>%
      mutate(rel_year = year(rel_date)) %>%
      group_by(species, mark_rear_type_name, rel_site, rel_year) %>%
      summarise(n = n_distinct(tag_code),
                LastObs = as_date(max(min_det))) %>%
      arrange(desc(LastObs)) %>%
      select(Species = species, Origin = mark_rear_type_name, `Release Site` = rel_site, `Release Year` = rel_year, `Unique Tags` = n, `Last Obs` = LastObs)
  })  
  
  # # Cumlative Arrival
  # output$gra_arrival <- renderPlot({
  #   
  #   dat_all %>%
  #     #filter(Mark.Species == input$basin_spp) %>%
  #     filter(site_code == 'GRA') %>%
  #     filter(rel_site != 'LGRLDR') %>%
  #     filter(mark_rear_type_name == 'H') %>%
  #     group_by(tag_code, t_species, mark_rear_type_name, rel_site) %>%
  #     summarise(min_det = min(min_det)) %>%
  #     ggplot(aes(x = min_det, colour = mark_rear_type_name)) +
  #     stat_ecdf() +
  #     #geom_histogram(colour = 'black')+
  #     scale_colour_brewer(palette = 'Set1') +
  #     facet_wrap(~rel_site, ncol = 3, drop = TRUE) +
  #     labs(x = 'Date',
  #          y = 'Observed Tags',
  #          colour = '') +
  #     theme_bw() +
  #     theme(legend.position = 'none')
  #   
  # })
  
# GRA arrival time plot
  # output$release_plot <- renderPlot({
  #   dat_all %>%
  #    #dat_all %>%
  #     filter(site_code == "GRA") %>%
  #     #filter(grepl(input$basin_spp, Mark.Species)) %>%
  #     #filter(grepl('Chinook', Mark.Species)) %>%
  #     filter(rel_site != 'LGRLDR') %>%
  #     filter(mark_rear_type_name == 'H') %>%
  #     mutate(rel_year = year(rel_date)) %>%
  #     group_by(rel_site, rel_year, mark_rear_type_name, min_det) %>%
  #     summarise(n = n_distinct(tag_code)) %>%
  #     ggplot(aes(x = min_det, y = n, fill = as.factor(rel_year))) +
  #     geom_col(colour = 'black') +
  #     #scale_x_datetime(labels = date_format("%d-%m"), breaks = "1 week") +
  #     facet_wrap( ~ rel_site, ncol = 3, drop = TRUE) +
  #     scale_fill_brewer(palette = 'Set1') +
  #     labs(x = 'Arrival Date',
  #          y = 'Observed Tags',
  #          fill = 'Release Year') +
  #     theme_bw() +
  #     theme(legend.position = 'bottom')
  # })

# Snake Basin Tab ----
  
  output$tagsGRA <- renderValueBox({
    n <- dat_all %>%
      filter(site_code == "LGR") %>%
      #filter(grepl(input$basin_spp,Mark.Species)) %>%
      summarise(n = n_distinct(tag_code)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'aqua',
      subtitle = "Released Granite Tags"
    )
  })
  
  output$tagsObs <- renderValueBox({
    
    n <- dat_all %>%
      filter(site_code != "LGR") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      summarise(n = n_distinct(tag_code)) %>%
      pull(n)
    
    valueBox(
      value = prettyNum(n, big.mark = ","),
      color = 'green',
      subtitle = "Observed Granite Tags"
    )
  })
  
  output$tagsPercent <- renderValueBox({
    
    n <- dat_all %>%
      filter(site_code == "LGR") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      summarise(n = n_distinct(tag_code)) %>%
      pull(n)
    
    o <- dat_all %>%
      filter(site_code != "LGR") %>%
      #filter(grepl(input$basin_spp, Mark.Species)) %>%
      summarise(n = n_distinct(tag_code)) %>%
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
       filter(mark_species_name == input$basin_spp) %>%
       group_by(mark_species_name, site_code, geometry) %>%
       summarise(n = n_distinct(tag_code)) %>%
       ungroup() %>%
     # left_join(site_loc, by = 'site_code') %>%
     sf::st_as_sf() %>%
     sf::st_transform(crs = 4326)
   
# Basin map output   
   output$map <- renderLeaflet({
      leaflet(options = leafletOptions(minZoom = 7, doubleClickZoom = FALSE)) %>%
         setView(lat = 45.8,
                 lng = -116.1,
                 zoom = 6) %>%
         addProviderTiles(providers$Esri.WorldTopoMap) %>%
         addScaleBar(position = 'topright') %>%
    addCircles(data = map_dat, group = 'sites', radius = 100,
               color = 'black',
    label = map_dat$site_code,
    labelOptions = labelOptions(noHide = T, textOnly = TRUE,
                                direction = 'right',
                                style = list("color" = 'black', "font-weight" = "bold", "padding-left" = "10px")),
    popup = paste("<b>Site ID:</b>", map_dat$site_code, "<br>",
                  "<b>Unique Tags:</b>", map_dat$n, "<br>"),
    popupOptions = popupOptions(noHide = T, textsize = "15px"),
    highlightOptions = highlightOptions(color = "white",
                                     weight = 5, bringToFront = F, opacity = 1))

     # addMarkers(map_dat()$Longitude, map_dat()$Latitude, group = 'sites',
     #            label = map_dat()$site_code,
     #            labelOptions = labelOptions(noHide = T),
     #            popup = paste("<b>Site ID:</b>", map_dat()$site_code, "<br>",
     #                          "<b>Unique Tags:</b>", map_dat()$n, "<br>"),
     #            popupOptions = popupOptions(noHide = TRUE, textsize = "15px"))
                
    # addPopups(map_dat()$Longitude, map_dat()$Latitude,
    #             paste("<b>Site ID:</b>", map_dat()$site_code, "<br>",
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
       filter(mark_species_name == input$basin_spp) %>%
     mutate(grp = ifelse(is.na(grp),'GRA', grp)) %>%
       group_by(species, mark_rear_type_name, grp) %>%
       summarise(n = n_distinct(tag_code),
                 LastObs = as_date(max(min_det))) %>%     
       select(Species = species,
              Origin = mark_rear_type_name,
              `Main Gate` = grp,
              `Last Obs` = LastObs,
              `Unique Tags` = n)
   })  
   
 # Basin unique tags per site  
   output$basin_tag_plot <- renderPlot({
     dat_all %>%
       filter(mark_species_name == input$basin_spp) %>%
     filter(!is.na(grp)) %>%
       distinct(species, mark_rear_type_name, grp, site_code, tag_code) %>%
       ggplot(aes(x = site_code, colour = mark_rear_type_name, fill = mark_rear_type_name)) +
       geom_bar(colour = 'black') +
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~grp, scales = 'free', ncol = 3) +
       labs(x = 'Site ID',
            y = 'Observed Tags',
            fill = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
   })
   
 # Basin travel time  
   output$basin_travel <- renderPlot({
     
     tmp2 <- dat_all %>%
       filter(mark_species_name == input$basin_spp) %>%
     arrange(tag_code, min_det) %>%
       mutate(grp = ifelse(is.na(grp),"GRA", grp)) %>%
       group_by(tag_code,mark_rear_type_name, grp) %>%
       summarise(minmin_det = min(min_det)) %>%
       ungroup() %>%
       group_by(tag_code) %>%
       mutate(dwnmin_det = lag(minmin_det),
              Reach = paste0(lag(grp)," - ", grp),
              TravelTime = difftime(minmin_det, dwnmin_det, units = 'days'))
     
     tmp2 %>%
       filter(!is.na(TravelTime),
              grepl("LGR - ", Reach))%>%
       filter(!grepl('Downstream LGR -', Reach)) %>%
       ggplot(aes(x = TravelTime, fill = mark_rear_type_name)) +
       geom_histogram(colour = 'black')+
       scale_fill_brewer(palette = 'Set1') +
       facet_wrap(~ Reach, scales = 'free_y', ncol = 3) +
       labs(x = 'Travel Time (days)',
            y = 'Observed Tags',
            fill = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
   
# Cumlative Arrival
   output$basin_arrival <- renderPlot({
     
     dat_all %>%
       filter(mark_species_name == input$basin_spp) %>%
       filter(!is.na(grp)) %>%
       filter(grp != 'Downstream LGR') %>%
       group_by(tag_code, species, mark_rear_type_name, grp) %>%
       summarise(minObs = min(min_det)) %>%
       ggplot(aes(x = minObs, colour = mark_rear_type_name)) +
       stat_ecdf() +
       #geom_histogram(colour = 'black')+
       scale_colour_brewer(palette = 'Set1') +
       facet_wrap(~grp, ncol = 3) +
       labs(x = 'Date',
            y = 'Observed Tags',
            colour = 'Origin') +
       theme_bw() +
       theme(legend.position = 'bottom')
     
   })
   


         
# Basin detection efficiencies
   output$basin_est_tags <- DT::renderDT({

 dat_all %>%
       filter(mark_species_name == input$basin_spp) %>%
       estNodeEff(node_order = node_order) %>%
       filter(tags_at_node != 0) %>%
       # left_join(configuration %>%
       #             sf::st_drop_geometry() %>%
       #             filter(!is.na(grp)) %>%
       #             select(node,
       #                    grp = grp) %>%
       #             distinct()) %>%
       # left_join(node_order %>%
       #             mutate(grp = stringr::str_split(path, ' ', simplify = TRUE)[,2],
       #                    grp = ifelse(grp == "", 'GRA', grp),
       #                    site_code = stringr::str_split(node, "_", simplify = TRUE)[,1]) %>%
       #             select(node, grp, site_code)) %>%

       mutate(detEff = round(eff_est,2)) %>%
       select(Node = node, `Unique Tags` = tags_at_node,
              `Est. Tags` = est_tags_at_node, `Detection Efficiency` = detEff)
   })
   
 # Basin migration plot  
   # output$basin_mig_plot <- renderPlot({
   #   
   #   # axis_labs <- dat_all %>%
   #   #   distinct(site_code, RKMTotal) %>% #changed from Node
   #   #   mutate(rkmStart = min(RKMTotal),
   #   #          RiverRKM = RKMTotal - rkmStart,
   #   #          site_code = paste0(site_code," (",RiverRKM,")")) #changed from Node
   #   
   #   tag_basin <- dat_all %>%
   #     slice(which.max(lastmin_detTime)) %>%
   #     select(tag_code, loc = grp)
   #   
   #   dat_all %>%
   #     filter(Mark.Species == input$basin_spp) %>%
   #     mutate(grp = ifelse(is.na(grp), 'GRA', grp)) %>%
   #     group_by(tag_code, grp) %>%
   #     slice(which.min(firstmin_detTime)) %>%
   #     left_join(tag_basin, by = 'tag_code') %>%
   #       ggplot(aes(x = firstmin_detTime, y = RKMTotal, grp = tag_code)) +
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
          distinct(grp) %>%
          pull() %>% as.character()
     
   output$watershed_menu <- renderUI({
      selectInput('watershed', "Selected Main Gate Array:",
                  choices = w,
                  selected = 'SF Clearwater',
                  selectize = FALSE,
                  multiple = FALSE
                  )                                  
   })
})

observeEvent(input$watershed,{   
# Watershed and species only data
     # inner_join(dat_all,tag_ids(), by = 'tag_code') %>%
   dat <- dat_all %>%
        #filter(Mark.Species == input$pit_spp) %>%
        filter(grp == input$watershed)


# Get unique tag_ids for selected watershed and species
   tag_ids <- dat %>%
     distinct(tag_code) %>%
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
       group_by(tag_code, species, mark_rear_type_name, rel_site, site_code) %>%
       slice(which.min(min_det)) %>%
       mutate(d_obs = floor_date(min_det, unit = 'days')) %>%
       ggplot(aes(x=d_obs, fill = mark_rear_type_name)) +
       geom_histogram(colour = 'black', bins = 100) +
       #geom_vline(xintercept = as.numeric(ymd_hms("20180611 15:00:00")), linetype=2)+
       scale_fill_brewer(palette = 'Set1') +
       #scale_fill_viridis(discrete = TRUE) +
       #facet_grid(factor(site_code,levels=c("IR1","IR2","IR3","IR4","IML","IR5"))~Mark.Species,scales="free_y") +
       facet_grid(site_code ~ .) +
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
       filter(!is.na(grp)) %>%
       #mutate(Group = ifelse(is.na(Group), 'GRA', Group)) %>%
       group_by(tag_code, mark_rear_type_name, site_code, node) %>%
       summarise(min_det = min(min_det)) %>%
       ungroup() %>%
       group_by(tag_code) %>%
       mutate(dwnmin_det = lag(min_det),
              Reach = paste0(lag(node)," - ", node),
              TravelTime = difftime(min_det, dwnmin_det, units = 'days'))
      
     tmp2 %>%
       filter(!is.na(TravelTime))%>%
       ggplot(aes(x = TravelTime, fill = mark_rear_type_name)) +
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
       group_by(species, mark_rear_type_name, rel_site, site_code) %>%
       summarise(Unique_Tags = n_distinct(tag_code),
                 LastObs = as_date(max(min_det))) %>%
       select(Species = species, Origin = mark_rear_type_name, `Release Site` = rel_site, site_code,
              `Last Obs` = LastObs, `Unique Tags` = Unique_Tags)
   })

# Watershed Detection Eff.  
   output$watershed_est_tags <- DT::renderDT({
     
    dat %>%
       estNodeEff(node_order = node_order) %>%
       filter(tags_at_node != 0) %>%    
       mutate(detEff = round(eff_est,2)) %>%
       select(Node = node, `Unique Tags` = tags_at_node,
              `Est. Tags` = est_tags_at_node, `Detection Efficiency` = detEff) 
   })  
   
# Watershed Migration Plot
   
  output$mig_plot <- renderPlot({
     
     if(is.null(input$tag_id)){
       return()
     } else {
       lims <- c(min(dat$min_det), Sys.time())
       tmp_tag <- selected_tag_ids()
       
       axis_labs <- dat %>%
         distinct(site_code, rkm_total) %>% #changed from Node
         mutate(rkmStart = min(rkm_total),
                RiverRKM = rkm_total - rkmStart,
                # RiverRKM = case_when(
                #  grepl('B0', Node) ~ RiverRKM - 2,
                #  grepl('AO', Node) ~ RiverRKM + 2,
                #  TRUE ~ RKMTotal),
                site_code = paste0(site_code," (",RiverRKM,")")) #changed from Node
       
       dat %>%  
         filter(tag_code %in% tmp_tag) %>%
         select(-node_order, -path) %>%
         PITcleanr::addDirection(parent_child = pc_nodes) %>%
         filter(site_code != 'GRA') %>%
         group_by(tag_code, site_code, direction) %>% #new
         slice(which.min(min_det)) %>% #new
         ggplot(aes(x = min_det, y = rkm_total, group = tag_code, colour =  mark_rear_type_name), drop = TRUE) +
         #geom_polygon(data = above_weir) +
         geom_line() +
         geom_point() +
         #geom_vline(xintercept = as.numeric(ymd_hms("20180611 15:00:00")), linetype=2)+
         scale_x_datetime(limits = lims, labels = date_format("%d-%b")) +
         scale_y_continuous(breaks = axis_labs$rkm_total, labels = axis_labs$site_code) + #changed from Node
         #scale_y_discrete(drop = FALSE) +
         #scale_colour_manual(values = c("Chinook" = "blue", "Bull Trout" = "red")) +
         scale_colour_brewer(palette = 'Set1') +
         #facet_wrap(~tag_code) +
         theme_bw() +
         theme(legend.position = 'bottom',
               text = element_text(size = 18)) +
         labs(x = 'Observation Date',
              y = 'Site (RKM)',
              colour = 'Origin')
     }
   })#, height = 475, width = 700)   

})

# PDF Reports ----
   
# 
# output$reports <- downloadHandler(
#   
#   filename = function(){
#     paste0(gsub(" ","_",input$pdf_reports),
#            "_",
#            format(Sys.time(), "%m_%d_%y_%H%M%S"),
#            ".html")
#   },
#   
#  content = function(file){
#    tempReport <- "documents/2019_Chinook_Bull_report.Rmd"
#    rmarkdown::render(tempReport, output_file = file)
#  }
# )
   
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
