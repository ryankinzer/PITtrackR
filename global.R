# Ryan N. Kinzer
# Modified: 5/31/19
# Web application for tracking PIT-tagged Chinook salmon and Bull trout in the Snake River.

library(shiny)
library(shinydashboard)
#library(shinycssloaders)
#library(shinyjs)
library(leaflet)
library(tidyverse)
#library(dplyr)
#library(ggplot2)
library(lubridate)
library(PITcleanr)
library(scales)
library(ggraph)
#library(WriteXLS)
#library(viridis)

#source('R/aws_keys.R') # removed keys for repo and added to gitignore
source('R/queryWindowCnts.R')
source('R/data_loading_fnc.R')

# Get site config data
#load("data/config_data_20190531.rda")
#load("data/config_data_20200612.rda")
#load("data/config_data_20200731.rda")
load('data/site_config_GRA.rda')

# need to order factor levels of nodes and sites

site_loc <- configuration %>%
  distinct(site_code, rkm_total, geometry)

site_order <- PITcleanr::buildNodeOrder(parent_child)

site_grp <- site_order %>%
  mutate(grp = stringr::str_split(path, ' ', simplify = TRUE)[,2],
         grp = ifelse(grp == '', 'GRA', grp))

site_grp$grp <- sapply(site_grp$grp, switch,
  'GRA' = 'LGR',
  'ACM' = 'Asotin Creek',
  'ALPOWC' = 'Asotin Creek',
  'BRC' = 'Middle Fork',
  'CLC' = 'SF Clearwater',
  'COC' = 'Imnaha River',
  'CRC' = 'Lemhi River',
  'DWL' = 'Hatcheries',
  'GRS' = 'Downstream LGR',
  'IR1' = 'Imnaha River',
  'JA1' = 'Jacks Creek',
  'JOC' = 'Joseph Creek',
  'JUL' = 'Potlatch Creek',
  'LAP' = 'Lapwai Creek',
  'LC1' = 'Lolo Creek',
  'LLR' = 'Lemhi River',
  'LOOH' = 'Lookingglass',
  'LRL' = 'Lochsa River',
  'MAR' = 'Middle Fork',
  'NFS' = 'NF Salmon',
  'NPTH' = 'Hatcheries',
  'OXBO' = 'Hatcheries',
  'PCA' = 'Panther Creek',
  'RAPH' = 'Rapid River',
  'SC1' = 'SF Clearwater',
  'SFG' = 'SF Salmon',
  'SW1' = 'Selway River',
  'TAY' = 'Middle Fork',
  'TENMC2' = 'Asotin Creek',
  'UGR' = 'Grande Ronde',
  'USE' = 'Salmon River',
  'WB1' = 'White Bird',
  'WEN' = 'Wenaha',
  'WR1' = 'Wallowa')

configuration <- left_join(configuration, site_grp,
                 by = c('site_code' = 'node'))

site_vec <- site_order %>%
  arrange(node_order, node) %>%
  pull(node)

node_order <- PITcleanr::buildNodeOrder(pc_nodes)

node_vec <- node_order %>%
  arrange(node_order, node) %>%
  pull(node)

# Get Dart obs for most recent year and add to previous years.

# tmp_ls <- processDART_LGR(species = 'Chinook',
#                           spawnYear = 2020,
#                           configuration = my_config,
#                           truncate = T)
# 
# tmp_mark <- tmp_ls$dart_obs %>%
#   select(tag_id, mark_date, file_id, mark_site, rel_site, rel_date,
#          t_rear_type, t_species, t_run, length, trans_status,
#          trans_proj, trans_year) %>%
#   distinct()
# 
# dat_all <<- tmp_ls$proc_ch %>%
#   left_join(site_loc, by = 'SiteID') %>%
#   mutate(Node = fct_relevel(Node, node_vec),
#          SiteID = fct_relevel(SiteID, site_vec),
#          Group = as.character(Group)) %>%
#   left_join(tmp_mark, by = c('TagID' = 'tag_id')) %>%
#   mutate(species = input$basin_spp,
#          spawn_yr = input$rtn_year) %>%
#   ungroup()

#load('data/all_dart_obs.rda')

#pit_exp <- read_csv('data/pit_expansion_2020.csv')

# all_dart_obs <- all_dart_obs %>%
#   left_join(site_loc, by = 'SiteID') %>%
#   mutate(Node = fct_relevel(Node, node_vec),
#          SiteID = fct_relevel(SiteID, site_vec),
#          Group = as.character(Group))

# Set amazon pass codes

#setKeys()