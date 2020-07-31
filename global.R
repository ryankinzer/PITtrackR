# Ryan N. Kinzer
# Modified: 5/31/19
# Web application for tracking PIT-tagged Chinook salmon and Bull trout in the Snake River.

library(shiny)
library(shinydashboard)
#library(shinycssloaders)
#library(shinyjs)
library(leaflet)
library(tidyverse)
#library(ggplot2)
library(lubridate)
library(PITcleanr)
library(scales)
library(ggraph)
#library(WriteXLS)
#library(viridis)

#source('R/aws_keys.R') # removed keys for repo and added to gitignore
source('R/queryWindowCnts.R')
#source('R/data_loading_fnc.R')
#load("data/config_data_20190531.rda")
#load("data/config_data_20200612.rda")
load("data/config_data_20200731.rda")

load('data/all_dart_obs.rda')

pit_exp <- read_csv('data/pit_expansion_2020.csv')

# need to order factor levels of nodes and sites
node_vec <- node_order %>%
  arrange(BranchNum, NodeOrder) %>%
  pull(Node)

site_vec <- node_order %>%
  arrange(BranchNum, NodeOrder) %>%
  pull(NodeSite)

site_loc <- my_config %>%
  distinct(SiteID, Latitude, Longitude, RKMTotal)

all_dart_obs <- all_dart_obs %>%
  left_join(site_loc, by = 'SiteID') %>%
  mutate(Node = fct_relevel(Node, node_vec),
         SiteID = fct_relevel(SiteID, site_vec),
         Group = as.character(Group))

# Set amazon pass codes

#setKeys()