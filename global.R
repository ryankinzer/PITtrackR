# Ryan N. Kinzer
# Modified: 5/31/19
# Web application for tracking PIT-tagged Chinook salmon and Bull trout in the Snake River.

library(shiny)
library(shinydashboard)
library(shinycssloaders)
library(leaflet)
library(tidyverse)
library(ggplot2)
library(lubridate)
library(PITcleanr)
library(scales)
library(WriteXLS)
#library(viridis)

#source('scripts/nodeEfficiency.R')
#source('scripts/nodeDetectionEvent.R')
#source('scripts/estimateSpawnLoc.R')
source('scripts/aws_keys.R')
source('scripts/data_loading_fnc.R')
load("data/config_data_20190531.rda")
#load('./data/DABOM_map_data.rda')

site_loc <- my_config %>%
  distinct(SiteID, Latitude, Longitude)
