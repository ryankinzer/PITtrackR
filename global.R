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

source('R/aws_keys.R')
source('R/queryWindowCnts.R')
source('R/data_loading_fnc.R')
load("data/config_data_20190531.rda")

site_loc <- my_config %>%
  distinct(SiteID, Latitude, Longitude)

# Set amazon pass codes

setKeys()