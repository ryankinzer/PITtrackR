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
#library(lubridate)
library(PITcleanr)
library(scales)
library(ggraph)
#library(WriteXLS)
#library(viridis)

#source('R/aws_keys.R') # removed keys for repo and added to gitignore
source('R/queryWindowCnts.R')
source('R/data_loading_fnc.R')



node_order <<- readRDS('./data/node_order.rds')
pc_nodes <- readRDS('./data/pc_nodes.rds')  
load('data/dat_all.rda')
print('dat_all loaded')
