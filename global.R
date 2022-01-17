library(jsonlite)
library(tidyverse)
library(openxlsx)
library(plotly)
library(shiny)
library(shinyWidgets)
library(shinythemes)
library(rhandsontable)
library(shinydashboard)

library(shinyalert)

library(geosphere)
library(sf)
# library(ggspatial)
library(RColorBrewer)

source('function/baidu.R')
# source('function/gaode.R')
source('function/connection.R')

load('data/source.RData')
# adcode <- read.xlsx('data/id.xlsx', sheet = "Sheet1", colNames = F)
# pccode <- read.xlsx('data/id.xlsx', sheet = "Sheet2", colNames = F)
# names(adcode) <- c('id', 'name', 'code')
# names(pccode) <- c('name', 'code')
# 
# 
# ## 省界
# map_data<-st_read("data/province/province.shp")[,c('NAME', 'geometry')]
# ## 国界
# border_data<-st_read("data/province/border.shp")[,c('geometry')]

## 20220116

# library(shinyhelper)
