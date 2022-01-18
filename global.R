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

library(ggsci)
library(Cairo)
# library(showtext)

# library(lubridate)
library(extrafont)

suppressWarnings(font_import(paths = './fonts/', pattern = "Times", prompt = F))


theme_set <- function(){
     theme_classic()+
          theme(legend.position = c(0.9, 0.9),
                plot.title = element_text(face = "bold", size = 18, vjust = 1,
                                          family = "Times_New_Roman", hjust = 0.5),
                legend.text = element_text(face = 'bold', size = 12, family = 'Times_New_Roman'),
                legend.title = element_text(face = 'bold', size = 12, family = 'Times_New_Roman'),
                legend.box.background = element_rect(fill = "transparent", colour = 'transparent'),
                legend.background = element_rect(fill = "transparent", colour = 'transparent'),
                axis.title.x = element_text(face = 'bold', size = 12, family = 'Times_New_Roman'),
                axis.title.y = element_text(face = 'bold', size = 12, family = 'Times_New_Roman'),
                axis.text.x = element_text(size = 12, family = 'Times_New_Roman'),
                axis.text.y = element_text(size = 12, family = 'Times_New_Roman'),
                plot.margin = margin(5, 35, 5, 5))
}