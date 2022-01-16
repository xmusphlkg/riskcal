library(shiny)

source('ui/single city.R', encoding = 'utf-8')
source('ui/about.R', encoding = 'utf-8')
source('ui/cities.R', encoding = 'utf-8')

navbarPage(
  title = "风险计算器(RiskCal)",
  header = tagList(
    useShinydashboard(),
    setBackgroundColor(color = c("ghostwhite")),
    tags$script(src="http://pv.sohu.com/cityjson?ie=utf-8"),
    tags$script('$( document ).on("shiny:sessioninitialized", function(event) {Shiny.setInputValue("cname",returnCitySN["cname"]);});'),  
    useShinyalert(force = TRUE),
    # includeHTML('function/googleanalytics.html')
  ), 
  inverse = TRUE,
  theme = shinythemes::shinytheme(theme = "flatly"), 
  
  tabPanel(title = "外溢风险", icon = icon('building'), page_single_city, value = 'single-city'),
  tabPanel(title = "输入风险", icon = icon('city'), page_cities, value = 'cities'),
  tabPanel(title = "关于", icon = icon('question-circle'), page_about, value = 'about')
  
)

