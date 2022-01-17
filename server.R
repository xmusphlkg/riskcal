function(input, output, session) {
  
  # sourceDirectory("server", recursive = TRUE)
  source('server/cities.R', local = TRUE, encoding = 'UTF-8')
  source('server/single city.R', local = TRUE, encoding = 'UTF-8')
  
  values <- reactiveValues(df_combind = NULL,
                           df_combind_cities = NULL)
  
  box_input <- reactiveValues()
  temp <- reactiveValues()
  
  observeEvent(input$cname,{
    shinyalert(type = 'success',
               timer = 5000, 
               paste0('欢迎来自', input$cname, '的朋友')
    )
  })
}