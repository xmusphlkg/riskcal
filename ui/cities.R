page_cities <- dashboardPage(
  title = 'cities',
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      column(
        width = 4,
        box(
          title = '设置',
          width = 12,
          status = 'primary',
          tags$b('评估目标')%>% 
            helper(icon = "question",
                   colour = "blue",
                   type = "markdown",
                   content = "cities_target"),
          hr(),
          column(width = 4,
                 pickerInput(
                   width = '120px',
                   inline = T,
                   inputId = "target_select_province", 
                   label = "省份", 
                   options = list(
                     style = "btn-success",
                     size = 10
                   ),
                   multiple = FALSE,
                   choices = pccode$name,
                   selected = '北京'
                 )),
          column(width = 4,
                 pickerInput(
                   width = '120px',
                   inline = T,
                   inputId = "target_select_city", 
                   label = "城市", 
                   options = list(
                     style = "btn-success"
                   ),
                   multiple = FALSE,
                   choices = NULL
                 ))
        )
      )
    )
  )
)