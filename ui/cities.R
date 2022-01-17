page_cities <- dashboardPage(
  title = 'cities',
  header = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body = dashboardBody(
    fluidRow(
      column(
        width = 12,
        box(
          title = '设置',
          width = 12,
          status = 'primary',
          box(
            title = p('评估目标',
                      actionButton("ques_target", "", 
                                   icon = icon("question"),
                                   class = "btn-xs", 
                                   title = "提示",
                                   style = 'position: absolute; right: 10px')),
            width = 4, solidHeader = TRUE, status = "info",
            fluidRow(
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
            ),
            fluidRow(
              column(width = 4,
                     actionBttn(
                       inputId = "addButton",
                       color = 'danger',
                       icon = icon("plus"),
                       size = 'md',
                       label = "疫源地",
                       style = 'unite')),
              column(width = 4,
                     actionBttn(
                       inputId = "confirmed_cities",
                       size = 'md',
                       label = "风险计算",
                       style = 'unite'))
            )
          ),
          box(title = p('疫源地',
                        actionButton("ques_source", "", 
                                     icon = icon("question"),
                                     class = "btn-xs", 
                                     title = "提示",
                                     style = 'position: absolute; right: 10px')
          ),
          width = 8, solidHeader = TRUE, status = "info",
          column(width = 12,
                 dateRangeInput(
                   inputId = "cities_select_date",
                   label = paste0("查询日期", "(百度迁徙更新至：", baidu_date, ")"),
                   start = baidu_date-10,
                   end = baidu_date,
                   max = baidu_date,
                   format = "yyyy/mm/dd"
                 ),
                 hr()
                 ),
          tags$div(id = "Panels"),
          column(width = 12,
                 uiOutput('source_boxs'))
          )
        )
      )
    )
  )
)