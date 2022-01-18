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
                       icon = icon("plus"),
                       style = 'simple',
                       color = 'primary',
                       label = "加疫源地")),
              column(width = 4,
                     actionBttn(
                       inputId = "confirmed_cities",
                       label = "风险计算",
                       style = 'simple',
                       color = 'primary',
                       icon = icon('power-off'))),
              column(width = 4,
                     actionBttn(
                       inputId = 'cities_risk_down',
                       label = '下载结果',
                       style = 'simple',
                       color = 'primary',
                       icon = icon('download'))
                     )
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
      ),
      column(width = 12,
             box(title = "风险评估",
                 width = 4,
                 status = "warning",
                 plotlyOutput(outputId = 'cities_risk', height = '600px', width = '100%')),
             box(title = "迁徙规模指数变化",
                 status = 'info',
                 width = 8,
                 plotlyOutput(outputId = 'baidu_index_plot_cities', height = '300px', width = '100%')
             ))
    )
  )
)