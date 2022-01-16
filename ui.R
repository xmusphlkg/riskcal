library(shiny)

navbarPage(
  title = "风险计算器(RiskCal)",
  header = tagList(
    useShinydashboard(),
    setBackgroundColor(color = c("ghostwhite")),
    tags$script(src="http://pv.sohu.com/cityjson?ie=utf-8"),
    tags$script('$( document ).on("shiny:sessioninitialized", function(event) {Shiny.setInputValue("cname",returnCitySN["cname"]);});'),  
    useShinyalert(),
    includeHTML('function/googleanalytics.html')
  ), 
  inverse = TRUE,
  theme = shinythemes::shinytheme(theme = "flatly"), 
  
  tabPanel(
    title = "Home",
    fluidRow(
      column(
        width = 4,
        box(title = '设置',
            width = 12,
            status = "primary",
            column(width = 4,
                   pickerInput(
                     width = '120px',
                     inline = T,
                     inputId = "select_province", 
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
                     inputId = "select_city", 
                     label = "城市", 
                     options = list(
                       style = "btn-success"
                     ),
                     multiple = FALSE,
                     choices = NULL
                   )),
            column(width = 4,
                   pickerInput(
                     width = '120px',
                     inline = T,
                     inputId = "select_direct", 
                     label = "方向", 
                     options = list(
                       style = "btn-success"
                     ),
                     multiple = FALSE,
                     choices = c("迁入" = "move_in", "迁出" = "move_out"),
                     selected = "move_out"
                   )),
            column(width = 12,
                   dateRangeInput(
                     inputId = "select_date",
                     label = paste0("查询日期", "(百度迁徙更新至：", baidu_date, ")"),
                     start = baidu_date-10,
                     end = baidu_date,
                     max = baidu_date,
                     format = "yyyy/mm/dd"
                   )),
            column(width = 12,
                   awesomeRadio(
                     inputId = "version_select",
                     label = "设置模式",
                     choices = c("高级" = 'max',
                                 "基本" = 'base',
                                 "原始" = 'pri'),
                     selected = "base",
                     inline = T,
                     status = "warning"
                   )),
            box(title = "本地病例设置（用于计算风险）",
                width = 12,
                status = 'primary',
                collapsible = T, 
                collapsed = F,
                rHandsontableOutput("datatable_risk", height = '200px')
            ),
            box(title = "全国病例设置（用于地图填色）",
                width = 12,
                status = 'primary',
                collapsible = T, 
                collapsed = F,
                rHandsontableOutput("datatable", height = '200px')
            ),
            column(
              width = 12,
              actionBttn(
                inputId = "confirmed",
                style = 'unite',
                size = 'md',
                label = "风险计算"),
              actionBttn(
                inputId = "confirmed_map",
                style = 'unite',
                size = 'md',
                label = "地图绘制")
            )
        )
      ),
      column(width = 8,
             column(width = 6,
                    box(title = "风险评估",
                        width = 12,
                        status = "warning",
                        plotlyOutput(outputId = 'risk_plot', height = '600px', width = '100%'),
                        downloadBttn('download_bar', 
                                     '结果下载(html)',
                                     style = 'fill',
                                     size = 'sm'),
                        downloadBttn('download_data', 
                                     '数据下载(csv)',
                                     style = 'fill',
                                     size = 'sm'))
             ),
             column(width = 6,
                    box(title = "数据分布(测试)",
                        status = 'info',
                        plotOutput(outputId = 'map_plot', height = '600px', width = '100%'),
                        downloadBttn('download_map',
                                     '结果下载(png)',
                                     style = 'fill',
                                     size = 'sm'),
                        downloadBttn('download_map_pdf',
                                     '结果下载(pdf)',
                                     style = 'fill',
                                     size = 'sm'),
                        width = 12)
             ),
             column(width = 12,
                    box(title = " 迁徙规模指数变化",
                        status = 'info',
                        plotlyOutput(outputId = 'baidu_index_plot', height = '300px', width = '100%'),
                        downloadBttn('download_baidu_curve',
                                     '结果下载(html)',
                                     style = 'fill',
                                     size = 'sm'),
                        downloadBttn('download_baidu_data',
                                     '数据下载(csv)',
                                     style = 'fill',
                                     size = 'sm'),
                        width = 12)
             ))
      
    )
  ),
  tabPanel(
    title = "关于",
    fluidRow(
      column(
        width = 8, offset = 2,
        box(title = '数据来源',
            width = 12,
            tagList(a('百度迁徙', herf = 'https://qianxi.baidu.com/'),
                    br(),
                    a('高德开放平台', herf = 'https://lbs.amap.com/'))
        ),
        box(title = '计算方法',
            width = 12,
            column(12,
                   withMathJax(),
                   includeMarkdown('about.Rmd'))),
        box(title = 'Update log',
            width = 12,
            column(12,
                   withMathJax(),
                   includeMarkdown('updatelog.md'))),
        box(title = "Update Plan",
            width = 12,
            column(12,
                   withMathJax(),
                   includeMarkdown('updateplan.md')))
      )
      
    )
  )
  
)

