page_about <- dashboardPage(
  title   = "about",
  header  = dashboardHeader(disable = TRUE),
  sidebar = dashboardSidebar(disable = TRUE),
  body    = dashboardBody(
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