
# add box dynamic ---------------------------------------------------------

inputUI <- function(id) {
  ns <- NS(id)
  uiOutput(ns("sourceModel"))
}

inputModelModule <- function(input, output, session) {
  
  temp$outcome <- NULL
  ### update table input
  tableModel <- reactive({
  # observeEvent(input$version_select_1, {
    if(input$version_select_1 == 'max'){
      DF <- data.frame(
        数量 = sample(x = 1:10, size = 5, replace = TRUE),
        发病日期 = format(seq(from = Sys.Date()-10, by = "days", length.out = 5),
                      '%Y/%m/%d'),
        隔离日期 = format(seq(from = Sys.Date()-5, by = "days", length.out = 5),
                      '%Y/%m/%d')
      )
      outcome <- rhandsontable(DF, language = 'zh-CN') %>% 
        hot_context_menu(allowColEdit = FALSE, allowRowEdit = TRUE) %>% 
        hot_col(c("发病日期", "隔离日期"), dateFormat = "YYYY/MM/DD", type = "date",
                language = 'zh-CN')
      
    } else if(input$version_select_1 == 'base'){
      DF <- data.frame(
        数量 = sample(x = 1:10, size = 5, replace = TRUE),
        报告日期 = format(seq(from = Sys.Date()-6, by = "days", length.out = 5),
                      '%Y/%m/%d')
      )
      outcome <- rhandsontable(DF, language = 'zh-CN') %>% 
        hot_context_menu(allowColEdit = FALSE, allowRowEdit = TRUE)%>% 
        hot_col("报告日期", dateFormat = "YYYY/MM/DD", type = "date",
                language = 'zh-CN')
      
    } else if(input$version_select_1 == 'pri'){
      outcome <- NULL
    }
    outcome
    # temp$outcome <- outcome
  })
  
  ### update city select
  observeEvent(input$select_province_1, {
    inputcode <- as.numeric(pccode[pccode$name == input$select_province_1,2])
    code <- adcode %>%
      filter(code >= inputcode*10^4 & code < (inputcode + 1)*10^4)
    city_select <- code[,3]
    eval(substitute(names(x)<-code[,2], list(x=as.symbol("city_select"))))
    updatePickerInput(
      session = session,
      inputId = "select_city_1",
      label = "城市",
      options = list(
        style = "btn-success",
        title = "请选择城市",
        size = 10
      ),
      choices = city_select,
      selected = city_select[1]
    )
  })
  
  ### partial data write to server
  
  listen_input <- reactive({
    list(input$select_city_1, input$version_select_1, input$datatable_risk_1)
  })

  observeEvent(listen_input(), {
    ns <- session$ns
    id <- environment(ns)[['namespace']]

    box_input[[id]] <- list(
      area_id = input$select_city_1,
      version = input$version_select_1,
      datafile = hot_to_r(input$datatable_risk_1)
    )
  })
  
  ### UI setting
  output[['sourceModel']] <- renderUI({
    ns <- session$ns
    id <- environment(ns)[['namespace']]
    
    output$datatable_risk_1 <- renderRHandsontable(tableModel())
    
    tags$div(id = id,
             tagList(
               box(
                 status = 'warning',
                 title = p('疫源地 ',
                           str_sub(id, -2, -1),
                           actionButton(ns('deleteButton'), '',
                                        icon = icon('times'),
                                        class = "btn-xs", 
                                        title = "提示",
                                        style = 'position: absolute; right: 10px')),
                 width = 4,
                 fluidRow(
                   column(width = 6,
                          pickerInput(
                            width = '120px',
                            inline = T,
                            inputId = ns("select_province_1"), 
                            label = "省份", 
                            options = list(
                              style = "btn-success",
                              size = 10
                            ),
                            multiple = FALSE,
                            choices = pccode$name,
                            selected = '北京'
                          )),
                   column(width = 6,
                          pickerInput(
                            width = '120px',
                            inline = T,
                            inputId = ns("select_city_1"), 
                            label = "城市", 
                            options = list(
                              style = "btn-success"
                            ),
                            multiple = FALSE,
                            choices = NULL
                          )),
                 ),
                 column(width = 12,
                        awesomeRadio(
                          inputId = ns("version_select_1"),
                          label = "设置模式",
                          choices = c("高级" = 'max',
                                      "基本" = 'base',
                                      "原始" = 'pri'),
                          selected = "base",
                          inline = T,
                          status = "warning"
                        )
                 ),
                 column(width = 12,
                        rHandsontableOutput(paste0(id,'-datatable_risk_1'))
               )
             )
             )
    )
  })
}

remove_shiny_inputs <- function(id, .input) {
  invisible(
    lapply(grep(id, names(.input), value = TRUE), function(i) {
      .subset2(.input, "impl")$.values$remove(i)
    })
  )
  
  .subset2(box_input, "impl")$.values$remove(id)
}

observeEvent(input$addButton, {
  i <- sprintf('%04d', input$addButton)
  id <- sprintf('sourceModel%s', i)
  insertUI(
    selector = '#Panels',
    where = "beforeEnd",
    ui = inputUI(id)
  )
  callModule(inputModelModule, id)
  observeEvent(input[[paste0(id, '-deleteButton')]], {
    removeUI(selector = sprintf('#%s', id))
    remove_shiny_inputs(id, input)
  })
})



# show notice -------------------------------------------------------------

observeEvent(input$ques_target,{
  showModal(modalDialog(
    withMathJax(),
    includeMarkdown('helpfiles/cities_target.md'),
    easyClose = TRUE
  ))
})

observeEvent(input$ques_source,{
  showModal(modalDialog(
    withMathJax(),
    includeMarkdown('helpfiles/cities_source.md'),
    easyClose = TRUE
  ))
})


# update city of target -------------------------------------------------

observeEvent(input$target_select_province, {
  inputcode <- as.numeric(pccode[pccode$name == input$target_select_province,2])
  code <- adcode %>%
    filter(code >= inputcode*10^4 & code < (inputcode + 1)*10^4)
  city_select <- code[,3]
  eval(substitute(names(x)<-code[,2], list(x=as.symbol("city_select"))))
  updatePickerInput(
    session = session,
    inputId = "target_select_city",
    label = "城市",
    options = list(
      style = "btn-success",
      title = "请选择城市",
      size = 10
    ),
    choices = city_select,
    selected = city_select[1]
  )
})


# server ------------------------------------------------------------------

observeEvent(input$confirmed_cities,{
  date_range <- seq.Date(from = input$cities_select_date[1], 
                         to = input$cities_select_date[2], by = 'day')
  print(isolate(box_input[['sourceModel0001']]))
})



