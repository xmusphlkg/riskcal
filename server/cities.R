
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
  
  ## move in -------------------------------------------------------------------
  
  df_history_all <- his_mobility(input$target_select_city, "move_in")
  df_history_curve <- df_history_all %>% 
    filter(date >= baidu_date-30 & date <= baidu_date)
  
  temp$df_move_in <- df_history_all
  
  df_history_curve$date <- format(as.Date(df_history_curve$date, format = '%Y%m%d'), '%Y/%m/%d')
  
  output$baidu_index_plot_cities <- renderPlotly({
    plot_ly(data = df_history_curve, x = ~date, y = ~value,
            type = 'scatter', mode = 'lines', fill = 'tonexty')%>% 
      layout(yaxis = list(title = ""), 
             xaxis = list(title = "",
                          tickformat="%m-%d<br>%Y"))%>%
      layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
             paper_bgcolor = "rgba(0, 0, 0, 0)")
  })
  
  outcome <- list()
  
  date_range <- seq.Date(from = input$cities_select_date[1], 
                         to = input$cities_select_date[2], by = 'day')
  box_input_list <- isolate(reactiveValuesToList(box_input))
  # print(box_input_list)
  module_list <- names(box_input_list)
  city_list <- sapply(module_list, function(x){box_input_list[[x]]$area_id})
  module_no <- str_sub(module_list, -2, -1)
  
  ### remove repeat box input
  repeat_no <- which(duplicated(city_list))
  
  if(length(city_list) == 0){
    shinyalert(title = '没找到疫源地',
               type = 'error',
               timer = 5000, 
               HTML('请点击“加疫源地”按钮并正确设置疫源地后再试~')
    )
  }else{
    ## risk cal ------------------------------------------------------------------
    
    if(length(repeat_no)>0){
      shinyalert(title = '疫源地设置重复，以下疫源地将删除',
                 type = 'warning',
                 timer = 5000, 
                 HTML(paste(sapply(paste0('疫源地', module_no[repeat_no]), 
                                   paste, collapse=":"), 
                            collapse="\n"))
      )
      
      module_list <- module_list[-repeat_no]
      city_list <- city_list[-repeat_no]
    }
    
    # x <- 1
    for (x in 1:length(city_list)) {
      withProgress(message = paste0('疫源地', module_no[x], "数据加载中。。。"),
                   value = 0.2,
                   detail = paste('百度迁徙数据爬取中。。。'),
                   {
                     ## get box input info -----------------------------------------
                     version_select <- box_input_list[[x]]$version
                     area_id <- box_input_list[[x]]$area_id
                     DF <- box_input_list[[x]]$datafile
                     
                     ## get baidu data ---------------------------------------------
                     ### access history data of input
                     df_history_all <- his_mobility(area_id, "move_out")
                     if(length(date_range) != 1){
                       df_history <- df_history_all %>% 
                         filter(date >= input$cities_select_date[1] & date <= input$cities_select_date[2]) %>% 
                         mutate(date = format(date, format = '%Y%m%d'))
                     } else {
                       df_history <- df_history_all %>% 
                         filter(date == date_range) %>% 
                         mutate(date = format(date, format = '%Y%m%d'))
                     }
                     ### access percent data of input
                     
                     df_mobility <- mobility_date(area_id, "move_out", format(date_range, format = '%Y%m%d')) %>% 
                       mutate(value = value / 100)
                     date_miss <- df_mobility[which(is.na(df_mobility$city_name)), 'date']
                     df_mobility <- df_mobility[which(!is.na(df_mobility$city_name)),]
                     
                     incProgress(2/10, detail = paste('百度迁徙数据爬取成功'))
                     
                     if(length(date_miss) > 0){
                       shinyalert(title = paste0('百度迁徙官方原因，疫源地', module_no[x],'以下日期缺失'),
                                  type = 'warning',
                                  timer = 5000, 
                                  paste(sapply(as.Date(date_miss, format = '%Y%m%d'), paste, collapse=":"), 
                                        collapse=" ")
                       )
                     }
                     
                     ## clean baidu data -------------------------------------------
                     
                     incProgress(1/10, detail = paste('百度迁徙数据合并中。。'))
                     
                     if(version_select == 'max'){
                       ### input cases
                       names(DF) <- c('n', 'onset_date', 'isolate_date')
                       
                       DF <- DF %>% 
                         mutate_if(is.character, as.Date)
                       
                       date_count <- lapply(1:nrow(DF), function(i){
                         date_count <- seq(from = DF[i,'onset_date'], to = DF[i,'isolate_date'], by = "days")
                         df <- data.frame(
                           date = date_count,
                           case_number =DF[i, 'n']
                         )
                         return(df)
                       })
                       
                       date_count <- do.call(rbind, date_count) %>% 
                         group_by(date) %>% 
                         mutate(case_number = sum(case_number)) %>% 
                         ungroup() %>% 
                         distinct() %>% 
                         complete(date = seq.Date(                              # ensure all dates are represented
                           from = input$cities_select_date[1],
                           to =input$cities_select_date[2],
                           by = "day"),
                           fill = list(case_number = 0))%>% 
                         mutate(date = format(date, format = '%Y%m%d')) %>% 
                         as.data.frame()
                       
                       df_combind <- df_mobility %>% 
                         left_join(df_history, by = c('date' = 'date')) %>% 
                         left_join(date_count, by = c('date' = 'date')) %>% 
                         mutate(value = value.x * value.y * case_number) %>% 
                         group_by(date) %>% 
                         mutate(percent = as.numeric(value/sum(value, na.rm = T))) %>%
                         ungroup()
                       df_combind$percent[is.nan(df_combind$percent)] <- 0
                       
                       df_combind <- df_combind %>% 
                         select(!c(value.x, value.y, value, case_number)) %>% 
                         pivot_wider(names_from = date, values_from = percent) %>% 
                         as.data.frame() %>%
                         mutate(total = rowSums(.[-c(1:2)], na.rm = T))%>% 
                         select(city_name, province_name, total)
                     }else if(version_select == 'base'){
                       
                       names(DF) <- c('case_number', 'date')
                       
                       date_count <- DF %>% 
                         distinct() %>% 
                         mutate_if(is.character, as.Date) %>% 
                         complete(date = seq.Date(                              # ensure all dates are represented
                           from = input$cities_select_date[1],
                           to =input$cities_select_date[2],
                           by = "day"),
                           fill = list(case_number = 0))%>% 
                         mutate(date = format(date, format = '%Y%m%d')) %>% 
                         as.data.frame()
                       
                       df_combind <- df_mobility %>% 
                         left_join(df_history, by = c('date' = 'date')) %>% 
                         left_join(date_count, by = c('date' = 'date')) %>% 
                         mutate(value = value.x * value.y * case_number) %>% 
                         group_by(date) %>% 
                         mutate(percent = as.numeric(value/sum(value, na.rm = T))) %>%
                         ungroup()
                       df_combind$percent[is.nan(df_combind$percent)] <- 0
                       
                       df_combind <- df_combind %>% 
                         select(!c(value.x, value.y, value, case_number)) %>% 
                         pivot_wider(names_from = date, values_from = percent) %>% 
                         as.data.frame() %>%
                         mutate(total = rowSums(.[-c(1:2)], na.rm = T))%>% 
                         select(city_name, province_name, total)
                     }else if(version_select == 'pri'){
                       
                       df_combind <- df_mobility %>% 
                         full_join(df_history, by = c('date' = 'date')) %>% 
                         group_by(date) %>% 
                         mutate(value = value.x * value.y,
                                percent = value/sum(value, na.rm = T)) %>%
                         ungroup() %>% 
                         select(!c(value.x, value.y, value)) %>% 
                         pivot_wider(names_from = date, values_from = percent) %>% 
                         mutate(total = rowSums(.[-c(1:2)], na.rm = T))%>% 
                         select(city_name, province_name, total)
                     }
                     
                     incProgress(1/10, detail = paste('百度迁徙数据合并成功'))
                     incProgress(1/10, detail = paste('溢出风险计算中。。'))
                     
                     # assign(paste0('datafile_pre_', x), df_combind)
                     df_combind$source <- adcode[adcode$code == city_list[x], 2]
                     outcome[[module_list[x]]] <- df_combind
                     
                   })
    }
    
    ## rbind all datafile
    dfs_combind <- do.call('rbind', outcome)
    rownames(dfs_combind) <- NULL
    dfs_combind <- dfs_combind %>% 
      mutate(percent = as.numeric(total/sum(total, na.rm = T)))
    
    dfs_outcome <- dfs_combind %>% 
      pivot_wider(names_from = source, values_from = percent)
    
    dfs_30 <- dfs_combind %>% 
      group_by(city_name, province_name) %>% 
      summarise(total = sum(total), .groups = 'drop') %>% 
      ungroup() %>% 
      arrange(desc(total)) %>% 
      head(30)
    
    df_plot <- dfs_combind %>% 
      filter(city_name %in% dfs_30$city_name)
    df_plot$city_name <- factor(df_plot$city_name,
                                levels = dfs_30$city_name)
    df_plot$risk <- round(df_plot$percent, 4)*100
    # df_plot$source <- sample(c('A', 'B'), 85, replace = T)
    ## create plot ---------------------------------------------------------------
    
    p <- ggplot(data = df_plot)+
      geom_col(mapping = aes(x = city_name, y = risk, 
                             fill = source, text = city_name))+
      coord_flip()+
      scale_y_continuous(expand = expansion(add = c(0, 0.05*100)))+
      scale_fill_npg()+
      theme_set()+
      labs(x = '',
           y = '溢出风险(%)',
           fill = '疫源地')
    
    output$cities_risk <- renderPlotly(ggplotly(p, tooltip = c('fill', 'y', 'text')))
    
    temp$df_plot <- dfs_outcome
    temp$fig_risk <- p
    
  }
  
})

# download outcome --------------------------------------------------------

observeEvent(input$cities_risk_down,{
  if (!is.null(temp$df_plot)){
    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_riskcal_png",
              "下载风险计算结果(png)",
              icon = icon("file-image"),
              class = 'butt'
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_riskcal_tiff",
              "下载风险计算结果(tiff)",
              icon = icon("file-image"),
              class = 'butt'
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_riskcal_pdf",
              "下载风险计算结果(pdf)",
              icon = icon("file-pdf"),
              class = 'butt'
            )
          )
        ),
        br(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_riskcal_csv",
              "下载风险计算数据(csv)",
              icon = icon("file-excel"),
              class = 'butt'
            )
          )
        ),
        hr(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_riskchange_csv",
              "下载迁入人流数据(csv)",
              icon = icon("file-excel"),
              class = 'butt'
            )
          )
        ),
        title = "下载",
        size = "s",
        footer = list(
          modalButton("取消")
        )
      )
    )
    
    output$download_riskcal_csv <- downloadHandler(
      filename = function(){
        paste0("ctmodelling_toobox", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(temp$df_plot, file, row.names = FALSE)
      }
    )
    
    output$download_riskchange_csv <- downloadHandler(
      filename = function(){
        paste0("ctmodelling_toobox", Sys.Date(), ".csv")
      },
      content = function(file) {
        write.csv(temp$df_move_in, file, row.names = FALSE)
      }
    )
    
    output$download_riskcal_png <- downloadHandler(
      filename = function(){
        paste0("ctmodelling_toobox", Sys.Date(), ".png")
      },
      content = function(file) {
        ggsave(file, plot = temp$fig_risk, height = 8, device = 'png')
      }
    )
    
    output$download_riskcal_tiff <- downloadHandler(
      filename = function(){
        paste0("ctmodelling_toobox", Sys.Date(), ".tiff")
      },
      content = function(file) {
        ggsave(file, plot = temp$fig_risk, height = 8, device = 'tiff')
      }
    )
    
    output$download_riskcal_pdf <- downloadHandler(
      filename = function(){
        paste0("ctmodelling_toobox", Sys.Date(), ".pdf")
      },
      content = function(file) {
        # cairo_pdf(file, bg = "transparent")
        ggsave(file, plot = temp$fig_risk, height = 8, device = cairo_pdf)
        # print(temp$fig_risk)
        # dev.off()
      }
    )
    
  } else {
    shinyalert(title = '没找到数据',
               type = 'error',
               timer = 5000, 
               HTML('请点击“风险计算”按钮并出现流行曲线后再试~')
    )
  }
})

