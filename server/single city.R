

# update city of province -------------------------------------------------

observeEvent(input$select_province, {
  inputcode <- as.numeric(pccode[pccode$name == input$select_province,2])
  code <- adcode %>%
    filter(code >= inputcode*10^4 & code < (inputcode + 1)*10^4)
  city_select <- code[,3]
  eval(substitute(names(x)<-code[,2], list(x=as.symbol("city_select"))))
  updatePickerInput(
    session = session,
    inputId = "select_city",
    label = "疫源地(城市)",
    options = list(
      style = "btn-success",
      title = "请选择城市",
      size = 10
    ),
    choices = city_select,
    selected = city_select[1]
  )
})


# set input table ---------------------------------------------------------

output$datatable <- renderRHandsontable({
  DF <- data.frame(
    地图省份 = map_data$NAME,
    # 数量 = sample(x = 1:10, size = 34, replace = TRUE)
    数量 = rep(0, 34)
  )
  rhandsontable(DF, language = 'zh-CN', rowHeaderWidth = 50) %>% 
    hot_cols(colWidths = 100) %>% 
    hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
})

observeEvent(input$version_select,{
  
  values$df_combind <- NULL
  
  if(input$version_select == 'max'){
    output$datatable_risk <- renderRHandsontable({
      # DF <- data.frame(
      #   数量 = sample(x = 1:10, size = 5, replace = TRUE),
      #   发病日期 = format(seq(from = Sys.Date()-10, by = "days", length.out = 5),
      #                 '%Y/%m/%d'),
      #   隔离日期 = format(seq(from = Sys.Date()-5, by = "days", length.out = 5),
      #                 '%Y/%m/%d')
      # )
      DF <- data.frame(
        数量 = sample(x = 1:10, size = 5, replace = TRUE),
        发病日期 = seq(from = Sys.Date()-10, by = "days", length.out = 5),
        隔离日期 = seq(from = Sys.Date()-5, by = "days", length.out = 5)
      )
      rhandsontable(DF, language = 'zh-CN') %>% 
        hot_context_menu(allowColEdit = FALSE, allowRowEdit = TRUE) %>% 
        hot_col(c("发病日期", "隔离日期"),
                # dateFormat = "YYYY/MM/DD",
                type = "date",
                language = 'zh-CN')
    })
  } else if(input$version_select == 'base'){
    output$datatable_risk <- renderRHandsontable({
      # DF <- data.frame(
      #   数量 = sample(x = 1:10, size = 5, replace = TRUE),
      #   报告日期 = format(seq(from = Sys.Date()-6, by = "days", length.out = 5),
      #                 '%Y/%m/%d')
      # )
      DF <- data.frame(
        数量 = sample(x = 1:10, size = 5, replace = TRUE),
        报告日期 = seq(from = Sys.Date()-6, by = "days", length.out = 5)
      )
      rhandsontable(DF, language = 'zh-CN') %>% 
        hot_context_menu(allowColEdit = FALSE, allowRowEdit = TRUE)%>% 
        hot_col("报告日期", 
                # dateFormat = "YYYY/MM/DD",
                type = "date",
                language = 'zh-CN')
    })
  } else if(input$version_select == 'pri'){
    output$datatable_risk <- NULL
  }
})

# bar plot ----------------------------------------------------------------

observeEvent(input$confirmed, {
  # print(input$confirmed)
  withProgress(message = "加载中。。。",
               value = 0.2,
               detail = paste('百度迁徙数据爬取中。。。'),
               {
                 df_combind <- values$df_combind
                 if(is.null(df_combind)){
                   date_range <- seq.Date(from = input$select_date[1], to = input$select_date[2], by = 'day')
                   
                   df_mobility <- mobility_date(input$select_city, input$select_direct, format(date_range, format = '%Y%m%d')) %>% 
                     mutate(value = value / 100)
                   date_miss <- df_mobility[which(is.na(df_mobility$city_name)), 'date']
                   df_mobility <- df_mobility[which(!is.na(df_mobility$city_name)),]
                   
                   df_history_all <- his_mobility(input$select_city, input$select_direct)
                   if(length(date_range) != 1){
                     df_history <- df_history_all %>% 
                       filter(date >= input$select_date[1] & date <= input$select_date[2]) %>% 
                       mutate(date = format(date, format = '%Y%m%d'))
                   } else {
                     df_history <- df_history_all %>% 
                       filter(date == date_range) %>% 
                       mutate(date = format(date, format = '%Y%m%d'))
                   }
                   incProgress(2/10, detail = paste('百度迁徙数据爬取成功'))
                   
                   # plot curve ----------------------------------------------------------------
                   
                   df_history_curve <- df_history
                   df_history_curve$date <- format(as.Date(df_history_curve$date, format = '%Y%m%d'), '%Y/%m/%d')
                   index_curve <- function(){
                     plot_ly(data = df_history_curve, x = ~date, y = ~value,
                             type = 'scatter', mode = 'lines', fill = 'tonexty')%>% 
                       layout(yaxis = list(title = ""), 
                              xaxis = list(title = "",
                                           tickformat="%m-%d<br>%Y"))%>%
                       layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                              paper_bgcolor = "rgba(0, 0, 0, 0)")
                   }
                   output$baidu_index_plot <- renderPlotly({
                     index_curve()
                   })
                   
                   
                   # output$download_baidu_curve <- downloadHandler(
                   #   filename = function(){'risk_index.html'},
                   #   content = function(file){
                   #     htmlwidgets::saveWidget(as_widget(index_curve()), file)
                   #   }
                   # )
                   
                   output$download_baidu_data <- downloadHandler(
                     filename = 'risk_index.csv',
                     content = function(file){
                       write.csv(df_history_all, file, row.names = FALSE)
                     }
                   )
                   
                   
                   
                   
                   if(length(date_miss) > 0){
                     shinyalert(title = '百度迁徙官方原因，以下日期缺失',
                                type = 'warning',
                                timer = 5000, 
                                paste(sapply(as.Date(date_miss, format = '%Y%m%d'), paste, collapse=":"), 
                                      collapse=" ")
                     )
                   }
                   
                   incProgress(1/10, detail = paste('百度迁徙数据合并中。。'))
                   # print(df_history)
                   
                   # print(date_count)
                   
                   # print(df_history)
                   if(input$version_select == 'max'){
                     ### input cases
                     DF <- hot_to_r(input$datatable_risk)
                     names(DF) <- c('n', 'onset_date', 'isolate_date')
                     
                     DF <- DF %>% 
                       mutate_if(is.character, as.Date)
                     
                     # print(DF)
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
                       mutate_if(is.character, as.Date) %>% 
                       complete(date = seq.Date(                              # ensure all dates are represented
                         from = input$select_date[1],
                         to =input$select_date[2],
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
                       mutate(total = rowSums(.[-c(1:2)], na.rm = T),
                              total_per = total/sum(total, na.rm = T)) %>% 
                       select(city_name, province_name, total_per) %>% 
                       arrange(desc(total_per))
                   }else if(input$version_select == 'base'){
                     
                     DF <- hot_to_r(input$datatable_risk)
                     names(DF) <- c('case_number', 'date')
                     # print(DF)
                     
                     date_count <- DF %>% 
                       distinct() %>%
                       mutate_if(is.character, as.Date) %>% 
                       complete(date = seq.Date(                              # ensure all dates are represented
                         from = input$select_date[1],
                         to =input$select_date[2],
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
                       mutate(total = rowSums(.[-c(1:2)], na.rm = T),
                              total_per = total/sum(total, na.rm = T)) %>% 
                       select(city_name, province_name, total_per) %>% 
                       arrange(desc(total_per))
                   }else if(input$version_select == 'pri'){
                     
                     df_combind <- df_mobility %>% 
                       full_join(df_history, by = c('date' = 'date')) %>% 
                       group_by(date) %>% 
                       mutate(value = value.x * value.y,
                              percent = value/sum(value, na.rm = T)) %>%
                       ungroup() %>% 
                       select(!c(value.x, value.y, value)) %>% 
                       pivot_wider(names_from = date, values_from = percent) %>% 
                       mutate(total = rowSums(.[-c(1:2)], na.rm = T),
                              total_per = total/sum(total, na.rm = T)) %>% 
                       select(city_name, province_name, total_per) %>% 
                       arrange(desc(total_per))
                   }
                   
                   incProgress(1/10, detail = paste('百度迁徙数据合并成功'))
                   incProgress(1/10, detail = paste('溢出风险计算中。。'))
                   
                   # print(df_combind)
                   values$df_combind <- df_combind
                   # print('百度分析')
                 }
                 
                 # plot bar ----------------------------------------------------------------
                 
                 df_plot <- df_combind %>%
                   head(30)
                 
                 df_plot$city_name <- factor(df_plot$city_name,
                                             levels = unique(df_plot$city_name)[order(df_plot$total_per, decreasing = F)])
                 df_plot$total_per <- round(df_plot$total_per, 4)
                 inputrisk_plot <- function(){
                   plot_ly(
                     df_plot, y= ~city_name, x = ~total_per*100, type = 'bar', orientation = 'h'
                   ) %>%
                     add_text(y= ~city_name, x = ~total_per*100, text=~total_per*100, textposition="right") %>%
                     layout(
                       showlegend = F,
                       xaxis = list(title = '溢出风险(%)', showline= T, linewidth=2, linecolor='black',
                                    range = c(0, (max(df_plot$total_per) + 0.1)*100)),
                       yaxis = list(title ="", showline= T, linewidth=2, linecolor='black')
                     )
                 }
                 output$risk_plot <- renderPlotly({
                   inputrisk_plot()
                 })
                 incProgress(1/10, detail = paste('溢出风险计算成功'))
               })
  output$download_bar <- downloadHandler(
    filename = function(){'risk_rank.html'},
    content = function(file){
      htmlwidgets::saveWidget(as_widget(inputrisk_plot()), file)
    }
  )
  
  output$download_data <- downloadHandler(
    filename = function(){'risk_rank.csv'},
    content = function(file){
      write.csv(df_combind, file, row.names = FALSE)
    }
  )
})

# map plot ----------------------------------------------------------------

observeEvent(input$confirmed_map, {
  withProgress(message = "加载中。。。",
               value = 0.2,
               detail = paste('百度迁徙数据获取中。。。'),
               {
                 df_combind <- values$df_combind
                 if(is.null(df_combind)){
                   date_range <- seq.Date(from = input$select_date[1], to = input$select_date[2], by = 'day')
                   
                   df_mobility <- mobility_date(input$select_city, input$select_direct, format(date_range, format = '%Y%m%d')) %>% 
                     mutate(value = value / 100)
                   date_miss <- df_mobility[which(is.na(df_mobility$city_name)), 'date']
                   df_mobility <- df_mobility[which(!is.na(df_mobility$city_name)),]
                   
                   if(length(date_range) != 1){
                     df_history <- his_mobility(input$select_city, input$select_direct) %>% 
                       filter(date >= input$select_date[1] & date <= input$select_date[2]) %>% 
                       mutate(date = format(date, format = '%Y%m%d'))
                   } else {
                     df_history <- his_mobility(input$select_city, input$select_direct) %>% 
                       filter(date == date_range) %>% 
                       mutate(date = format(date, format = '%Y%m%d'))
                   }
                   incProgress(1/10, detail = paste('百度迁徙数据爬取成功'))
                   
                   # plot curve ----------------------------------------------------------------
                   
                   df_history_curve <- df_history
                   df_history_curve$date <- format(as.Date(df_history_curve$date, format = '%Y%m%d'), '%Y/%m/%d')
                   index_curve <- function(){
                     plot_ly(data = df_history_curve, x = ~date, y = ~value,
                             type = 'scatter', mode = 'lines', fill = 'tonexty')%>% 
                       layout(yaxis = list(title = ""), 
                              xaxis = list(title = "",
                                           tickformat="%m-%d<br>%Y"))%>%
                       layout(plot_bgcolor  = "rgba(0, 0, 0, 0)",
                              paper_bgcolor = "rgba(0, 0, 0, 0)")
                   }
                   output$baidu_index_plot <- renderPlotly({
                     index_curve()
                   })
                   
                   
                   # output$download_baidu_curve <- downloadHandler(
                   #   filename = function(){'risk_index.html'},
                   #   content = function(file){
                   #     htmlwidgets::saveWidget(as_widget(index_curve()), file)
                   #   }
                   # )
                   
                   output$download_baidu_data <- downloadHandler(
                     filename = 'risk_index.csv',
                     content = function(file){
                       write.csv(df_history_all, file, row.names = FALSE)
                     }
                   )
                   
                   
                   if(length(date_miss) > 0){
                     shinyalert(title = '百度迁徙官方原因，以下日期缺失',
                                type = 'warning',
                                timer = 5000, 
                                paste(sapply(as.Date(date_miss, format = '%Y%m%d'), paste, collapse=":"), 
                                      collapse=" ")
                     )
                   }
                   
                   incProgress(1/10, detail = paste('百度迁徙数据合并中。。'))
                   
                   if(input$version_select == 'max'){
                     ### input cases
                     DF <- hot_to_r(input$datatable_risk)
                     names(DF) <- c('n', 'onset_date', 'isolate_date')
                     
                     DF <- DF %>% 
                       mutate_if(is.character, as.Date)
                     
                     # print(DF)
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
                       mutate_if(is.character, as.Date) %>% 
                       complete(date = seq.Date(                              # ensure all dates are represented
                         from = input$select_date[1],
                         to =input$select_date[2],
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
                       mutate(total = rowSums(.[-c(1:2)], na.rm = T),
                              total_per = total/sum(total, na.rm = T)) %>% 
                       select(city_name, province_name, total_per) %>% 
                       arrange(desc(total_per))
                   }else if(input$version_select == 'base'){
                     
                     DF <- hot_to_r(input$datatable_risk)
                     names(DF) <- c('case_number', 'date')
                     # print(DF)
                     
                     date_count <- DF %>% 
                       distinct() %>% 
                       mutate_if(is.character, as.Date) %>% 
                       complete(date = seq.Date(                              # ensure all dates are represented
                         from = input$select_date[1],
                         to =input$select_date[2],
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
                       mutate(total = rowSums(.[-c(1:2)], na.rm = T),
                              total_per = total/sum(total, na.rm = T)) %>% 
                       select(city_name, province_name, total_per) %>% 
                       arrange(desc(total_per))
                   }else if(input$version_select == 'pri'){
                     
                     df_combind <- df_mobility %>% 
                       full_join(df_history, by = c('date' = 'date')) %>% 
                       group_by(date) %>% 
                       mutate(value = value.x * value.y,
                              percent = value/sum(value, na.rm = T)) %>%
                       ungroup() %>% 
                       select(!c(value.x, value.y, value)) %>% 
                       pivot_wider(names_from = date, values_from = percent) %>% 
                       mutate(total = rowSums(.[-c(1:2)], na.rm = T),
                              total_per = total/sum(total, na.rm = T)) %>% 
                       select(city_name, province_name, total_per) %>% 
                       arrange(desc(total_per))
                   }
                   
                   incProgress(1/10, detail = paste('百度迁徙数据合并成功'))
                   incProgress(1/10, detail = paste('溢出风险计算中。。'))
                   
                   # print(df_combind)
                   values$df_combind <- df_combind
                   # print('百度分析')
                 }
                 
                 # plot map ----------------------------------------------------------------
                 incProgress(1/10, detail = paste('百度迁徙数据获取成功'))
                 df_map <- df_combind %>% 
                   left_join(adcode, by = c('city_name' = 'name'))
                 
                 incProgress(2/10, detail = paste('地理编码数据整理中。。'))
                 start_point <- adcode[adcode$code == input$select_city,]
                 
                 df_map$lng_start <- start_point[1, 'lng']
                 df_map$lat_start <- start_point[1, 'lat']
                 df_map$value <- df_combind$total_per
                 df_map$province <- df_combind$province_name
                 
                 df_map$lng <- as.numeric(df_map$lng)
                 df_map$lat <- as.numeric(df_map$lat)
                 # print(df_map$city_name)
                 # print(df_map)
                 data_ready_plot=data.frame()
                 for(i in c(1:nrow(df_map))){
                   tmp=data_for_connection(df_map$lng_start[i], df_map$lat_start[i], 
                                           df_map$lng[i], df_map$lat[i] , i)
                   tmp$province=df_map$province[i]
                   tmp$n=df_map$value[i]
                   data_ready_plot=rbind(data_ready_plot, tmp)
                   # print(df_map$city_name[i])
                 }
                 # print(df_map)
                 data_ready_plot$province <- as.factor(data_ready_plot$province)
                 
                 incProgress(1/10, detail = paste('地理编码数据整理成功'))
                 incProgress(0, detail = paste('地图绘图准备中。。'))
                 
                 # print(data_ready_plot)
                 colourCount = length(unique(df_map$province))
                 
                 DF <- hot_to_r(input$datatable)
                 names(DF) <- c('NAME', 'value')
                 
                 sf_data <- map_data %>%
                   left_join(
                     DF
                   )
                 
                 sf_df <- df_map %>%
                   st_as_sf(coords = c("lng", "lat"), crs = 4326) %>%
                   st_transform(crs = st_crs(map_data))%>%
                   cbind(st_coordinates(.))
                 
                 sf_line <- data_ready_plot %>%
                   st_as_sf(coords = c("lon", "lat"), crs = 4326) %>%
                   st_transform(crs = st_crs(map_data))%>%
                   cbind(st_coordinates(.))
                 
                 incProgress(1/10, detail = paste('地图绘图准备完成'))
                 incProgress(0, detail = paste('地图绘图中。。'))
                 
                 plot_map <- ggplot() +
                   ## 填色
                   geom_sf(data = sf_data, aes(fill = value), alpha = 0.3, show.legend = T)+
                   geom_sf(data = border_data, color = 'black', show.legend = F)+
                   ## 加点
                   geom_point(data = sf_df, aes(x=X, y=Y, size=value*200), color = "#D55E00", show.legend = F)+
                   ## 加线
                   # geom_line(data = sf_line, aes(x=X, y=Y, group=group,
                   #                               alpha = 0.5, colour = province, size = n*30)) +
                   # scale_fill_manual(values = colorRampPalette(brewer.pal(12, "Paired"))(colourCount),
                   #                   na.value = "Gray78") +
                   geom_line(data = sf_line, aes(x=X, y=Y, group=group,
                                                 alpha = 0.4, size = n*30), color = "#F0E442", show.legend = F) +
                   scale_fill_distiller(palette = "YlOrRd", na.value = 'white', direction = 1)+
                   theme_void() +
                   theme(
                     legend.position = c(0.1, 0.2),
                     panel.background = element_rect(fill = "white", colour = "white"),
                     panel.spacing=unit(c(0,0,0,0), "null"),
                     plot.margin=grid::unit(c(0,0,0,0), "cm"),
                   )+
                   guides(size = "none", alpha = "none", fill = guide_legend(override.aes = list(alpha = 0.3),
                                                                             title = "Cases"))+
                   coord_sf(expand = F)
                 
                 output$map_plot <- renderPlot({
                   plot_map
                 })
                 incProgress(1/10, detail = paste('服务器处理完成'))
                 
                 output$download_map <- downloadHandler(
                   filename =  paste0("ctmodelling_toobox", Sys.Date(), ".png"),
                   content = function(file) {
                     ggsave(file, plot = plot_map, device = 'png')
                   }
                 )
                 output$download_mapplot_tiff <- downloadHandler(
                   filename =  paste0("ctmodelling_toobox", Sys.Date(), ".tiff"),
                   content = function(file) {
                     grDevices::tiff(file, bg = "transparent")
                     print(plot_map)
                     dev.off()
                     # ggsave(file, plot = plot_map, device = 'tiff')
                   }
                 )
                 output$download_map_pdf <- downloadHandler(
                   filename =  paste0("ctmodelling_toobox", Sys.Date(), ".pdf"),
                   content = function(file) {
                     ggsave(file, plot = plot_map, device = cairo_pdf)
                   }
                 )
               })
  
})


observeEvent(input$risk_down,{
  if (!is.null(values$df_combind)){
    showModal(
      modalDialog(
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_bar",
              "下载风险计算结果(html)",
              icon = icon("file-code"),
              class = 'butt'
            )
          )
        ),
        br(),
        # fluidRow(
        #   column(
        #     width = 12,
        #     align = "center",
        #     downloadButton(
        #       "download_riskcal_png",
        #       "下载风险计算结果(png)",
        #       icon = icon("file-image"),
        #       class = 'butt'
        #     )
        #   )
        # ),
        # br(),
        # fluidRow(
        #   column(
        #     width = 12,
        #     align = "center",
        #     downloadButton(
        #       "download_riskcal_tiff",
        #       "下载风险计算结果(tiff)",
        #       icon = icon("file-image"),
        #       class = 'butt'
        #     )
        #   )
        # ),
        # br(),
        # fluidRow(
        #   column(
        #     width = 12,
        #     align = "center",
        #     downloadButton(
        #       "download_riskcal_pdf",
        #       "下载风险计算结果(pdf)",
        #       icon = icon("file-pdf"),
        #       class = 'butt'
        #     )
        #   )
        # ),
        # br(),
        fluidRow(
          column(
            width = 12,
            align = "center",
            downloadButton(
              "download_data",
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
              "download_baidu_data",
              "下载迁出人流数据(csv)",
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
              "download_map",
              "下载地图(png)",
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
              "download_mapplot_tiff",
              "下载地图(tiff)",
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
              "download_map_pdf",
              "下载地图(pdf)",
              icon = icon("file-pdf"),
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
    
  } else {
    shinyalert(title = '没找到数据',
               type = 'error',
               timer = 5000, 
               HTML('请点击“风险计算”按钮并出现流行曲线后再试~')
    )
  }
})