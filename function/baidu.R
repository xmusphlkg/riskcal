# get percent -------------------------------------------------------------

mobility_get <- function(area_id, direction, date_input){
  
  if(area_id %in% c(110000, 310000, 500000, 120000)){
    class_name <- "province"
  } else {
    class_name <- "city"
  }
  
  url <- paste0(
    'http://huiyan.baidu.com/migration/cityrank.jsonp?dt=', class_name,
    '&id=', area_id,
    "&type=", direction,
    "&date=", date_input
  )
  
  huiyan_df <- fromJSON(
    paste(readLines(url,warn = F, encoding = 'unicode'), collapse = "") %>% 
      str_remove("cb") %>% 
      str_remove_all("\\(|\\)")
  )
  
  if(length(huiyan_df$data$list) == 0){
    huiyan_df <- data.frame(
      city_name = NA,
      province_name = NA,
      value = NA
    )
  } else {
    huiyan_df <- huiyan_df$data$list
  }
  
  huiyan_df$date <- date_input
  # huiyan_df$city <- area_id
  return(huiyan_df)
}

mobility_date <- function(area_id, direction, daterange){
  
  huiyan_combind <- lapply(daterange, mobility_get,
                           area_id = area_id,
                           direction = direction)
  huiyan_ln <- do.call('rbind', huiyan_combind)
  
  return(huiyan_ln)
}

# get history data --------------------------------------------------------

his_mobility <- function(area_id, direction){

  if(area_id %in% c(110000, 310000, 500000, 120000)){
    class_name <- "province"
  } else {
    class_name <- "city"
  }
  
  url <- paste0(
    "https://huiyan.baidu.com/migration/historycurve.jsonp?dt=", class_name,
    "&id=", area_id,
    "&type=", direction
  )
  
  huiyan_ln <- fromJSON(
    paste(readLines(url,warn = F, encoding = 'unicode'), collapse = "") %>% 
      str_remove("cb") %>% 
      str_remove_all("\\(|\\)")
  )
  huiyan_ln <- data.frame(
    date = as.Date(names(huiyan_ln$data$list), format = '%Y%m%d'),
    value = as.numeric(huiyan_ln$data$list)
  )
  # huiyan_ln$city <- area_id
  return(huiyan_ln)
}

## update date
baidu_date <- fromJSON(
  paste(readLines('https://huiyan.baidu.com/migration/lastdate.json',warn = F, encoding = 'unicode'), collapse = "") %>% 
    str_remove("cb") %>% 
    str_remove_all("\\(|\\)")
)
baidu_date <- as.Date(baidu_date$data$lastdate, format = '%Y%m%d')
