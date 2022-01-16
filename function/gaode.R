key <-  '11eb5fc7a625d45cf34172a043eda0c3'

gaodemap <- function(x){
  address <- stringr::str_replace_all(x, "[^[:alnum:]]", "")
  url <- paste0(
    'https://restapi.amap.com/v3/geocode/geo?',
    '&key=', key, ## key
    '&address=', address, ## 详细地址
    '&output=', 'JSON'
  )
  temp_geo <- fromJSON(paste(readLines(url,warn = F, encoding = 'UTF-8'), collapse = ""))
  status <- temp_geo$status ## 判断查询是否成功
  if (status == 1 & length(temp_geo$geocodes) != 0){ ## 有时候成功了但是没有收到数据，所以用了一个“且”进行判定
    temp <- paste(x, temp_geo$geocodes$location, sep = ",")
  } else {
    temp <- paste(x, 'NULL,NULL', sep = ",")
  }
  return(temp)
}