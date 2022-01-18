box_input <- reactiveValues(
  sourceModel0001 = list(
    area_id = '110000',
    version = 'base',
    datafile = data.frame(
      数量 = sample(x = 1:10, size = 5, replace = TRUE),
      报告日期 = format(seq(from = Sys.Date()-6, by = "days", length.out = 5),
                    '%Y/%m/%d')
    )
  ),
  sourceModel0002 = list(
    area_id = '130400',
    version = 'max',
    datafile = data.frame(
      数量 = sample(x = 1:10, size = 5, replace = TRUE),
      发病日期 = format(seq(from = Sys.Date()-10, by = "days", length.out = 5),
                    '%Y/%m/%d'),
      隔离日期 = format(seq(from = Sys.Date()-5, by = "days", length.out = 5),
                    '%Y/%m/%d')
    )
  ),
  sourceModel0003 = list(
    area_id = '150800',
    version = 'pri',
    datafile = data.frame(
      数量 = sample(x = 1:10, size = 5, replace = TRUE),
      发病日期 = format(seq(from = Sys.Date()-10, by = "days", length.out = 5),
                    '%Y/%m/%d'),
      隔离日期 = format(seq(from = Sys.Date()-5, by = "days", length.out = 5),
                    '%Y/%m/%d')
    )
  ),
  sourceModel0004 = list(
    area_id = '150800',
    version = 'pri',
    datafile = data.frame(
      数量 = sample(x = 1:10, size = 5, replace = TRUE),
      发病日期 = format(seq(from = Sys.Date()-10, by = "days", length.out = 5),
                    '%Y/%m/%d'),
      隔离日期 = format(seq(from = Sys.Date()-5, by = "days", length.out = 5),
                    '%Y/%m/%d')
    )
  )
)

date_range <- seq.Date(from = Sys.Date()-10, 
                       to = Sys.Date(), by = 'day')

df_history <- df_history_all %>% 
  filter(date >= date_range[1] & date <= max(date_range)) %>% 
  mutate(date = format(date, format = '%Y%m%d'))


input <- list(cities_select_date = c(as.Date(Sys.Date()-10), as.Date(Sys.Date())))
