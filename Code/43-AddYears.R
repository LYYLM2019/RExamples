library(readr)
library(lubridate)
library(dplyr)

dfX = read_table("Category    date
desktop     2017-12-25
tablet      2016-05-13
desktop     2018-06-01
desktop     2017-08-06
tablet      2015-12-31",
                 col_types = list(col_character(), col_date()))


dfX %>%
  mutate(date_new = ifelse(Category == "desktop", date + dyears(2), 
                           ifelse(Category == "tablet", date + dyears(1), date + dyears(1))))