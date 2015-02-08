library(dplyr)
library(ggplot2)

flights = tbl_df(read.csv("dplyr-tutorial/Data/flights.csv", stringsAsFactors = FALSE))
flights$date = as.Date(flights$date)

weather = tbl_df(read.csv("dplyr-tutorial/Data/weather.csv", stringsAsFactors = FALSE))
weather$date = as.Date(weather$date)
planes = tbl_df(read.csv("dplyr-tutorial/Data/planes.csv", stringsAsFactors = FALSE))
airports = tbl_df(read.csv("dplyr-tutorial/Data/airports.csv", stringsAsFactors = FALSE))

flights
weather
planes
airports
