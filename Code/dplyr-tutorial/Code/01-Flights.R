library(dplyr)
library(ggplot2)
library(lubridate)

# read in all the data
flights = tbl_df(read.csv("dplyr-tutorial/Data/flights.csv", stringsAsFactors = FALSE))
flights$date = as.Date(flights$date)
weather = tbl_df(read.csv("dplyr-tutorial/Data/weather.csv", stringsAsFactors = FALSE))
weather$date = as.Date(weather$date)
planes = tbl_df(read.csv("dplyr-tutorial/Data/planes.csv", stringsAsFactors = FALSE))
airports = tbl_df(read.csv("dplyr-tutorial/Data/airports.csv", stringsAsFactors = FALSE))

# Exercises: filter 
flightsS1 = filter(flights, dest %in% c("SFO", "OAK"))
flightsS2 = filter(flights, month(date) == 1)
flightsS3 = filter(flights, arr_delay > 60)
flightsS4 = filter(flights, arr_delay > 2*dep_delay)
flightsS5 = filter(flights, hour >= 0 & hour < 5)

# Exercises: select
select(flights, date, ends_with("delay"))
select(flights, date, contains("delay"))
select(flights, date, matches("delay$"))

# Exercises: arrange
arrange(flights, date, hour, minute)
arrange(flights, desc(arr_delay))
arrange(flights, arr_delay - dep_delay)

# Exercises: mutate
flightsS6 = mutate(flights, mph = dist/(time/60))
range(flightsS6$mph, na.rm = TRUE)
mutate(flights, gained = arr_delay - dep_delay)
mutate(flights, hour_new = dep %/% 100, minute_new = dep %% 100)

# Exercises: pipelines
flights %>% 
  filter(!is.na(dep_delay)) %>%
  group_by(date) %>% 
  summarize(mean = mean(dep_delay),
            max = max(dep_delay),
            min = min(dep_delay),
            q90 = quantile(dep_delay, 0.9),
            numflights = n()) %>%
  filter(numflights > 10)

flights %>% 
  filter(!is.na(arr_delay - dep_delay)) %>%
  group_by(dest) %>%
  summarize(avgdelay = mean(arr_delay - dep_delay), numflights = n()) %>%
  arrange(desc(avgdelay)) %>%
  filter(numflights > 10)

flights %>%
  group_by(carrier, flight, dest) %>%
  summarize(n = n_distinct(date)) %>%
  group_by() %>%
  arrange(desc(n))
  