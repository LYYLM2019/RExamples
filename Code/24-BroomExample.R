
library(dplyr)
library(broom)

df.h = data.frame(
  hour     = factor(rep(1:24, each = 21)),
  price    = runif(504, min = -10, max = 125),
  wind     = runif(504, min = 0, max = 2500),
  temp     = runif(504, min = - 10, max = 25)
)

dfHour = df.h %>% group_by(hour) %>%
  do(fitHour = lm(price ~ wind + temp, data = .))

# get the coefficients by group in a tidy data_frame
dfHourCoef = tidy(dfHour, fitHour)
dfHourCoef

# get the predictions by group in a tidy data_frame
dfHourPred = augment(dfHour, fitHour)
dfHourPred

# get the summary statistics by group in a tidy data_frame
dfHourSumm = glance(dfHour, fitHour)
dfHourSumm