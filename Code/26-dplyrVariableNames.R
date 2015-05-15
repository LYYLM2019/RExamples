library(dplyr)
library(lazyeval)

# function to make random table with given column names
makeTable = function(colNames, sampleSize) {
  liSample = lapply(colNames, function(week) {
    sample = rnorm(sampleSize)
  })
  names(liSample) = as.character(colNames)
  return(tbl_df(data.frame(liSample, check.names = FALSE)))
}

# create some sample data with the column name patterns required
weekDates = seq.Date(from = as.Date("2014-01-01"),
                     to = as.Date("2014-08-01"), by = "week")
dfTest = makeDataFrame(weekDates, 10)

# test mutate on this table
dfTest %>% 
  mutate_(sumvar = interp(~ sum(var, na.rm = TRUE), 
                          var = as.name(paste(as.character(weekDates), collapse =","))))

createSum = function(data, variableNames) {
  data %>% 
    mutate_(sumvar = interp(~ sum(var, na.rm = TRUE), 
                            var = as.name(paste(as.character(variableNames), collapse =","))))
  
}

rowSums(dfTest[, as.character(weekDates)])
