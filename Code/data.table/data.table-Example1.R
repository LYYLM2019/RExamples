#====================================================================
# purpose: follow along to the advanced data.table tutorial from UseR!2013
# comments: 
# 1. get two random rows by group and set the value of a column for that group
#====================================================================
library(data.table)

dtX = data.table(city = sample(c("Cape Town", "New York",  "Tel Aviv"), size=15, replace = TRUE), 
                 score = sample(x=1:10, size = 15, replace=TRUE))
setkey(dtX, city)
dtX[dtX[, .I[sample(.N, 2)], city]$V1, score := 0] 
